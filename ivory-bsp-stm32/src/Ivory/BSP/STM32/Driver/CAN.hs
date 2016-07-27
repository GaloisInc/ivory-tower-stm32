{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.BSP.STM32.Driver.CAN
  ( canTower
  ) where

import Control.Monad (forM, forM_)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4

canTower :: (e -> ClockConfig)
         -> CANPeriph
         -> Integer
         -> GPIOPin
         -> GPIOPin
         -> Tower e ( ChanOutput ('Struct "can_message")
                    , AbortableTransmit ('Struct "can_message") ('Stored IBool)
                    , AbortableTransmit ('Struct "can_message") ('Stored IBool)
                    , AbortableTransmit ('Struct "can_message") ('Stored IBool)
                    )
canTower tocc periph bitrate rxpin txpin = do
  towerDepends canDriverTypes
  towerModule  canDriverTypes
  reschan <- channel

  let minMessageLength = 44 -- bits in a frame with no data and no stuffing
  let minTimePerFrame = Microseconds $ 1000000 * minMessageLength `div` bitrate

  tx_irq <- signalUnsafe
                (Interrupt $ canIntTX periph)
                minTimePerFrame
                (interrupt_disable $ canIntTX periph)

  ([api0, api1, api2], transmitters) <- fmap unzip $ forM (canRegTX periph) $ \ txmailbox -> do
    (abortableTransmit, reqChan) <- channel
    (resChan, abortableComplete) <- channel
    (abortableAbort, abortChan) <- channel

    return $ (,) (AbortableTransmit { .. }) $ do
      handler reqChan "request" $ do
        callback $ \ req -> do
          tsr <- getReg (canRegTSR periph)
          comment "mailbox must be empty"
          assert $ bitToBool $ tsr #. canTXEmpty txmailbox
          comment "any completed request must have already been reported"
          assert $ iNot $ bitToBool $ tsr #. canTXRQCP txmailbox

          arb <- deref $ req ~> can_message_id
          len <- deref $ req ~> can_message_len

          let get_bytes :: (BitData reg, SafeCast Uint8 (BitDataRep reg)) => [(Ix 8, BitDataField reg (Bits 8))] -> Ivory eff [BitDataM reg ()]
              get_bytes = mapM $ \ (idx, field) -> do
                v <- deref $ (req ~> can_message_buf) ! idx
                return $ setField field $ fromRep v

          low_bytes <- get_bytes [(0, can_tdlr_data0), (1, can_tdlr_data1), (2, can_tdlr_data2), (3, can_tdlr_data3)]
          hi_bytes <- get_bytes [(4, can_tdhr_data4), (5, can_tdhr_data5), (6, can_tdhr_data6), (7, can_tdhr_data7)]

          modifyReg (canRegTDTR txmailbox) $ do
            clearBit can_tdtr_tgt
            setField can_tdtr_dlc $ fromRep $ castDefault $ fromIx len
          setReg (canRegTDLR txmailbox) $ sequence_ low_bytes
          setReg (canRegTDHR txmailbox) $ sequence_ hi_bytes
          setReg (canRegTIR txmailbox) $ do
            setField can_tir_id $ arb #. can_arbitration_id
            setField can_tir_ide $ arb #. can_arbitration_ide
            setField can_tir_rtr $ arb #. can_arbitration_rtr
            setBit can_tir_txrq

      handler abortChan "abort" $ do
        callback $ const $ do
          setReg (canRegTSR periph) $ setBit $ canTXAbort txmailbox

      return (resChan, txmailbox)

  receivers <- forM (zip [0 :: Int ..] $ canRegRX periph) $ \ (idx, fifo) -> do
    rx_irq <- signalUnsafe
                  (Interrupt $ canIntRX fifo)
                  minTimePerFrame
                  (interrupt_disable $ canIntRX fifo)
    return $ handler rx_irq ("rx" ++ show idx ++ "_irq") $ do
      resultEmitter <- emitter (fst reschan) 3
      callback $ const $ do
        arrayMap $ \ (_ :: Ix 3) -> do
          rfr <- getReg $ canRegRFR fifo
          -- If the FIFO is empty, we have nothing to do.
          when (rfr #. can_rfr_fmp ==? fromRep 0) breakOut

          -- If the FIFO is still reloading the mailbox, we can't pull the
          -- next message out. All we can do is loop and try again.
          unless (bitToBool $ rfr #. can_rfr_rfom) $ do
            rir <- getReg $ canRegRIR fifo
            rdtr <- getReg $ canRegRDTR fifo
            rdlr <- getReg $ canRegRDLR fifo
            rdhr <- getReg $ canRegRDHR fifo

            -- Release this FIFO entry ASAP to overlap the mailbox reload
            -- with emitting the result.
            setReg (canRegRFR fifo) $ do
              setBit can_rfr_rfom
              setBit can_rfr_fovr
              setBit can_rfr_full

            let arb = fromRep $ withBits 0 $ do
                  setField can_arbitration_id $ rir #. can_rir_id
                  setField can_arbitration_ide $ rir #. can_rir_ide
                  setField can_arbitration_rtr $ rir #. can_rir_rtr

            msg <- local $ istruct
              [ can_message_id .= ival arb
              , can_message_buf .= iarray (map ival $ map toRep $
                  map (rdlr #.) [can_rdlr_data0, can_rdlr_data1, can_rdlr_data2, can_rdlr_data3] ++
                  map (rdhr #.) [can_rdhr_data4, can_rdhr_data5, can_rdhr_data6, can_rdhr_data7])
              , can_message_len .= ival (toIx $ toRep $ rdtr #. can_rdtr_dlc)
              ]
            emit resultEmitter $ constRef msg

        interrupt_enable $ canIntRX fifo

  monitor (canName periph ++ "PeripheralDriver") $ do
    clockconfig <- fmap tocc getEnv

    handler systemInit "init" $ do
      callback $ const $ do
        canInit periph bitrate rxpin txpin clockconfig
        modifyReg (canRegIER periph) $ do
          setBit can_ier_tmeie
          setBit can_ier_fmpie0
          setBit can_ier_fmpie1
        interrupt_enable $ canIntTX periph
        forM_ (canRegRX periph) $ \ fifo -> do
          interrupt_enable $ canIntRX fifo

    tx_emitters <- sequence transmitters
    handler tx_irq "tx_irq" $ do
      tx_callbacks <- forM tx_emitters $ \ (resChan, txmailbox) -> do
        res <- emitter resChan 1
        return (res, txmailbox)
      callback $ const $ do
        tsr <- getReg $ canRegTSR periph
        forM_ tx_callbacks $ \ (res, txmailbox) -> do
          when (bitToBool $ tsr #. canTXRQCP txmailbox) $ do
            setReg (canRegTSR periph) $ setBit $ canTXRQCP txmailbox
            emitV res $ bitToBool $ tsr #. canTXOK txmailbox
        interrupt_enable $ canIntTX periph

    sequence_ receivers

    monitorModuleDef $ do
      hw_moduledef

  return (snd reschan, api0, api1, api2)
