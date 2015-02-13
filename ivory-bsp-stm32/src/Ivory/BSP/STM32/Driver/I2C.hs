{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.BSP.STM32.Driver.I2C
  ( i2cTower
  , module Ivory.BSP.STM32.Driver.I2C.Types
  , module Ivory.BSP.STM32.Driver.I2C.I2CDeviceAddr
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.I2C.Regs
import Ivory.BSP.STM32.Peripheral.I2C.Peripheral

import Ivory.BSP.STM32.Driver.I2C.Types
import Ivory.BSP.STM32.Driver.I2C.I2CDeviceAddr
import Ivory.BSP.STM32.Driver.I2C.I2CDriverState


i2cTower :: (STM32Interrupt s)
         => (e -> ClockConfig)
         -> I2CPeriph s
         -> GPIOPin
         -> GPIOPin
         -> Tower e ( ChanInput  (Struct "i2c_transaction_request")
                    , ChanOutput (Struct "i2c_transaction_result")
                    , ChanOutput (Stored ITime))
i2cTower tocc periph sda scl = do
  towerDepends i2cTowerTypes
  towerModule  i2cTowerTypes
  reqchan <- channel
  reschan <- channel
  readychan <- channel
  ready_per <- period (Milliseconds 1)
  evt_irq <- signalUnsafe
                (Interrupt (i2cIntEvent periph))
                (Microseconds 50)
                (do debugToggle debugPin1
                    modifyReg (i2cRegCR2 periph)
                      (clearBit i2c_cr2_itbufen >> clearBit i2c_cr2_itevten)
                    interrupt_disable (i2cIntEvent periph))

  err_irq <- signalUnsafe
                (Interrupt (i2cIntError periph))
                (Microseconds 50)
                (do debugToggle debugPin2
                    modifyReg (i2cRegCR2 periph)
                      (clearBit i2c_cr2_iterren)
                    interrupt_disable (i2cIntError periph))
  monitor ((i2cName periph) ++ "PeripheralDriver") $
    i2cPeripheralDriver tocc periph sda scl evt_irq err_irq
      (snd reqchan) (fst reschan) ready_per (fst readychan)
  return (fst reqchan, snd reschan, snd readychan)

i2cTowerTypes :: Module
i2cTowerTypes = package "i2cTowerTypes" $ do
  defStruct (Proxy :: Proxy "i2c_transaction_request")
  defStruct (Proxy :: Proxy "i2c_transaction_result")


i2cPeripheralDriver :: forall s e
                     . (STM32Interrupt s)
                    => (e -> ClockConfig)
                    -> I2CPeriph s
                    -> GPIOPin
                    -> GPIOPin
                    -> ChanOutput (Stored ITime)
                    -> ChanOutput (Stored ITime)
                    -> ChanOutput (Struct "i2c_transaction_request")
                    -> ChanInput  (Struct "i2c_transaction_result")
                    -> ChanOutput (Stored ITime)
                    -> ChanInput  (Stored ITime)
                    -> Monitor e ()
i2cPeripheralDriver tocc periph sda scl evt_irq err_irq req_chan res_chan ready_per ready_in = do
  clockConfig <- fmap tocc getEnv
  monitorModuleDef $ hw_moduledef

  driverstate <- stateInit "driverstate" (ival stateInactive)

  handler systemInit "init" $ do
    callback $ const $ do
      debugSetup     debugPin1
      debugSetup     debugPin2
      debugSetup     debugPin3
      debugSetup     debugPin4
      i2cInit        periph sda scl clockConfig
      -- Setup hardware for interrupts
      interrupt_enable (i2cIntEvent periph)
      interrupt_enable (i2cIntError periph)

  ready_sent <- state "ready_sent"
  handler ready_per "readyPeriod" $ do
    send_ready <- emitter ready_in 1
    callback $ \now -> do
      r <- deref ready_sent
      unless r $ emit send_ready now
      store ready_sent true

  (reqbuffer :: Ref Global (Struct "i2c_transaction_request")) <- state "reqbuffer"
  (reqbufferpos :: Ref Global (Stored (Ix 128)))               <- state "reqbufferpos"

  (resbuffer :: Ref Global (Struct "i2c_transaction_result"))  <- state "resbuffer"
  (resbufferpos :: Ref Global (Stored (Ix 128)))               <- state "resbufferpos"

  (invalid_request :: Ref Global (Stored Uint32)) <- state "invalid_request"

  (transaction_delayed :: Ref Global (Stored Uint32)) <- state "transaction_delayed"

  let sendresult :: Emitter (Struct "i2c_transaction_result")
                 -> Ref s' (Struct "i2c_transaction_result")
                 -> Uint8
                 -> Ivory eff ()
      sendresult e res code = do
          debugToggle debugPin4
          store (res ~> resultcode) code
          emit e (constRef res)

  handler err_irq "error_irq" $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      -- Must read these registers, sometimes reading dr helps too??
      sr1  <- getReg (i2cRegSR1 periph)
      _sr2 <- getReg (i2cRegSR2 periph)
      _dr  <- getReg (i2cRegDR periph)

      -- If bus error (BERR), acknowledge failure (AF), send Stop
      let must_release = bitToBool (sr1 #. i2c_sr1_berr)
                     .|| bitToBool (sr1 #. i2c_sr1_af)
      when must_release $ setStop periph

      clearSR1 periph

      -- If there is an active transaction, terminate it
      s <- deref driverstate
      cond_
        [ (s ==? stateActive) ==> do
            store driverstate stateInactive
            sendresult res_emitter resbuffer 1
        , (s ==? stateError) ==> do
            store driverstate stateInactive
        ]

      -- Re-enable interrupt
      modifyReg (i2cRegCR2 periph)
        (setBit i2c_cr2_iterren)
      interrupt_enable (i2cIntError periph)

  handler evt_irq "event_irq" $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      debugOn debugPin3
      s <- deref driverstate
      cond_
        [ (s ==? stateActive) ==> do
            -- Hardware requires us to read both status registers.
            -- We don't actually need the contents of SR2.
            sr1  <- getReg (i2cRegSR1 periph)
            _sr2 <- getReg (i2cRegSR2 periph)

            when (bitToBool (sr1 #. i2c_sr1_btf)) $ do
              comment "transaction delayed because byte transfer finished before we woke up again"
              transaction_delayed %= (+1)

            when (bitToBool (sr1 #. i2c_sr1_sb)) $ do
              tx_sz  <- deref (reqbuffer ~> tx_len)
              tx_pos <- deref reqbufferpos
              tx_ad <- deref (reqbuffer ~> tx_addr)
              let write_remaining = tx_sz - tx_pos
              -- Start bit sent. Send addr field:
              addr <- assign ((write_remaining >? 0) ?
                                (writeAddr tx_ad, readAddr tx_ad))
              modifyReg (i2cRegDR periph) $
                setField i2c_dr_data (fromRep addr)

            when (bitToBool (sr1 #. i2c_sr1_addr)) $ do
              tx_pos <- deref reqbufferpos
              tx_sz  <- deref (reqbuffer ~> tx_len)
              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)

              let write_remaining = tx_sz - tx_pos
                  read_remaining =  rx_sz - rx_pos

              cond_
                [ (write_remaining >? 0) ==> do
                    -- Writing: take a byte off the write buffer, write to DR
                    w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
                    store reqbufferpos (tx_pos + 1)
                    modifyReg (i2cRegDR periph) $
                      setField i2c_dr_data (fromRep w)
                , (read_remaining >? 1) ==> do
                    -- Send an ack on the next byte
                    modifyReg (i2cRegCR1 periph) $
                      setBit i2c_cr1_ack
                , true ==> do
                    -- One byte left to read, send a stop afterwards
                    setStop periph
                ]
            when (bitToBool (sr1 #. i2c_sr1_rxne)) $ do
              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)

              let read_remaining =  rx_sz - rx_pos
              -- Read into the read buffer
              dr <- getReg (i2cRegDR periph)
              r  <- assign (toRep (dr #. i2c_dr_data))
              store ((resbuffer ~> rx_buf) ! rx_pos) r
              store resbufferpos (rx_pos + 1)

              when (read_remaining ==? 2) $ do
                 -- Now 1 remaining
                 -- Unset Ack, then Stop
                 modifyReg (i2cRegCR1 periph) $
                   clearBit i2c_cr1_ack
                 setStop periph

              when (read_remaining ==? 1) $ do
                 -- Now 0 remaining
                 store driverstate stateInactive
                 sendresult res_emitter resbuffer 0

            when (bitToBool (sr1 #. i2c_sr1_txe)) $ do

              tx_pos <- deref reqbufferpos
              tx_sz  <- deref (reqbuffer ~> tx_len)
              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)

              let write_remaining = tx_sz - tx_pos
                  read_remaining =  rx_sz - rx_pos
              cond_
                -- TXE set: tx buffer is empty, still writing
                [ (write_remaining >? 0) ==> do
                    w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
                    store reqbufferpos (tx_pos + 1)
                    modifyReg (i2cRegDR periph) $
                      setField i2c_dr_data (fromRep w)
                , (read_remaining >? 0) ==> do
                    -- Done writing, ready to read: send repeated start
                    setStart periph
                , true ==> do
                    setStop periph
                    -- Done, send response
                    store driverstate stateInactive
                    sendresult res_emitter resbuffer 0
                ]
        , (s ==? stateError) ==> do
            modifyReg (i2cRegCR1 periph) $ clearBit i2c_cr1_pe
            debugToggle debugPin2 -- XXX PLACEHOLDER
            store driverstate stateInactive
        ]

      debugOff debugPin3
      modifyReg (i2cRegCR2 periph)
        (setBit i2c_cr2_itbufen >> setBit i2c_cr2_itevten)
      interrupt_enable (i2cIntEvent periph)

  handler req_chan "request" $ do
    res_emitter <- emitter res_chan 1
    callback $ \req -> do
      debugOn debugPin3
      s <- deref driverstate
      cond_
        [ (s ==? stateInactive) ==> do
            -- Setup state
            modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_pe
            store driverstate stateActive
            refCopy reqbuffer req
            store reqbufferpos 0
            store resbufferpos 0
            setStart periph
        , true ==> do
            invalid_request %= (+1)
            -- XXX how do we want to handle this error?
            sendresult res_emitter resbuffer 1
        ]
      debugOff debugPin3


setStop :: I2CPeriph p -> Ivory (AllocEffects s) ()
setStop periph = do
  -- Generate an I2C Stop condition. Per the reference manual, we must
  -- wait for the hardware to clear the stop bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_stop
  times (15 :: Ix 16) $ \_ -> do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_stop)) $
      breakOut

setStart :: I2CPeriph p -> Ivory (AllocEffects s) ()
setStart periph = do
  -- Generate an I2C Start condition. Per the reference manual, we must
  -- wait for the hardware to clear the start bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_start
  times (15 :: Ix 16) $ \_ -> do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_start)) $
      breakOut

clearSR1 :: I2CPeriph p -> Ivory eff ()
clearSR1 periph = modifyReg (i2cRegSR1 periph) $ do
  clearBit i2c_sr1_smbalert
  clearBit i2c_sr1_timeout
  clearBit i2c_sr1_pecerr
  clearBit i2c_sr1_ovr
  clearBit i2c_sr1_af
  clearBit i2c_sr1_arlo
  clearBit i2c_sr1_berr
  clearBit i2c_sr1_txe
  clearBit i2c_sr1_rxne
  clearBit i2c_sr1_stopf
  clearBit i2c_sr1_add10
  clearBit i2c_sr1_btf
  clearBit i2c_sr1_addr
  clearBit i2c_sr1_sb


-- Debugging Helpers: useful for development, disabled for production.
debugPin1, debugPin2, debugPin3, debugPin4 :: Maybe GPIOPin
-- Debug 1: toggles on event interrupt
debugPin1 = Nothing -- Just pinB8 -- i2c1 sda
-- Debug 2: toggles on error interrupt
debugPin2 = Nothing -- Just pinB9 -- i2c1 scl
-- Debug 3: active when starting driver with request
debugPin3 = Nothing -- Just pinC12 -- uart5 tx
-- Debug 4: toggles when terminating driver with response
debugPin4 = Nothing -- Just pinD2  -- uart5 rx

debugSetup :: Maybe GPIOPin -> Ivory eff ()
debugSetup (Just p) = do
  pinEnable        p
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_50mhz
  pinSetPUPD       p gpio_pupd_none
  pinClear         p
  pinSetMode       p gpio_mode_output
debugSetup Nothing = return ()

debugOff :: Maybe GPIOPin -> Ivory eff ()
debugOff (Just p) = pinClear p
debugOff Nothing  = return ()

debugOn :: Maybe GPIOPin -> Ivory eff ()
debugOn (Just p) = pinSet p
debugOn Nothing  = return ()

debugToggle :: Maybe GPIOPin -> Ivory eff ()
debugToggle p = do
  -- turn it on a couple of times just to take a bit longer. sometimes my
  -- logic analyzer can't catch a single on/off blip.
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOff p
  debugOff p
  debugOff p
  debugOff p
  debugOff p
  debugOff p
  debugOff p
