{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.BSP.STM32.Driver.I2C
  ( i2cTower
  , module Ivory.Tower.HAL.Bus.I2C
  , module Ivory.Tower.HAL.Bus.I2C.DeviceAddr
  ) where

import Control.Monad (void)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.I2C.Regs
import Ivory.BSP.STM32.Peripheral.I2C.Peripheral

import Ivory.BSP.STM32.Driver.I2C.I2CDriverState

i2c_TIMEOUT :: ITime
i2c_TIMEOUT = toITime (Milliseconds 50)

-- | Max number of failed transactions in a row before we try to reset
-- the peripheral
i2c_MAX_ERROR_RUN :: Uint32
i2c_MAX_ERROR_RUN = 5

i2cTower :: (e -> ClockConfig)
         -> I2CPeriph
         -> I2CPins
         -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
                    , ChanOutput ('Stored ITime))
i2cTower tocc periph I2CPins{..} = do
  towerDepends i2cTowerTypes
  towerModule  i2cTowerTypes
  reqchan <- channel
  reschan <- channel
  readychan <- channel
  ready_per <- period (Milliseconds 1)
  watchdog_per <- period (Milliseconds 20)
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
    i2cPeripheralDriver tocc periph i2cpins_sda i2cpins_scl evt_irq err_irq
      (snd reqchan) (fst reschan) ready_per (fst readychan) watchdog_per
  return (BackpressureTransmit (fst reqchan) (snd reschan), snd readychan)

i2cTowerTypes :: Module
i2cTowerTypes = package "i2cTowerTypes" $ do
  defStruct (Proxy :: Proxy "i2c_transaction_request")
  defStruct (Proxy :: Proxy "i2c_transaction_result")


i2cPeripheralDriver :: forall e
                     . (e -> ClockConfig)
                    -> I2CPeriph
                    -> GPIOPin
                    -> GPIOPin
                    -> ChanOutput ('Stored ITime)
                    -> ChanOutput ('Stored ITime)
                    -> ChanOutput ('Struct "i2c_transaction_request")
                    -> ChanInput  ('Struct "i2c_transaction_result")
                    -> ChanOutput ('Stored ITime)
                    -> ChanInput  ('Stored ITime)
                    -> ChanOutput ('Stored ITime)
                    -> Monitor e ()
i2cPeripheralDriver tocc periph sda scl evt_irq err_irq req_chan res_chan ready_per ready_in watchdog_per = do
  clockConfig <- fmap tocc getEnv
  monitorModuleDef $ hw_moduledef

  let named nm = i2cName periph ++ "_" ++ nm

  driverstate <- stateInit (named "driverstate") (ival i2cInactive)

  handler systemInit (named "init") $ do
    callback $ const $ do
      debugSetup     debugPin1
      debugSetup     debugPin2
      debugSetup     debugPin3
      debugSetup     debugPin4
      i2cInit        periph sda scl clockConfig
      interrupt_enable (i2cIntEvent periph)
      interrupt_enable (i2cIntError periph)

  ready_sent <- state (named "ready_sent")
  handler ready_per (named "ready_period") $ do
    send_ready <- emitter ready_in 1
    callback $ \now -> do
      r <- deref ready_sent
      unless r $ emit send_ready now
      store ready_sent true

  (reqbuffer :: Ref 'Global ('Struct "i2c_transaction_request"))
    <- state (named "reqbuffer")
  (reqbufferpos :: Ref 'Global ('Stored (Ix 128)))
    <- state (named "reqbufferpos")

  (resbuffer :: Ref 'Global ('Struct "i2c_transaction_result"))
    <- state (named "resbuffer")
  (resbufferpos :: Ref 'Global ('Stored (Ix 128)))
    <- state (named "resbufferpos")

  (invalid_request :: Ref 'Global ('Stored Uint32))
    <- state (named "invalid_request")

  -- number of errors we've seen in a row
  (error_run :: Ref 'Global ('Stored Uint32))
    <- state (named "error_run")

  -- time of last event
  (last_event :: Ref 'Global ('Stored ITime))
    <- state (named "last_event")

  let sendresult' :: IBool
                  -> Emitter ('Struct "i2c_transaction_result")
                  -> Ref s' ('Struct "i2c_transaction_result")
                  -> Uint8
                  -> Ivory eff ()
      sendresult' unconditional e res code = do
          debugToggle debugPin4

          -- clean up status flags and shut down interrupts until the
          -- next request comes in
          clearSR1 periph
          disableCRInterrupts periph

          -- Don't send a result when inactive unless forced to
          s <- deref driverstate
          when (s /=? i2cInactive .|| unconditional) $ do
            store driverstate i2cInactive
            -- keep track of errors, resetting the run of errors
            -- whenever we have a successful result to send
            store (res ~> resultcode) code
            ifte_ (code >? 0)
              (do errs <- deref error_run
                  store error_run (errs + 1))
              (store error_run 0)
            emit e (constRef res)
      sendresult e res code = sendresult' false e res code

  handler watchdog_per (named "watchdog") $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      t_last <- deref last_event
      t <- getTime
      s <- deref driverstate
      errs <- deref error_run

      when (s /=? i2cInactive .&& t - t_last >? i2c_TIMEOUT) $ do
        -- handle a timeout like an error coming from the peripheral
        sendresult res_emitter resbuffer 1

      when (s ==? i2cInactive) $ do
        sr2 <- getReg (i2cRegSR2 periph)
        when (bitToBool (sr2 #. i2c_sr2_busy)) $ do
          -- if we're inactive but the bus is busy, reset the peripheral
          disableCRInterrupts periph
          i2cReset periph sda scl clockConfig
          -- increment the error run in case we need to try again
          store error_run (errs + 1)

      when (errs >=? i2c_MAX_ERROR_RUN) $ do
        when (s /=? i2cInactive) $ do
          -- shoot down any in-progress transaction
          sendresult res_emitter resbuffer 1
        -- reset the peripheral
        disableCRInterrupts periph
        i2cReset periph sda scl clockConfig
        -- reset the error run
        store error_run 0

  handler err_irq (named "error_irq") $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      -- Must read these registers, sometimes reading dr helps too??
      _sr1 <- getReg (i2cRegSR1 periph)
      _sr2 <- getReg (i2cRegSR2 periph)
      _dr  <- getReg (i2cRegDR periph)

      -- If there is any active transaction, terminate it send an
      -- error response
      setStop periph
      clearSR1 periph
      sendresult res_emitter resbuffer 1

      -- reenable the stm32 interrupt but keep the CR1 interrupt flags
      -- turned off
      interrupt_enable (i2cIntError periph)

  handler evt_irq (named "event_irq") $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      debugOn debugPin3
      store last_event =<< getTime
      s <- deref driverstate
      sr1 <- getReg (i2cRegSR1 periph)
      cond_
        [ (s ==? i2cEV5) ==> do
            comment "i2cEV5"
            -- I2C start bit sent and Master mode selected
--            sr2 <- getReg (i2cRegSR2 periph)
            cond_
              [(bitToBool (sr1 #. i2c_sr1_sb)) ==> do
                  comment "master mode, busy bus, start bit set"
                  tx_ad  <- deref (reqbuffer ~> tx_addr)
                  tx_pos <- deref reqbufferpos
                  tx_sz  <- deref (reqbuffer ~> tx_len)
                  let write_remaining = tx_sz - tx_pos
                  addr <- assign ((write_remaining >? 0) ?
                                    (writeAddr tx_ad, readAddr tx_ad))

                  -- clear address next
                  store driverstate i2cEV6

                  modifyReg (i2cRegDR periph) $
                    setField i2c_dr_data (fromRep addr)
              , (bitToBool (sr1 #. i2c_sr1_addr)) ==> do
                  comment "addr bit shouldn't be set unless start bit is"
                  sendresult res_emitter resbuffer 1
              ]

        , (s ==? i2cEV6) ==> do
            comment "i2cEV6"

            cond_
              [ (bitToBool (sr1 #. i2c_sr1_sb)) ==> do
                  comment "start bit should have been cleared when writing addr"
                  sendresult res_emitter resbuffer 1

              , (bitToBool (sr1 #. i2c_sr1_addr)) ==> do
                  comment "addr bit set, ready to start sending/receiving"
                  tx_pos <- deref reqbufferpos
                  tx_sz  <- deref (reqbuffer ~> tx_len)
                  rx_pos <- deref resbufferpos
                  rx_sz  <- deref (reqbuffer ~> rx_len)

                  let write_remaining = tx_sz - tx_pos
                      read_remaining  = rx_sz - rx_pos

                  cond_
                    [ (write_remaining >? 0) ==> do
                        comment "tx: clear addr"
                        void $ getReg (i2cRegSR2 periph)
                        -- take a byte off the write buffer, write to DR
                        w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
                        store reqbufferpos (tx_pos + 1)
                        modifyReg (i2cRegDR periph) $
                          setField i2c_dr_data (fromRep w)
                        ifte_ (write_remaining ==? 1)
                          -- stop transmission after this
                          (store driverstate i2cEV8_2)
                          -- otherwise keep transmitting
                          (store driverstate i2cEV8)
                    , (read_remaining >? 2) ==> do
                        comment "rx: send an ack until cleared"
                        modifyReg (i2cRegCR1 periph) $
                          setBit i2c_cr1_ack
                        ifte_ (read_remaining ==? 3)
                          -- special case read N-2 next
                          (store driverstate i2cEV7_N2)
                          -- otherwise normal reading next
                          (store driverstate i2cEV7)
                    , (read_remaining ==? 2) ==> do
                        comment "rx: special case read 2 bytes (pg841)"
                        -- set ACK low, set POS high
                        modifyReg (i2cRegCR1 periph) $
                          (clearBit i2c_cr1_ack >> setBit i2c_cr1_pos)
                        -- special case read N-1 next
                        store driverstate i2cEV7_N1
                    , (read_remaining ==? 1) ==> do
                        comment "rx: 1 byte read, set nack now (Figure 244)"
                        modifyReg (i2cRegCR1 periph) $
                          clearBit i2c_cr1_ack
                        -- read final byte next
                        store driverstate i2cEV7_rx1
                    , true ==> do
                        -- nothing to do
                        sendresult res_emitter resbuffer 0
                    ]
                  -- read sr2 to finish clearing address bit for reads
                  void $ getReg (i2cRegSR2 periph)
              ]

        , (s ==? i2cEV7) ==> do
            comment "i2cEV7"
            -- Receive mode: data register non-empty with either at
            -- least two more or no more bytes left to receive
            when (bitToBool (sr1 #. i2c_sr1_rxne)) $ do
              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)
              let read_remaining = rx_sz - rx_pos

              when (read_remaining ==? 4) $
                -- after this, 3 bytes left so start the
                -- end-of-receive special cases
                store driverstate i2cEV7_N2

              dr <- getReg (i2cRegDR periph)
              r  <- assign (toRep (dr #. i2c_dr_data))
              store ((resbuffer ~> rx_buf) ! rx_pos) r
              store resbufferpos (rx_pos + 1)

              when (read_remaining ==? 1) $ do
                 -- Now 0 remaining
                 sendresult res_emitter resbuffer 0

        , (s ==? i2cEV7_N2) ==> do
            comment "i2cEV7_N2"
            -- Receive mode: data register contains byte N-2, shift
            -- register contains byte N-1 (pg842)

            -- wait for rxne and btf (so both the data register and
            -- shift register are full)
            when ( bitToBool (sr1 #. i2c_sr1_rxne) .&&
                   bitToBool (sr1 #. i2c_sr1_btf)
                 ) $ do
              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)
              let read_remaining = rx_sz - rx_pos
              assert (read_remaining ==? 3)

              -- send NACK when reading the final byte
              modifyReg (i2cRegCR1 periph) $
                clearBit i2c_cr1_ack

              -- do N-1 case next
              store driverstate i2cEV7_N1

              dr <- getReg (i2cRegDR periph)
              r  <- assign (toRep (dr #. i2c_dr_data))
              store ((resbuffer ~> rx_buf) ! rx_pos) r
              store resbufferpos (rx_pos + 1)

        , (s ==? i2cEV7_N1) ==> do
            comment "i2cEV7_N1"
            -- Receive mode: data register contains byte N-1, shift
            -- register contains byte N (pg842)

            -- wait for rxne and btf (so both the data register and
            -- shift register are full)
            when ( bitToBool (sr1 #. i2c_sr1_rxne) .&&
                   bitToBool (sr1 #. i2c_sr1_btf)
                 ) $ do
              -- clear the special-case POS bit if it was set
              modifyReg (i2cRegCR1 periph) $ clearBit i2c_cr1_pos

              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)
              let read_remaining = rx_sz - rx_pos
              assert (read_remaining ==? 2)

              -- send STOP after reading data register
              setStop periph

              -- do normal read next, which covers the N case
              store driverstate i2cEV7

              dr <- getReg (i2cRegDR periph)
              r  <- assign (toRep (dr #. i2c_dr_data))
              store ((resbuffer ~> rx_buf) ! rx_pos) r
              store resbufferpos (rx_pos + 1)

        , (s ==? i2cEV7_rx1) ==> do
            comment "i2cEV7_rx1"
            -- Receive mode: data register non-empty with no bytes
            -- left to receive
            when (bitToBool (sr1 #. i2c_sr1_rxne)) $ do
              -- because we're only receiving one byte, we have to set
              -- the stop bit here and hope the peripheral doesn't end
              -- up requesting another one
              setStop periph

              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)
              let read_remaining = rx_sz - rx_pos

              assert (read_remaining ==? 1)

              dr <- getReg (i2cRegDR periph)
              r  <- assign (toRep (dr #. i2c_dr_data))
              store ((resbuffer ~> rx_buf) ! rx_pos) r
              store resbufferpos (rx_pos + 1)

              sendresult res_emitter resbuffer 0

        , (s ==? i2cEV8) ==> do
            comment "i2cEV8"
            -- Transmit mode: data register is empty and ready for
            -- another byte
            when (bitToBool (sr1 #. i2c_sr1_txe)) $ do
              tx_pos <- deref reqbufferpos
              tx_sz  <- deref (reqbuffer ~> tx_len)

              let write_remaining = tx_sz - tx_pos

              assert (write_remaining >? 0)

              when (write_remaining ==? 1) $
                -- if we're out of bytes to send, end the transmission
                store driverstate i2cEV8_2

              w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
              store reqbufferpos (tx_pos + 1)
              modifyReg (i2cRegDR periph) $
                setField i2c_dr_data (fromRep w)

        , (s ==? i2cEV8_2) ==> do
            comment "i2cEV8_2"
            -- Transmit mode: no bytes left to send; wait for shift
            -- register to empty
            when ( bitToBool (sr1 #. i2c_sr1_txe) .&&
                   bitToBool (sr1 #. i2c_sr1_btf)
                 ) $ do
              tx_pos <- deref reqbufferpos
              tx_sz  <- deref (reqbuffer ~> tx_len)
              rx_pos <- deref resbufferpos
              rx_sz  <- deref (reqbuffer ~> rx_len)

              let write_remaining = tx_sz - tx_pos
                  read_remaining =  rx_sz - rx_pos
              assert (write_remaining ==? 0)

              ifte_ (read_remaining >? 0)
                -- if there are any reads to do, send repeated start
                -- and take it from the top
                (store driverstate i2cEV5 >> setStart periph)
                -- otherwise we're done
                (do setStop periph
                    sendresult res_emitter resbuffer 0)

        , (s ==? i2cInactive) ==> do
            comment "i2cInactive"
            -- TODO: probably want some error handling in here,
            -- because this should be an unreachable state
        ]

      debugOff debugPin3

      ifte_ (s /=? i2cInactive)
        (enableCRInterrupts periph)
        (disableCRInterrupts periph)

      interrupt_enable (i2cIntEvent periph)

  handler req_chan (named "request") $ do
    res_emitter <- emitter res_chan 1
    callback $ \req -> do
      debugOn debugPin3
      ready <- deref ready_sent
      s <- deref driverstate
      cond_
        [ (ready .&& s ==? i2cInactive) ==> do
            cr1 <- getReg (i2cRegCR1 periph)
            ifte_ ( bitToBool (cr1 #. i2c_cr1_start) .||
                    bitToBool (cr1 #. i2c_cr1_stop)
                  )
              (do -- the start/stop bits should really not persist
                  -- long enough to be seen by multiple requests. If
                  -- they remain there, the hardware is probably stuck
                  -- somehow.
                  errs <- deref error_run
                  store error_run (errs + 1)
                  -- set this temporarily so we actually send a response back
                  sendresult' true res_emitter resbuffer 1
              )
              (do -- Setup state by clearing all CR1 flags except enable
                  setReg (i2cRegCR1 periph) $ setBit i2c_cr1_pe
                  store driverstate i2cEV5
                  refCopy reqbuffer req
                  store reqbufferpos 0
                  store resbufferpos 0
                  store last_event =<< getTime
                  enableCRInterrupts periph
                  setStart periph)
        , true ==> do
            invalid_request %= (+1)
            sendresult' true res_emitter resbuffer 1
        ]
      debugOff debugPin3

_busyMaster :: I2C_SR2 -> IBool
_busyMaster sr2 =
  bitToBool (sr2 #. i2c_sr2_busy) .&&
  bitToBool (sr2 #. i2c_sr2_msl)

setStop :: I2CPeriph -> Ivory (AllocEffects s) ()
setStop periph = do
  -- Generate an I2C Stop condition. Per the reference manual, we must
  -- wait for the hardware to clear the stop bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_stop
  times (15 :: Ix 16) $ \_ -> do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_stop)) $
      breakOut

setStart :: I2CPeriph -> Ivory (AllocEffects s) ()
setStart periph = do
  -- Generate an I2C Start condition. Per the reference manual, we must
  -- wait for the hardware to clear the start bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_start
  times (15 :: Ix 16) $ \_ -> do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_start)) $
      breakOut

clearSR1 :: I2CPeriph -> Ivory eff ()
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

enableCRInterrupts :: I2CPeriph -> Ivory eff ()
enableCRInterrupts periph = do
  modifyReg (i2cRegCR2 periph)
    (mapM_ setBit [i2c_cr2_itbufen, i2c_cr2_itevten, i2c_cr2_iterren])

disableCRInterrupts :: I2CPeriph -> Ivory eff ()
disableCRInterrupts periph = do
  modifyReg (i2cRegCR2 periph)
    (mapM_ clearBit [i2c_cr2_itbufen, i2c_cr2_itevten, i2c_cr2_iterren])

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
