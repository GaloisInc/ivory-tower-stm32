{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Driver.SPI
  ( spiTower
  , module Ivory.Tower.HAL.Bus.SPI
  , module Ivory.Tower.HAL.Bus.SPI.DeviceHandle
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.SPI.Regs
import Ivory.BSP.STM32.Peripheral.SPI.Peripheral

spiTower :: forall e
          . (e -> ClockConfig)
         -> [SPIDevice]
         -> SPIPins
         -> Tower e ( BackpressureTransmit ('Struct "spi_transaction_request")
                                           ('Struct "spi_transaction_result")
                    , ChanOutput ('Stored ITime))
spiTower tocc devices pins = do
  towerDepends spiDriverTypes
  towerModule  spiDriverTypes
  reqchan <- channel
  reschan <- channel
  readychan <- channel
  irq <- signalUnsafe (Interrupt interrupt)
                (Microseconds 9)
                (do debugToggle debugPin1
                    interrupt_disable interrupt)
  watchdog_per <- period (Milliseconds 1)
  monitor (periphname ++ "PeripheralDriver") $ do
    spiPeripheralDriver tocc periph pins devices (snd reqchan) (fst reschan) (fst readychan) irq watchdog_per
  return (BackpressureTransmit (fst reqchan) (snd reschan), snd readychan)
  where
  interrupt = spiInterrupt periph
  periphname = spiName periph
  periph = case devices of
    [] -> err "for an empty device set"
    d:ds ->
      let canonicalp = spiDevPeripheral d
      in case and (map (\d' -> canonicalp `eqname` spiDevPeripheral d') ds) of
        False -> err "with devices on different peripherals"
        True -> case and (map (\d' -> spiDevClockHz d' <= 500000) devices) of
          False -> err "with any device clock speed over 500khz" -- XXX if we go higher, we drop interrupts
          True -> canonicalp
  eqname a b = spiName a == spiName b
  err m = error ("spiTower cannot be created " ++ m)


spiPeripheralDriver :: forall e
                     . (e -> ClockConfig)
                    -> SPIPeriph
                    -> SPIPins
                    -> [SPIDevice]
                    -> ChanOutput   ('Struct "spi_transaction_request")
                    -> ChanInput    ('Struct "spi_transaction_result")
                    -> ChanInput    ('Stored ITime)
                    -> ChanOutput ('Stored ITime)
                    -> ChanOutput ('Stored ITime)
                    -> Monitor e ()
spiPeripheralDriver tocc periph pins devices req_out res_in ready_in irq watchdog_per = do
  clockconfig <- fmap tocc getEnv
  monitorModuleDef $ hw_moduledef
  done <- state "done"
  shutdown <- stateInit "shutdown" (ival false)
  handler systemInit "initialize_hardware" $ do
    send_ready <- emitter ready_in 1
    callback $ \ now -> do
      debugSetup     debugPin1
      debugSetup     debugPin2
      debugSetup     debugPin3
      debugSetup     debugPin4
      spiInit        periph pins
      mapM_ spiDeviceInit devices
      store done true
      emit send_ready now
      interrupt_enable interrupt

  reqbuffer    <- state "reqbuffer"
  reqbufferpos <- state "reqbufferpos"

  resbuffer    <- state "resbuffer"
  resbufferpos <- state "resbufferpos"

  handler watchdog_per "spi_shutdown_watchdog" $ do
    e <- emitter res_in 1
    callback $ \_ -> do
      do_shutdown <- deref shutdown
      when do_shutdown $ do
        sr <- getReg (spiRegSR periph)
        unless (bitToBool (sr #. spi_sr_bsy)) $ do
          spiBusEnd periph
          chooseDevice spiDeviceDeselect (reqbuffer ~> tx_device)
          emit e (constRef resbuffer)
          store done true
          store shutdown false

  handler irq "irq" $ do
    callback $ \_ -> do
      tx_pos <- deref reqbufferpos
      tx_sz  <- deref (reqbuffer ~> tx_len)
      rx_pos <- deref resbufferpos
      rx_sz  <- deref (resbuffer ~> rx_idx)

      sr <- getReg (spiRegSR periph)

      cond_
        [ bitToBool (sr #. spi_sr_rxne) ==> do
            debugOn debugPin2
            when (rx_pos <? rx_sz) $ do
              r <- spiGetDR periph
              store ((resbuffer ~> rx_buf) ! rx_pos) r
              store resbufferpos (rx_pos + 1)
            when (tx_pos <? tx_sz) $ do
              comment "still more to send"
              modifyReg (spiRegCR2 periph)
                (setBit spi_cr2_txeie >> clearBit spi_cr2_rxneie)
            when (rx_pos ==? (rx_sz - 1)) $ do
              comment "done receiving"
              modifyReg (spiRegCR2 periph)
                (setBit spi_cr2_txeie >> clearBit spi_cr2_rxneie)
            debugOff debugPin2

        , bitToBool (sr #. spi_sr_txe) ==> do
            debugOn debugPin3
            when (tx_pos <? tx_sz) $ do
              w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
              spiSetDR periph w
              store reqbufferpos (tx_pos + 1)
              when (rx_pos <? rx_sz) $ do
                comment "still more to receive"
                modifyReg (spiRegCR2 periph)
                  (clearBit spi_cr2_txeie >> setBit spi_cr2_rxneie)
            when (tx_pos >=? tx_sz .&& rx_pos >=? (rx_sz - 1) .&&
                  iNot (bitToBool (sr #. spi_sr_bsy))) $ do
              comment "transaction done; disable interrupts and let watchdog"
              comment "shut down the device once the bus isn't busy"
              modifyReg (spiRegCR2 periph) (clearBit spi_cr2_txeie)
              store shutdown true

            debugOff debugPin3
        ]
      interrupt_enable interrupt

  let deviceBeginProc :: SPIDevice -> Def('[]':->())
      deviceBeginProc dev = proc ((spiDevName dev) ++ "_devicebegin") $
        body $ do
          spiBusBegin clockconfig dev
          spiDeviceSelect dev

  monitorModuleDef $ do
    mapM_ (incl . deviceBeginProc) devices

  handler req_out  "request" $ do
    callback $ \req -> do
      debugOn debugPin4
      ready <- deref done
      when ready $ do
        store done false
        -- Initialize request and result state
        refCopy reqbuffer req
        reqlen <- deref (reqbuffer ~> tx_len)
        store reqbufferpos 0
        store resbufferpos 0
        store (resbuffer ~> rx_idx) reqlen
        -- Get the first byte to transmit
        tx0 <- deref ((reqbuffer ~> tx_buf) ! 0)
        store reqbufferpos 1
        -- select the device and setup the spi peripheral
        chooseDevice (call_ . deviceBeginProc) (reqbuffer ~> tx_device)
        -- Send the first byte, enable tx empty interrupt
        spiSetDR  periph tx0
        modifyReg (spiRegCR2 periph) (setBit spi_cr2_txeie)

      unless ready $ do
        return () -- XXX how do we want to handle this error?
      debugOff debugPin4

  where
  interrupt = spiInterrupt periph

  chooseDevice :: (SPIDevice -> Ivory eff ())
               -> Ref 'Global ('Stored SPIDeviceHandle) -> Ivory eff ()
  chooseDevice cb devref = do
    comment "selecting device:"
    currdev <- deref devref
    assert (currdev <? invalidhandle)
    cond_ (zipWith (aux currdev) devices [(0::Integer)..])
    comment "end selecting configured device"
    where
    invalidhandle = SPIDeviceHandle (fromIntegral (length devices))
    aux cd device idx =
      cd ==? SPIDeviceHandle (fromIntegral idx) ==> cb device

-- Debugging Helpers: useful for development, disabled for production.
debugPin1, debugPin2, debugPin3, debugPin4 :: Maybe GPIOPin
debugPin1 = Nothing
debugPin2 = Nothing
debugPin3 = Nothing
debugPin4 = Nothing
{-
debugPin1 = Just pinB8
debugPin2 = Just pinB9
debugPin3 = Just pinA6
debugPin4 = Just pinA5
-}

debugSetup :: Maybe GPIOPin -> Ivory eff ()
debugSetup (Just p) = do
  pinEnable        p
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_50mhz
  pinSetPUPD       p gpio_pupd_none
  pinClear         p
  pinSetMode       p gpio_mode_output
  debugToggle (Just p)
debugSetup Nothing = return ()

debugOff :: Maybe GPIOPin -> Ivory eff ()
debugOff (Just p) = pinClear p
debugOff Nothing  = return ()

debugOn :: Maybe GPIOPin -> Ivory eff ()
debugOn (Just p) = pinSet p
debugOn Nothing  = return ()

debugToggle :: Maybe GPIOPin -> Ivory eff ()
debugToggle p = do
  comment "debugToggle: make sure pin stays hi/lo long enough to see on my crummy logic analyzer"
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

