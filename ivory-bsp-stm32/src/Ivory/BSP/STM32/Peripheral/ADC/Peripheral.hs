{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
--
-- Peripheral.hs --- ADC peripheral driver for the STM32F4.
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.ADC.Peripheral where

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.ADC.Regs
import Ivory.BSP.STM32.Peripheral.ADC.RegTypes
import Ivory.HW
import Ivory.Language

data ADCPeriph = ADCPeriph
  { adcRegSR :: BitDataReg ADC_SR
  , adcRegCR1 :: BitDataReg ADC_CR1
  , adcRegCR2 :: BitDataReg ADC_CR2
  , adcRegSQR1 :: BitDataReg ADC_SQR1
  , adcRegSQR2 :: BitDataReg ADC_SQR2
  , adcRegSQR3 :: BitDataReg ADC_SQR3
  , adcRegDR :: BitDataReg ADC_DR
  , adcRCCEnable :: forall eff . Ivory eff ()
  , adcRCCDisable :: forall eff . Ivory eff ()
  , adcInt :: HasSTM32Interrupt
  , adcName :: String
  }

mkADCPeriph :: (STM32Interrupt i)
            => Integer -- ^ Base
            -> (forall eff . Ivory eff ()) -- ^ RCC Enable
            -> (forall eff . Ivory eff ()) -- ^ RCC Disable
            -> i -- ^ global adc interrupt. NB: shared with other adc periphs!
            -> String -- ^ Name
            -> ADCPeriph
mkADCPeriph base rccen rccdis int n =
  ADCPeriph
    { adcRegSR      = reg 0x00 "sr"
    , adcRegCR1     = reg 0x04 "cr1"
    , adcRegCR2     = reg 0x08 "cr2"
    -- TODO: remaining registers
    , adcRegSQR1    = reg 0x2C "sqr1"
    , adcRegSQR2    = reg 0x30 "sqr2"
    , adcRegSQR3    = reg 0x34 "sqr3"
    -- TODO: remaining registers
    , adcRegDR      = reg 0x4C "dr"
    , adcRCCEnable  = rccen
    , adcRCCDisable = rccdis
    , adcInt        = HasSTM32Interrupt int
    , adcName       = n
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

adcInit :: ADCPeriph
        -> ADCResolution -- ^ how many bits of precision to use in conversion?
        -> IBool -- ^ left-align converted bits in 16-bit data register?
        -> Ivory eff ()
adcInit periph res align = do
  adcRCCEnable periph
  modifyReg (adcRegCR1 periph) $ do
    setField adc_cr1_res res
  modifyReg (adcRegCR2 periph) $ do
    setField adc_cr2_align $ boolToBit align
    setBit adc_cr2_adon

adcStartConversion :: ADCPeriph -> Int -> Ivory eff ()
adcStartConversion periph chan = do
  setReg (adcRegSQR3 periph) $ do
    setField adc_sqr3_sq1 $ fromRep $ fromIntegral chan
  setReg (adcRegSQR1 periph) $ do
    setField adc_sqr1_l $ fromRep 1
  modifyReg (adcRegCR2 periph) $ do
    setBit adc_cr2_swstart
    clearBit adc_cr2_eocs
    clearBit adc_cr2_dma
    clearBit adc_cr2_cont

adcGetConversion :: ADCPeriph -> Ivory eff Uint16
adcGetConversion periph = do
  dr <- getReg (adcRegDR periph)
  return (toRep (dr #. adc_dr_data))

