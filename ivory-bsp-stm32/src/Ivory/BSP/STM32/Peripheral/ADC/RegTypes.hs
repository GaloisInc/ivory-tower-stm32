{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- ADC register types
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.ADC.RegTypes where

import Ivory.Language

[ivory|
  bitdata ADCResolution :: Bits 2
    = adc_12bit as 0
    | adc_10bit as 1
    | adc_8bit  as 2
    | adc_6bit  as 3

  bitdata ADCPrescaler :: Bits 2
    = adc_pclk2_div2 as 0
    | adc_pclk2_div4 as 1
    | adc_pclk2_div6 as 2
    | adc_pclk2_div8 as 3

  bitdata ADCExtEn :: Bits 2
    = adc_exten_none    as 0
    | adc_exten_rising  as 1
    | adc_exten_falling as 2
    | adc_exten_both    as 3

  bitdata ADCExtSel :: Bits 4
    = adc_ext_t1cc1  as 0
    | adc_ext_t1cc2  as 1
    | adc_ext_t1cc3  as 2
    | adc_ext_t2cc2  as 3
    | adc_ext_t2cc3  as 4
    | adc_ext_t2cc4  as 5
    | adc_ext_t2trgo as 6
    | adc_ext_t3cc1  as 7
    | adc_ext_t3trgo as 8
    | adc_ext_t4cc4  as 9
    | adc_ext_t5cc1  as 10
    | adc_ext_t5cc2  as 11
    | adc_ext_t5cc3  as 12
    | adc_ext_t8cc1  as 13
    | adc_ext_t8trgo as 14
    | adc_ext_exti   as 15 -- external pin line 11

  bitdata ADCJExtSel :: Bits 4
    = adc_jext_t1cc4  as 0
    | adc_jext_t1trgo as 1
    | adc_jext_t2cc1  as 2
    | adc_jext_t2trgo as 3
    | adc_jext_t3cc2  as 4
    | adc_jext_t3cc4  as 5
    | adc_jext_t4cc1  as 6
    | adc_jext_t4cc2  as 7
    | adc_jext_t4cc3  as 8
    | adc_jext_t4trgo as 9
    | adc_jext_t5cc4  as 10
    | adc_jext_t5trgo as 11
    | adc_jext_t8cc2  as 12
    | adc_jext_t8cc3  as 13
    | adc_jext_t8cc4  as 14
    | adc_jext_exti   as 15 -- external pin line 15

  bitdata ADCJL :: Bits 2
    = adc_jl1   as 0
    | adc_jl2   as 1
    | adc_jl3   as 2
    | adc_jl4   as 3
|]
