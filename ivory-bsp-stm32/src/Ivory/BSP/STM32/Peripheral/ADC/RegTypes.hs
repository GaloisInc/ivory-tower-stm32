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
|]
