{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- Advanced Timer (TIM1 and TIM8)  register types
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.ATIM18.RegTypes where

import Ivory.Language

-- Compare Mode bit field definitions:
[ivory|
 bitdata CCMRMode :: Bits 3
   = ccmr_mode_frzn     as 0
   | ccmr_mode_chact    as 1
   | ccmr_mode_chinact  as 2
   | ccmr_mode_ocreftog as 3
   | ccmr_mode_ocreflo  as 4
   | ccmr_mode_ocrefhi  as 5
   | ccmr_mode_pwm1     as 6
   | ccmr_mode_pwm2     as 7

-- Capture/Compare Selection bit field definitions:
 bitdata CCSMode :: Bits 2
   = ccs_mode_out   as 0
   | ccs_mode_in1   as 1
   | ccs_mode_in2   as 2
   | ccs_mode_intrc as 3

-- CR2 Master Mode Selection field definitions:
 bitdata CR2MMS  :: Bits 3
   = cr2_mms_reset          as 0
   | cr2_mms_enable         as 1
   | cr2_mms_update         as 2
   | cr2_mms_compare_pulse  as 3
   | cr2_mms_compare_oc1ref as 4
   | cr2_mms_compare_oc2ref as 5
   | cr2_mms_compare_oc3ref as 6
   | cr2_mms_compare_oc4ref as 7

-- CR1 Direction
bitdata CR1DIR  :: Bit
   = cr1_dir_up   as 0
   | cr1_dir_down as 1

-- CR1 Center-aligned mode Selection field definitions:
bitdata CR1CMS  :: Bits 2
   = cr1_cms_edge_align    as 0
   | cr1_cms_center_align1 as 1
   | cr1_cms_center_align2 as 2
   | cr1_cms_center_align3 as 3
|]
