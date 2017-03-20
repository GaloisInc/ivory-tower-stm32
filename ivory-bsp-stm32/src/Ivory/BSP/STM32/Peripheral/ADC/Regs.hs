{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- ADC peripheral registers for the STM32F4.
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.ADC.Regs where

import Ivory.BSP.STM32.Peripheral.ADC.RegTypes
import Ivory.Language

[ivory|
  bitdata ADC_SR :: Bits 32 = adc_sr
  { _ :: Bits 26
  , adc_sr_ovr :: Bit
  , adc_sr_strt :: Bit
  , adc_sr_jstrt :: Bit
  , adc_sr_jeoc :: Bit
  , adc_sr_eoc :: Bit
  , adc_sr_awd :: Bit
  }

  bitdata ADC_CR1 :: Bits 32 = adc_cr1
  { _ :: Bits 5
  , adc_cr1_ovrie :: Bit
  , adc_cr1_res :: ADCResolution
  , adc_cr1_awden :: Bit
  , adc_cr1_jawden :: Bit
  , _ :: Bits 6
  , adc_cr1_discnum :: Bits 3
  , adc_cr1_jdiscen :: Bit
  , adc_cr1_discen :: Bit
  , adc_cr1_jauto :: Bit
  , adc_cr1_awdsgl :: Bit
  , adc_cr1_scan :: Bit
  , adc_cr1_jeocie :: Bit
  , adc_cr1_awdie :: Bit
  , adc_cr1_eocie :: Bit
  , adc_cr1_awdch :: Bits 5
  }

  bitdata ADC_CR2 :: Bits 32 = adc_cr2
  { _ :: Bit
  , adc_cr2_swstart :: Bit
  , adc_cr2_exten :: ADCExtEn
  , adc_cr2_extsel :: ADCExtSel
  , _ :: Bit
  , adc_cr2_jswstart :: Bit
  , adc_cr2_jexten :: ADCExtEn
  , adc_cr2_jextsel :: ADCJExtSel
  , _ :: Bits 4
  , adc_cr2_align :: Bit
  , adc_cr2_eocs :: Bit
  , adc_cr2_dds :: Bit
  , adc_cr2_dma :: Bit
  , _ :: Bits 6
  , adc_cr2_cont :: Bit
  , adc_cr2_adon :: Bit
  }

  bitdata ADC_SMPR1 :: Bits 32 = adc_smpr1
  { _ :: Bits 5
  , adc_smpr1_smp18 :: Bits 3
  , adc_smpr1_smp17 :: Bits 3
  , adc_smpr1_smp16 :: Bits 3
  , adc_smpr1_smp15 :: Bits 3
  , adc_smpr1_smp14 :: Bits 3
  , adc_smpr1_smp13 :: Bits 3
  , adc_smpr1_smp12 :: Bits 3
  , adc_smpr1_smp11 :: Bits 3
  , adc_smpr1_smp10 :: Bits 3
  }

  bitdata ADC_SMPR2 :: Bits 32 = adc_smpr2
  { _ :: Bits 2
  , adc_smpr2_smp9 :: Bits 3
  , adc_smpr2_smp8 :: Bits 3
  , adc_smpr2_smp7 :: Bits 3
  , adc_smpr2_smp6 :: Bits 3
  , adc_smpr2_smp5 :: Bits 3
  , adc_smpr2_smp4 :: Bits 3
  , adc_smpr2_smp3 :: Bits 3
  , adc_smpr2_smp2 :: Bits 3
  , adc_smpr2_smp1 :: Bits 3
  }

  bitdata ADC_JOFR1 :: Bits 32 = adc_jofr1
  { _ :: Bits 20
  , adc_jofr1_data :: Bits 12
  }

  bitdata ADC_JOFR2 :: Bits 32 = adc_jofr2
  { _ :: Bits 20
  , adc_jofr2_data :: Bits 12
  }

  bitdata ADC_JOFR3 :: Bits 32 = adc_jofr3
  { _ :: Bits 20
  , adc_jofr3_data :: Bits 12
  }

  bitdata ADC_JOFR4 :: Bits 32 = adc_jofr4
  { _ :: Bits 20
  , adc_jofr4_data :: Bits 12
  }

  bitdata ADC_HTR :: Bits 32 = adc_htr
  { _ :: Bits 20
  , adc_htr_data :: Bits 12
  }

  bitdata ADC_LTR :: Bits 32 = adc_ltr
  { _ :: Bits 20
  , adc_ltr_data :: Bits 12
  }

  bitdata ADC_SQR1 :: Bits 32 = adc_sqr1
  { _ :: Bits 8
  , adc_sqr1_l :: Bits 4
  , adc_sqr1_sq16 :: Bits 5
  , adc_sqr1_sq15 :: Bits 5
  , adc_sqr1_sq14 :: Bits 5
  , adc_sqr1_sq13 :: Bits 5
  }

  bitdata ADC_SQR2 :: Bits 32 = adc_sqr2
  { _ :: Bits 2
  , adc_sqr2_sq12 :: Bits 5
  , adc_sqr2_sq11 :: Bits 5
  , adc_sqr2_sq10 :: Bits 5
  , adc_sqr2_sq9 :: Bits 5
  , adc_sqr2_sq8 :: Bits 5
  , adc_sqr2_sq7 :: Bits 5
  }

  bitdata ADC_SQR3 :: Bits 32 = adc_sqr3
  { _ :: Bits 2
  , adc_sqr3_sq6 :: Bits 5
  , adc_sqr3_sq5 :: Bits 5
  , adc_sqr3_sq4 :: Bits 5
  , adc_sqr3_sq3 :: Bits 5
  , adc_sqr3_sq2 :: Bits 5
  , adc_sqr3_sq1 :: Bits 5
  }

  bitdata ADC_JSQR :: Bits 32 = adc_jsqr
  { _ :: Bits 10
  , adc_jsqr_jl  :: ADCJL
  , adc_jsqr4    :: Bits 5
  , adc_jsqr3    :: Bits 5
  , adc_jsqr2    :: Bits 5
  , adc_jsqr1    :: Bits 5
  }

  bitdata ADC_JDR1 :: Bits 32 = adc_jdr1
  { _ :: Bits 16
  , adc_jdr1_data  :: Bits 16
  }

  bitdata ADC_JDR2 :: Bits 32 = adc_jdr2
  { _ :: Bits 16
  , adc_jdr2_data  :: Bits 16
  }

  bitdata ADC_JDR3 :: Bits 32 = adc_jdr3
  { _ :: Bits 16
  , adc_jdr3_data  :: Bits 16
  }

  bitdata ADC_JDR4 :: Bits 32 = adc_jdr4
  { _ :: Bits 16
  , adc_jdr4_data  :: Bits 16
  }

  bitdata ADC_DR :: Bits 32 = adc_dr
  { _ :: Bits 16
  , adc_dr_data :: Bits 16
  }

  bitdata ADC_CSR :: Bits 32 = adc_csr
  { _ :: Bits 10
  , adc_csr_ovr3   :: Bit
  , adc_csr_strt3  :: Bit
  , adc_csr_jstrt3 :: Bit
  , adc_csr_jeoc3  :: Bit
  , adc_csr_eoc3   :: Bit
  , adc_csr_awd3   :: Bit
  , _              :: Bits 2
  , adc_csr_ovr2   :: Bit
  , adc_csr_strt2  :: Bit
  , adc_csr_jstrt2 :: Bit
  , adc_csr_jeoc2  :: Bit
  , adc_csr_eoc2   :: Bit
  , adc_csr_awd2   :: Bit
  , _              :: Bits 2
  , adc_csr_ovr1   :: Bit
  , adc_csr_strt1  :: Bit
  , adc_csr_jstrt1 :: Bit
  , adc_csr_jeoc1  :: Bit
  , adc_csr_eoc1   :: Bit
  , adc_csr_awd1   :: Bit
  }

  bitdata ADC_CCR :: Bits 32 = adc_ccr
  { _ :: Bits 8
  , adc_ccr_tsvrefe :: Bit
  , adc_ccr_vbate   :: Bit
  , _               :: Bits 4
  , adc_ccr_adcpre  :: ADCPrescaler
  -- wat
  , adc_ccr_dma     :: Bits 2
  , _               :: Bit
  , adc_ccr_delay   :: Bits 4
  , _               :: Bits 3
  , adc_ccr_multi   :: Bits 5
  }

  bitdata ADC_CDR :: Bits 32 = adc_cdr
  { adc_dr_data2 :: Bits 16
  , adc_dr_data1 :: Bits 16
  }
|]
