{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RNG/Regs.hs --- Random Number Generator registers
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.RNG.Regs where

import Ivory.Language

[ivory|
 bitdata RNG_CR   :: Bits 32 = rng_cr
  { _             :: Bits 28
  , rng_cr_ie     :: Bit
  , rng_cr_rngen  :: Bit
  , _             :: Bits 2
  }
|]

[ivory|
 bitdata RNG_SR   :: Bits 32 = rng_sr
  { _             :: Bits 25
  , rng_sr_seis   :: Bit
  , rng_sr_ceis   :: Bit
  , _             :: Bits 2
  , rng_sr_secs   :: Bit
  , rng_sr_cecs   :: Bit
  , rng_sr_drdy   :: Bit
  }
|]

[ivory|
 bitdata RNG_DR   :: Bits 32 = rng_dr
  { rng_dr_data   :: Bits 32
  }
|]

