{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.BSP.STM32.Driver.RNG
  ( rngTower
  , RNG()
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.RNG

rngTower :: (Time t)
         => RNG
         -> t
         -> Tower e (ChanOutput ('Stored Uint32))
rngTower rng t = do
  reschan <- channel

  per <- period t

  monitor "rngPeripheral" $ do
    monitorModuleDef $ hw_moduledef

    handler systemInit "rngInit" $ do
      callback $ const $ do
        rngRCCEnable rng
        modifyReg (rngRegCR rng) $ do
          setBit rng_cr_rngen

    handler per "rngPoll" $ do
      e <- emitter (fst reschan) 1
      callback $ const $ do
        sr <- getReg (rngRegSR rng)
        when (         bitToBool (sr #. rng_sr_drdy) -- Data is ready
             .&& iNot (bitToBool (sr #. rng_sr_secs)) -- Seed error not present
             .&& iNot (bitToBool (sr #. rng_sr_cecs))) $ do -- Clock error not present
          dr <- getReg (rngRegDR rng)
          dr_u32 <- assign (toRep (dr #. rng_dr_data))
          emitV e dr_u32

  return (snd reschan)
