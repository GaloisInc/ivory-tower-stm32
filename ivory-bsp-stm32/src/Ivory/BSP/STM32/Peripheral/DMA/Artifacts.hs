{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.BSP.STM32.Peripheral.DMA.Artifacts where

import qualified Paths_ivory_bsp_stm32 as P
import Ivory.Artifact

dmaRefToUint32Header :: String
dmaRefToUint32Header = "ref_to_uint32.h"

dmaArtifacts :: [Located Artifact]
dmaArtifacts = [ Incl (a dmaRefToUint32Header), Src (a "ref_to_uint32.c") ]
  where a fname = artifactCabalFile P.getDataDir ("support/" ++ fname)


