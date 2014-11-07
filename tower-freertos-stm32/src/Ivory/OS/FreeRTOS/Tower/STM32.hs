
module Ivory.OS.FreeRTOS.Tower.STM32
  ( stm32FreeRTOS
  , module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ) where

import Ivory.Language
import Ivory.Artifact
import Ivory.HW
import qualified Ivory.Tower.AST as AST

import qualified Ivory.OS.FreeRTOS as FreeRTOS
import qualified Ivory.Tower.Types.TowerPlatform as T

import Ivory.OS.FreeRTOS.Tower.System
import qualified Ivory.OS.FreeRTOS.Tower.STM32.Build as STM32
import           Ivory.OS.FreeRTOS.Tower.STM32.Config

stm32FreeRTOS :: Config -> T.TowerPlatform
stm32FreeRTOS config = T.TowerPlatform
  { T.platformName     = "stm32FreeRTOS"
  , T.threadModules    = threadModules
  , T.monitorModules   = monitorModules
  , T.systemModules    = systemModules
  , T.systemArtifacts  = stm32Artifacts config
  }

stm32Artifacts :: Config -> AST.Tower -> [Module] -> [Artifact]
stm32Artifacts conf ast ms = (systemArtifacts ast ms) ++ as
  where
  as = [ STM32.makefile objs ] ++ STM32.artifacts conf
    ++ FreeRTOS.kernel fconfig ++ FreeRTOS.wrapper
    ++ hw_artifacts

  objs = FreeRTOS.objects ++ [ moduleName m ++ ".o" | m <- ms ]
  fconfig = FreeRTOS.defaultConfig
    { FreeRTOS.max_priorities = fromIntegral (length (AST.towerThreads ast)) + 1
    -- XXX expand tower config to fill in the rest of these values
    }
