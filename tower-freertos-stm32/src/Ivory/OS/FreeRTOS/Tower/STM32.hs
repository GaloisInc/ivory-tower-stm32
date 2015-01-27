{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Tower.STM32
  ( stm32FreeRTOS
  , module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ) where

import System.FilePath
import Ivory.Language
import Ivory.Artifact
import Ivory.HW
import qualified Ivory.Tower.AST as AST

import qualified Ivory.OS.FreeRTOS as FreeRTOS
import qualified Ivory.Tower.Types.TowerPlatform as T

import           Ivory.OS.FreeRTOS.Tower.System
import           Ivory.OS.FreeRTOS.Tower.Time (time_module)
import qualified Ivory.OS.FreeRTOS.Tower.STM32.Build as STM32
import           Ivory.OS.FreeRTOS.Tower.STM32.Config

import Ivory.BSP.STM32.VectorTable (reset_handler)
import Ivory.BSP.STM32.ClockConfig.Init (init_clocks)

stm32FreeRTOS :: (e -> STM32Config) -> e -> T.TowerPlatform e
stm32FreeRTOS fromEnv e = T.TowerPlatform
  { T.threadModules    = threadModules
  , T.monitorModules   = monitorModules
  , T.systemModules    = stm32Modules (fromEnv e)
  , T.systemArtifacts  = stm32Artifacts (fromEnv e)
  , T.platformEnv      = e
  }

stm32Modules :: STM32Config -> AST.Tower -> [Module]
stm32Modules conf ast = systemModules ast ++ [ main_module, time_module ]
  where
  main_module :: Module
  main_module = package "stm32_main" $ do
    inclHeader "stm32_freertos_init.h"
    incl reset_handler_proc
    hw_moduledef
    private $ do
      incl (init_clocks (stm32config_clock conf))
      incl init_relocate
      incl init_libc
      incl main_proc

  reset_handler_proc :: Def('[]:->())
  reset_handler_proc = proc reset_handler $ body $ do
    call_ init_relocate
    call_ (init_clocks (stm32config_clock conf))
    call_ init_libc
    call_ main_proc

  init_relocate :: Def('[]:->())
  init_relocate = importProc "init_relocate" "stm32_freertos_init.h"
  init_libc :: Def('[]:->())
  init_libc = importProc "init_libc" "stm32_freertos_init.h"
  main_proc :: Def('[]:->())
  main_proc = importProc "main" "stm32_freertos_init.h"


stm32Artifacts :: STM32Config -> AST.Tower -> [Module] -> [Artifact]
stm32Artifacts conf ast ms = (systemArtifacts ast ms) ++ as
  where
  as = [ STM32.makefile makeobjs ] ++ STM32.artifacts conf
    ++ FreeRTOS.kernel fconfig ++ FreeRTOS.wrapper
    ++ hw_artifacts

  makeobjs = FreeRTOS.objects
          ++ [ moduleName m ++ ".o" | m <- ms ]
          ++ [ replaceExtension a ".o"
             | a <- AST.tower_artifact_fs ast
             , takeExtension a == ".c"
             ]
  fconfig = FreeRTOS.defaultConfig
    { FreeRTOS.max_priorities = fromIntegral (length (AST.towerThreads ast)) + 1
    -- XXX expand tower config to fill in the rest of these values
    }

