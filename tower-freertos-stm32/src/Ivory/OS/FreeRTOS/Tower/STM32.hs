{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.FreeRTOS.Tower.STM32
  ( compileTowerSTM32FreeRTOS
  , module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ) where

import Control.Arrow (second)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Monoid
import System.FilePath
import Ivory.Language
import Ivory.Artifact
import Ivory.HW
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Backend.Compat

import qualified Ivory.OS.FreeRTOS as FreeRTOS
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.SignalCode
import Ivory.Tower (Tower)
import Ivory.Tower.Monad.Tower (runTower)
import Ivory.Tower.Compile

import           Ivory.OS.FreeRTOS.Tower.System
import           Ivory.OS.FreeRTOS.Tower.Time (time_module)
import qualified Ivory.OS.FreeRTOS.Tower.STM32.Build as STM32
import           Ivory.OS.FreeRTOS.Tower.STM32.Config

import Ivory.BSP.STM32.VectorTable (reset_handler)
import Ivory.BSP.STM32.ClockConfig.Init (init_clocks)

newtype Wrapper = Wrapper CompatBackend

unWrapSomeHandler :: SomeHandler Wrapper -> SomeHandler CompatBackend
unWrapSomeHandler (SomeHandler (WrapHandler h)) = SomeHandler h

instance TowerBackend Wrapper where
  newtype TowerBackendCallback Wrapper a = WrapCallback { unWrapCallback :: TowerBackendCallback CompatBackend a }
  newtype TowerBackendEmitter Wrapper = WrapEmitter { unWrapEmitter :: TowerBackendEmitter CompatBackend }
  newtype TowerBackendHandler Wrapper a = WrapHandler { unWrapHandler :: TowerBackendHandler CompatBackend a }
  newtype TowerBackendMonitor Wrapper = WrapMonitor { unWrapMonitor :: TowerBackendMonitor CompatBackend }
  data TowerBackendOutput Wrapper = WrapOutput (TowerBackendOutput CompatBackend) AST.Tower

  callbackImpl (Wrapper b) ast cb = WrapCallback $ callbackImpl b ast cb
  emitterImpl (Wrapper b) ast sinks = second WrapEmitter $ emitterImpl b ast $ map unWrapHandler sinks
  handlerImpl (Wrapper b) ast ems cbs = WrapHandler $ handlerImpl b ast (map unWrapEmitter ems) (map unWrapCallback cbs)
  monitorImpl (Wrapper b) ast hs moddef = WrapMonitor $ monitorImpl b ast (map unWrapSomeHandler hs) moddef
  towerImpl (Wrapper b) ast mons = WrapOutput (towerImpl b ast $ map unWrapMonitor mons) ast


compileTowerSTM32FreeRTOS :: (e -> STM32Config) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerSTM32FreeRTOS fromEnv getEnv twr = do
  (topts, compile) <- towerCompile
  env <- getEnv topts

  let cfg = fromEnv env
      (ast, output, deps, sigs) = runTower compatBackend twr env

      addModules :: GeneratedCode -> AST.Tower -> [Module]
      addModules = threadModules <> monitorModules

      mods = dependencies_modules deps
          ++ withGC addModules output deps sigs
          ++ stm32Modules cfg ast

      givenArtifacts = dependencies_artifacts deps
      as = stm32Artifacts cfg ast mods givenArtifacts
  compile mods (as ++ givenArtifacts)
  where
  compatBackend = Wrapper CompatBackend

withGC :: (GeneratedCode -> AST.Tower -> a) -- f
       -> TowerBackendOutput Wrapper        -- Wrapoutput
       -> Dependencies                      -- deps
       -> SignalCode                        -- sigs
       -> a
withGC f (WrapOutput output ast) deps sigs = f gc ast
  where
  gc = GeneratedCode
    { generatedcode_modules = dependencies_modules deps
    , generatedcode_depends = dependencies_depends deps
    , generatedcode_threads = Map.insertWith mappend initThread mempty $ compatoutput_threads output
    , generatedcode_monitors = Map.map MonitorCode $ compatoutput_monitors output
    , generatedcode_signals = signalcode_signals sigs
    , generatedcode_init = signalcode_init sigs
    , generatedcode_artifacts = dependencies_artifacts deps
    }
  initThread = AST.InitThread AST.Init

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


stm32Artifacts :: STM32Config -> AST.Tower -> [Module] -> [Artifact] -> [Artifact]
stm32Artifacts conf ast ms gcas = (systemArtifacts ast ms) ++ as
  where
  as = [ STM32.makefile conf makeobjs ] ++ STM32.artifacts conf
    ++ FreeRTOS.kernel fconfig ++ FreeRTOS.wrapper
    ++ hw_artifacts

  makeobjs = nub $ FreeRTOS.objects
          ++ [ moduleName m ++ ".o" | m <- ms ]
          ++ [ replaceExtension f ".o"
             | a <- gcas
             , let f = artifactFileName a
             , takeExtension f == ".c"
             ]
  fconfig = FreeRTOS.defaultConfig
    { FreeRTOS.max_priorities = fromIntegral (length (AST.towerThreads ast)) + 1
    -- XXX expand tower config to fill in the rest of these values
    }

