{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.OS.FreeRTOS.Tower.Dummy where


import Prelude ()
import Prelude.Compat hiding (length, foldl)

import Control.Monad (forM_)
import Data.List
import qualified Data.Map as Map
import System.FilePath

import Ivory.Language
import Ivory.Artifact
import Ivory.HW
import qualified Ivory.Stdlib as I
import qualified Ivory.Tower.AST as AST
import Ivory.Compile.C.CmdlineFrontend (runCompiler)
import Ivory.Tower.Backend
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Unique

import qualified Ivory.OS.FreeRTOS as FreeRTOS
import Ivory.Tower.Types.Dependencies
import Ivory.Tower (Tower)
import Ivory.Tower.Monad.Tower (runTower)
import Ivory.Tower.Options
import Ivory.Tower.Types.Backend
import Ivory.Tower.Types.Emitter

import Ivory.OS.FreeRTOS.Tower.Dummy.Monitor
import qualified Ivory.OS.FreeRTOS.Tower.Dummy.Build as Dum
import Ivory.OS.FreeRTOS.Tower.Dummy.System
import Ivory.OS.FreeRTOS.Tower.Dummy.Time


import Ivory.BSP.STM32.VectorTable (reset_handler)
import Ivory.BSP.STM32.ClockConfig.Init (init_clocks)
import Ivory.BSP.STM32.Config

data DummyBackend = DummyBackend

instance TowerBackendTypes DummyBackend where
  newtype TowerBackendCallback DummyBackend a = DummyCallback (forall s. AST.Handler -> AST.Thread -> (Def ('[ConstRef s a] ':-> ()), ModuleDef))
  newtype TowerBackendEmitter DummyBackend = DummyEmitter (Maybe (AST.Monitor -> AST.Thread -> EmitterCode))
  data TowerBackendHandler DummyBackend a = DummyHandler AST.Handler (forall s. AST.Monitor -> AST.Thread -> (Def ('[ConstRef s a] ':-> ()), ThreadCode))
  newtype TowerBackendMonitor DummyBackend = DummyMonitor (AST.Tower -> TowerBackendOutput DummyBackend)
    deriving Monoid
  data TowerBackendOutput DummyBackend = DummyOutput
    { compatoutput_threads :: Map.Map AST.Thread ThreadCode
    , compatoutput_monitors :: Map.Map AST.Monitor ModuleDef
    }

instance TowerBackend DummyBackend where

  callbackImpl _ ast f = DummyCallback $ \ h t ->
    let p = proc (callbackProcName ast (AST.handler_name h) t) $ \ r -> body $ noReturn $ f r
    in (p, incl p)

  emitterImpl _ _ [] = (Emitter $ const $ return (), DummyEmitter Nothing)
  emitterImpl _ ast handlers =
    ( Emitter $ call_ $ trampolineProc ast $ const $ return ()
    , DummyEmitter $ Just $ \ mon thd -> emitterCode ast thd [ fst $ h mon thd | DummyHandler _ h <- handlers ]
    )

  handlerImpl _ ast emitters callbacks = DummyHandler ast $ \ mon thd ->
    let ems = [ e mon thd | DummyEmitter (Just e) <- emitters ]
        (cbs, cbdefs) = unzip [ c ast thd | DummyCallback c <- callbacks ]
        runner = handlerProc cbs ems thd mon ast
    in (runner, ThreadCode
      { threadcode_user = sequence_ cbdefs
      , threadcode_emitter = mapM_ emittercode_user ems
      , threadcode_gen = mapM_ emittercode_gen ems >> private (incl runner)
      })

  monitorImpl _ ast handlers moddef = DummyMonitor $ \ twr -> DummyOutput
    { compatoutput_threads = Map.fromListWith mappend
        [ (thd, snd $ h ast thd)
        -- handlers are reversed to match old output for convenient diffs
        | SomeHandler (DummyHandler hast h) <- reverse handlers
        , thd <- AST.handlerThreads twr hast
        ]
    , compatoutput_monitors = Map.singleton ast moddef
    }

  towerImpl _ ast monitors = case mconcat monitors of DummyMonitor f -> f ast

instance Monoid (TowerBackendOutput DummyBackend) where
  mempty = DummyOutput mempty mempty
  mappend a b = DummyOutput
    { compatoutput_threads = Map.unionWith mappend (compatoutput_threads a) (compatoutput_threads b)
    , compatoutput_monitors = Map.unionWith (>>) (compatoutput_monitors a) (compatoutput_monitors b)
    }

data EmitterCode = EmitterCode
  { emittercode_init :: forall eff. Ivory eff ()
  , emittercode_deliver :: forall eff. Ivory eff ()
  , emittercode_user :: ModuleDef
  , emittercode_gen :: ModuleDef
  }


emitterCode :: (IvoryArea a, IvoryZero a)
            => AST.Emitter
            -> AST.Thread
            -> (forall s. [Def ('[ConstRef s a] ':-> ())])
            -> EmitterCode
emitterCode ast thr sinks = EmitterCode
  { emittercode_init = store (addrOf messageCount) 0
  , emittercode_deliver = do
      mc <- deref (addrOf messageCount)
      forM_ (zip messages [0..]) $ \ (m, index) ->
        I.when (fromInteger index <? mc) $
          forM_ sinks $ \ p ->
            call_ p (constRef (addrOf m))

  , emittercode_user = do
      private $ incl trampoline
  , emittercode_gen = do
      incl eproc
      private $ do
        mapM_ defMemArea messages
        defMemArea messageCount
  }
  where
  max_messages = AST.emitter_bound ast - 1
  messageCount :: MemArea ('Stored Uint32)
  messageCount = area (e_per_thread "message_count") Nothing

  messages = [ area (e_per_thread ("message_" ++ show d)) Nothing
             | d <- [0..max_messages] ]

  messageAt idx = foldl aux dflt (zip messages [0..])
    where
    dflt = addrOf (messages !! 0) -- Should be impossible.
    aux basecase (msg, midx) =
      (fromInteger midx ==? idx) ? (addrOf msg, basecase)

  trampoline = trampolineProc ast $ call_ eproc

  eproc = voidProc (e_per_thread "emit")  $ \ msg -> body $ do
               mc <- deref (addrOf messageCount)
               I.when (mc <=? fromInteger max_messages) $ do
                 store (addrOf messageCount) (mc + 1)
                 storedmsg <- assign (messageAt mc)
                 refCopy storedmsg msg

  e_per_thread suffix =
    emitterProcName ast ++ "_" ++ AST.threadName thr ++ "_" ++ suffix

trampolineProc :: IvoryArea a
               => AST.Emitter
               -> (forall eff. ConstRef s a -> Ivory eff ())
               -> Def ('[ConstRef s a] ':-> ())
trampolineProc ast f = proc (emitterProcName ast) $ \ r -> body $ f r

handlerProc :: (IvoryArea a, IvoryZero a)
            => [Def ('[ConstRef s a] ':-> ())]
            -> [EmitterCode]
            -> AST.Thread -> AST.Monitor -> AST.Handler
            -> Def ('[ConstRef s a] ':-> ())
handlerProc callbacks emitters t m h =
  proc (handlerProcName h t) $ \ msg -> body $ do
    comment "init emitters"
    mapM_ emittercode_init emitters
    comment "take monitor lock"
    monitorLockProc m h
    comment "run callbacks"
    forM_ callbacks $ \ cb -> call_ cb msg
    comment "release monitor lock"
    monitorUnlockProc m h
    comment "deliver emitters"
    mapM_ emittercode_deliver emitters

emitterProcName :: AST.Emitter -> String
emitterProcName e = showUnique (AST.emitter_name e)

callbackProcName :: Unique -> Unique -> AST.Thread -> String
callbackProcName callbackname _handlername tast
  =  showUnique callbackname
  ++ "_"
  ++ AST.threadName tast


--------------
-- BACKEND BIS 
--------------

--monitorImplTD :: AST.Monitor -> DummyMonitor
--monitorImplTD mon = 

--towerImplTD :: AST.Tower -> TowerBackendOutput DummyBackend
--towerImplTD a = 




--data TowerBackendOutput DummyBackend = DummyOutput
--    { compatoutput_threads :: Map.Map AST.Thread ThreadCode
--    , compatoutput_monitors :: Map.Map AST.Monitor ModuleDef
--    }

--------

compileTowerDummy :: (e -> STM32Config) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerDummy fromEnv getEnv twr = compileTowerDummyWithOpts fromEnv getEnv twr []


compileTowerDummyWithOpts :: (e -> STM32Config) -> (TOpts -> IO e) -> Tower e () -> [AST.Tower -> IO AST.Tower] -> IO ()
compileTowerDummyWithOpts fromEnv getEnv twr optslist = do
  (copts, topts) <- towerGetOpts
  env <- getEnv topts

  let cfg = fromEnv env
  (ast, monitors, deps, sigs) <- runTower compatBackend twr env optslist
  let o = towerImpl compatBackend ast monitors
  -- TODO reconstruct the o
  let mods = dependencies_modules deps
          ++ threadModules deps sigs (thread_codes o) ast
          ++ monitorModules deps (Map.toList (compatoutput_monitors o))
          ++ stm32Modules cfg ast

      givenArtifacts = dependencies_artifacts deps
      as = stm32Artifacts cfg ast mods givenArtifacts
  runCompiler mods (as ++ givenArtifacts) copts
  where
  compatBackend = DummyBackend

  thread_codes o = Map.toList
                 $ Map.insertWith mappend (AST.InitThread AST.Init) mempty
                 $ compatoutput_threads o

                  
stm32Modules :: STM32Config -> AST.Tower -> [Module]
stm32Modules conf ast = systemModules ast ++ [ main_module, time_module ]
  where
  main_module :: Module
  main_module = package "stm32_main" $ do
    incl reset_handler_proc
    hw_moduledef
    private $ do
      incl (init_clocks (stm32config_clock conf))
      incl init_relocate
      incl init_libc
      incl main_proc

  reset_handler_proc :: Def('[]':->())
  reset_handler_proc = proc reset_handler $ body $ do
    call_ init_relocate
    call_ (init_clocks (stm32config_clock conf))
    call_ init_libc
    call_ main_proc

  init_relocate :: Def('[]':->())
  init_relocate = importProc "init_relocate" "stm32_freertos_init.h"
  init_libc :: Def('[]':->())
  init_libc = importProc "init_libc" "stm32_freertos_init.h"
  main_proc :: Def('[]':->())
  main_proc = importProc "main" "stm32_freertos_init.h"


stm32Artifacts :: STM32Config -> AST.Tower -> [Module] -> [Located Artifact] -> [Located Artifact]
stm32Artifacts conf ast ms gcas = (systemArtifacts ast ms) ++ as
  where
  as = [ Dum.makefile conf makeobjs ] ++ Dum.artifacts conf
    ++ FreeRTOS.kernel fconfig ++ FreeRTOS.wrapper
    ++ hw_artifacts

  makeobjs = nub $ FreeRTOS.objects
          ++ [ moduleName m ++ ".o" | m <- ms ]
          ++ [ replaceExtension f ".o"
             | Src a <- gcas
             , let f = artifactFileName a
             , takeExtension f == ".c"
             ]
  fconfig = FreeRTOS.defaultConfig
    { FreeRTOS.max_priorities = fromIntegral (length (AST.towerThreads ast)) + 1
    -- XXX expand tower config to fill in the rest of these values
    }


