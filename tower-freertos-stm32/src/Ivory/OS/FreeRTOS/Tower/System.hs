{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Tower.System
  ( threadModules
  , monitorModules
  , systemModules
  , systemArtifacts
  ) where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.List (partition, sort, elemIndex)

import Text.Show.Pretty

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Time
import Ivory.Tower.Codegen.Handler
import qualified Ivory.Tower.AST.Graph as G
import qualified Ivory.Tower.AST as AST

import Ivory.Language
import Ivory.Artifact
import Ivory.Artifact.Location

import Ivory.OS.FreeRTOS.Tower.Signal
import Ivory.OS.FreeRTOS.Tower.Monitor

import qualified Ivory.OS.FreeRTOS.Task as Task
import qualified Ivory.OS.FreeRTOS.Time as Time

systemArtifacts :: AST.Tower -> [Module] -> [Located Artifact]
systemArtifacts twr mods = map Root
  [ artifactString "debug_mods.txt" dbg
  , artifactString "debug_ast.txt" (ppShow twr)
  , artifactString "out.dot" (G.graphviz (G.messageGraph twr))
  ]
  where
  dbg = show (map moduleName mods)

monitorModules :: GeneratedCode -> AST.Tower -> [Module]
monitorModules gc _twr = concatMap permon mods
  where
  mods = Map.toList (generatedcode_monitors gc)
  permon (ast, code) = generateMonitorCode gc code ast

threadModules :: GeneratedCode -> AST.Tower-> [Module]
threadModules gc twr = concatMap pertask (Map.toList (generatedcode_threads gc))
  where
  pertask tc = [threadUserModule tc, threadGenModule tc]
  threadUserModule (t, tc) =
    package (AST.threadUserCodeModName t) $ do
      dependencies
      threadMonitorDeps t monitorStateModName
      depend (threadGenModule (t, tc))
      depend (package "tower_time" (return ())) -- Provide in per-platform codegen
      threadcode_user tc
      threadcode_emitter tc
  threadGenModule (t, tc) =
    package (AST.threadGenCodeModName t) $ do
      dependencies
      depend (threadUserModule (t, tc))
      threadMonitorDeps t monitorGenModName
      threadLoopModdef gc twr t
      threadcode_gen tc

  dependencies = mapM_ depend (generatedcode_depends gc)

  threadMonitorDeps :: AST.Thread -> (AST.Monitor -> String) -> ModuleDef
  threadMonitorDeps t mname = sequence_
    [ depend $ package (mname m) $ return ()
    | (m,_) <- AST.threadHandlers (AST.messageGraph twr) t ]

threadLoopRunHandlers :: AST.Tower -> AST.Thread
                      -> Ref s (Stored ITime) -> Ivory eff ()
threadLoopRunHandlers twr thr t = sequence_
  [ call_ (hproc h) (constRef t)
  | (_m,h) <- AST.towerChanHandlers twr (AST.threadChan thr) ]
  where
  hproc :: AST.Handler -> Def('[ConstRef s (Stored ITime)]:->())
  hproc h = proc (handlerProcName h thr) (const (body (return ())))

threadLoopModdef :: GeneratedCode -> AST.Tower -> AST.Thread -> ModuleDef
threadLoopModdef _gc twr thr@(AST.PeriodThread p) = do
  Task.moddef
  Time.moddef
  incl tloopProc

  where
  period_ms :: Uint32
  period_ms = fromIntegral (toMilliseconds (AST.period_dt p))
  tloopProc :: Def('[Ref Global (Struct "taskarg")]:->())
  tloopProc = proc (AST.threadLoopProcName thr) $ const $ body $ noReturn $ do
    tick_rate <- call Time.getTickRateMilliseconds
    let tickITime :: Uint32 -> ITime
        tickITime t = fromIMilliseconds (t `iDiv` tick_rate)
        timeToTick :: ITime -> Uint32
        timeToTick t = castWith 0 (toIMicroseconds t) * tick_rate

    ticks_now <- call Time.getTickCount -- XXX add phase
    let tick_init = ticks_now + timeToTick (toITime (AST.period_phase p))
    ticks_last_wake <- local (ival (tick_init))

    forever $ noBreak $ do
      -- Invariant: delayUntil stores the sum of the two arguments in
      -- ticks_last_wake. It does not store the actual time at which it last
      -- woke.
      call_ Time.delayUntil ticks_last_wake (tick_rate * period_ms)
      now <- deref ticks_last_wake
      t <- local (ival (tickITime now))
      threadLoopRunHandlers twr thr t

threadLoopModdef gc twr thr@(AST.SignalThread s) = do
  Task.moddef
  Time.moddef
  incl tloopProc
  codegensignal_moddef cgs

  unGeneratedSignal (generatedCodeForSignal s gc) $ codegensignal_ready cgs
  where
  cgs = codegenSignal thr
  tloopProc :: Def('[Ref Global (Struct "taskarg")]:->())
  tloopProc = proc (AST.threadLoopProcName thr) $ const $ body $ noReturn $ do
    t_rate <- call Time.getTickRateMilliseconds
    let tickITime :: Uint32 -> ITime
        tickITime t = fromIMilliseconds (t `iDiv` t_rate)
    forever $ noBreak $ do
      codegensignal_wait cgs
      now <- call Time.getTickCount
      t <- local (ival (tickITime now))
      threadLoopRunHandlers twr thr t

threadLoopModdef gc twr thr@(AST.InitThread _) = do
  Time.moddef
  incl $ proc (AST.threadLoopProcName thr) $ body $ do
    t <- local (ival 0)
    generatedcode_init gc
    noReturn $ threadLoopRunHandlers twr thr t
    retVoid

isInitThread :: AST.Thread -> Bool
isInitThread (AST.InitThread _) = True
isInitThread _ = False

systemModules :: AST.Tower -> [Module]
systemModules twr = [initModule]
  where
  initModule = package "tower_init" $ do
    Task.moddef
    sequence_ [ depend (package (monitorGenModName m) (return ()))
              | m <- AST.tower_monitors twr ]
    sequence_ [ depend (package (AST.threadGenCodeModName t) (return ()))
              | t <- AST.towerThreads twr ]
    incl entryProc
    where
    entryProc :: Def('[]:->())
    entryProc = proc "tower_entry" $ body $ do
      forM_ (AST.tower_monitors twr) $ \m -> do
        call_ (monitorInitProc m)
      forM_ (AST.towerThreads twr) $ \thr -> do
        codegensignal_init (codegenSignal thr)

      -- Init threads don't need actual threads.
      let (initThreads, otherThreads) = partition isInitThread $ AST.towerThreads twr
      forM_ initThreads $ \ thr ->
        call_ $ proc (AST.threadLoopProcName thr) $ body $ retVoid
      forM_ otherThreads threadBegin

  threadBegin :: AST.Thread -> Ivory eff ()
  threadBegin thr = do
    call_ Task.begin (Task.taskProc threadLoopProcStub)
                  stacksize priority debugname
    where
    threadLoopProcStub :: Def('[Ref s (Struct "taskarg")]:->())
    threadLoopProcStub = proc (AST.threadLoopProcName thr)
                          (const (body (return ())))
    stacksize :: Uint32
    stacksize = 4096 -- XXX need some story for computing this

    debugname :: IString
    debugname = fromString (AST.threadName thr)

    priority :: Uint8
    priority = fromIntegral (idx + 1)
      where
      Just idx = elemIndex thr priorityordering
      priorityordering = reverse (sort (AST.towerThreads twr))

