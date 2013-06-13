{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS
  ( compile
  , os
  , searchDir
  ) where

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Tower (assembleTower)

import qualified Ivory.OS.FreeRTOS.Task  as Task

import Ivory.Tower.Compile.FreeRTOS.SharedState
import Ivory.Tower.Compile.FreeRTOS.ChannelQueues
import Ivory.Tower.Compile.FreeRTOS.Schedule

import Ivory.Tower.Compile.FreeRTOS.SearchDir

compile :: Tower () -> (Assembly, [Module])
compile t = (asm, ms)
  where
  asm = assembleTower t os
  ms = buildModules asm

buildModules :: Assembly -> [Module]
buildModules asm = ms
  where
  towerst = asm_towerst asm
  (tnodes, taskentrys, taskMods) = unzip3 $ asm_taskdefs asm
  (sys_mdef, sys_initdef) = asm_system asm
  tasks = map nodest_impl tnodes

  ms = [ twr_entry ]
    ++ towerst_modules towerst
    ++ concatMap taskst_extern_mods tasks

  twr_entry = package "tower" $ do
    mapM_ inclHeader headerdeps
    mapM_ sourceDep  headerdeps
    mapM_ sourceDep  sourcedeps
    mapM_ incl taskentrys
    sequence_ taskMods
    mapM_ ncg_mdef $ concatMap nodest_codegen tnodes
    -- mapM_ ncg_mdef $ concatMap nodest_codegen tsnodes -- XXX signals
    towerst_moddef towerst
    sys_mdef
    incl towerentry

  towerentry :: Def ('[]:->())
  towerentry = proc "tower_entry" $ body $ do
    call_ sys_initdef
    mapM_ (call_ . ncg_init) $ concatMap nodest_codegen tnodes
--    mapM_ call_ $ concatMap (ncg_init . nodest_codegen) snodes -- XXX  implement signals
    mapM_ call_ $ towerst_dataportinit towerst
    mapM_ taskCreate (zip tasks taskentrys)
    retVoid

taskCreate :: (TaskSt, Def('[]:->())) -> Ivory eff ()
taskCreate (taskst, entry) = call_ Task.create pointer stacksize priority
  where
  pointer = procPtr entry
  stacksize = maybe defaultstacksize fromIntegral (taskst_stacksize taskst)
  priority = defaulttaskpriority + (maybe 0 fromIntegral (taskst_priority taskst))

os :: OS
os = OS
  { os_mkDataPort     = mkDataPort
  , os_mkTaskSchedule = mkTaskSchedule
  , os_mkSysSchedule  = mkSystemSchedule
  , os_mkSigSchedule  = undefined -- XXX
  , os_mkChannel      = mkChannel
  , os_getTimeMillis  = call Task.getTimeMillis
  }

mkDataPort :: forall (area :: Area) . (IvoryArea area)
           => DataSource area -> (Def ('[]:->()), ModuleDef)
mkDataPort source = (fdp_initDef fdp, fdp_moduleDef fdp)
  where
  fdp :: FreeRTOSDataport area
  fdp = sharedState (unDataSource source)

mkChannel :: forall (area :: Area) i . (IvoryArea area, IvoryZero area)
              => ChannelReceiver area
              -> NodeSt i
              -> (Def('[]:->()), ModuleDef)
mkChannel rxer destNode = (fch_initDef fch, fch_moduleDef fch)
  where
  fch :: FreeRTOSChannel area
  fch = eventQueue (unChannelReceiver rxer) destNode

defaultstacksize :: Uint32
defaultstacksize = 256

-- XXX since our scheduler now depends on round robin to schedule the loop
-- bodies, better make sure that every eventloop task has the default priority
defaulttaskpriority :: Uint8
defaulttaskpriority = 1

-- ivory-freertos-wrapper

headerdeps :: [FilePath]
headerdeps =
  [ "freertos_queue_wrapper.h"
  , "freertos_semaphore_wrapper.h"
  , "freertos_task_wrapper.h"
  ]

sourcedeps :: [FilePath]
sourcedeps =
  [ "freertos_queue_wrapper.c"
  , "freertos_semaphore_wrapper.c"
  , "freertos_task_wrapper.c"
  ]


