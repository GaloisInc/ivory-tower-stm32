{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Tower.Monitor
  ( generateMonitorCode
  , monitorInitProc
  , monitorLockProc
  , monitorUnlockProc
  , monitorStateModName
  , monitorGenModName
  , monitorLockProcName
  , monitorUnlockProcName
  ) where

import Ivory.Tower.Types.Dependencies

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Opts

import Ivory.Language
import qualified Ivory.OS.FreeRTOS.Mutex as Mutex
import Data.Foldable (traverse_)
import Data.List

monitorStateModName :: AST.Monitor -> String
monitorStateModName mon = "tower_state_monitor_" ++ AST.monitorName mon

monitorGenModName :: AST.Monitor -> String
monitorGenModName mon = "tower_gen_monitor_" ++ AST.monitorName mon

generateMonitorCode :: Dependencies
                    -> (AST.Monitor, ModuleDef)
                    -> [Module]
generateMonitorCode d (mon, moddef)  =
  [ package (monitorStateModName mon) $ do
      dependencies
      moddef
  , package (monitorGenModName mon) $ do
      dependencies
      gen_pkg
  ]
  where
  dependencies = mapM_ depend (dependencies_depends d)
  gen_pkg = do
    Mutex.moddef
    defMemArea (monitorLockArea mon)
    incl (monitorInitProc mon)

monitorLockName :: AST.Monitor -> String
monitorLockName mon = "lock_"  ++ AST.monitorName mon

monitorLockArea :: AST.Monitor -> MemArea Mutex.Mutex
monitorLockArea mon = area (monitorLockName mon) Nothing

monitorLock :: AST.Monitor -> Mutex.MutexHandle
monitorLock mon = addrOf (monitorLockArea mon)


monitorLockAreaWithCoarsening :: AST.Monitor -> Int -> MemArea Mutex.Mutex
monitorLockAreaWithCoarsening mon lockId = area ("lock" ++ (show lockId) ++ "_"  ++ AST.monitorName mon) Nothing

monitorLockWithCoarsening :: AST.Monitor -> Int -> Mutex.MutexHandle
monitorLockWithCoarsening mon lockId = addrOf (monitorLockAreaWithCoarsening mon lockId)

monitorInitProc :: AST.Monitor -> Def('[]':->())
monitorInitProc mon = 
  if (LockCoarsening OptVoid `elemOpt` (AST.monitor_transformers mon))
    then 
      monitorInitProcLockCoarsening mon
    else
      monitorInitProcRaw mon

monitorInitProcRaw :: AST.Monitor -> Def('[]':->())
monitorInitProcRaw mon = proc n $ body $ do
  call_ Mutex.create (monitorLock mon)
  where
  n = "monitor_init_" ++ AST.monitorName mon

monitorInitProcLockCoarsening :: AST.Monitor -> Def('[]':->())
monitorInitProcLockCoarsening mon = proc n $ body $ 
  traverse_ (call_ Mutex.create) $ map (monitorLockWithCoarsening mon) [1..(length $ globmon)]
  where
    (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon
    n = "monitor_init_" ++ AST.monitorName mon


monitorUnlockProc :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorUnlockProc mon h =
  if (LockCoarsening OptVoid `elemOpt` (AST.monitor_transformers mon))
    then 
      monitorUnlockProcLockCoarsening mon h
    else
      monitorUnlockProcRaw mon


monitorUnlockProcRaw :: AST.Monitor -> Ivory eff ()
monitorUnlockProcRaw mon = 
  call_ Mutex.give (monitorLock mon)


monitorUnlockProcLockCoarsening :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorUnlockProcLockCoarsening mon h = 
  traverse_ (call_ Mutex.give) $ map (monitorLockWithCoarsening mon) locksToTake
  where
    (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon
    (Just (LockCoarsening (OptHandler globhan))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h
    locksToTake :: [Int]
    locksToTake = map succ $ nub $ concat $ map (\x -> findIndices (\list -> elem x list) $ globmon) $ globhan


monitorLockProc :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorLockProc mon h =
  if (LockCoarsening OptVoid `elemOpt` (AST.monitor_transformers mon))
    then 
      monitorLockProcLockCoarsening mon h
    else
      monitorLockProcRaw mon

monitorLockProcRaw :: AST.Monitor -> Ivory eff ()
monitorLockProcRaw mon = 
  call_ Mutex.take (monitorLock mon)

monitorLockProcLockCoarsening :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorLockProcLockCoarsening mon h = 
  traverse_ (call_ Mutex.take) $ map (monitorLockWithCoarsening mon) locksToTake
  where
    (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon
    (Just (LockCoarsening (OptHandler globhan))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h
    locksToTake :: [Int]
    locksToTake = map succ $ nub $ concat $ map (\x -> findIndices (\list -> elem x list) $ globmon) $ globhan


monitorUnlockProcName :: AST.Monitor -> String
monitorUnlockProcName mon = "monitor_unlock_" ++ AST.monitorName mon

monitorLockProcName :: AST.Monitor -> String
monitorLockProcName mon = "monitor_lock_" ++ AST.monitorName mon

