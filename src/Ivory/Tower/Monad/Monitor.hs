{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Monad.Monitor
  ( Monitor
  , runMonitor
  , monitorPutASTHandler
  , monitorPutModules
  , monitorPutCode
  , monitorPutThreadCode
  ) where

import MonadLib
import Control.Monad.Fix
import Control.Applicative

import Ivory.Tower.Types.Unique
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Monad.Base
import Ivory.Tower.Monad.Tower
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

newtype Monitor a = Monitor
  { unMonitor :: StateT AST.Monitor
                    (StateT (AST.Monitor -> MonitorCode) Tower) a
  } deriving (Functor, Monad, Applicative, MonadFix)

runMonitor :: String -> Monitor ()
           -> Tower (AST.Monitor, MonitorCode)
runMonitor n b = do
  u <- freshname n
  (ast, mkmc) <- runStateT (const emptyMonitorCode)
                    (fmap snd (runStateT (AST.emptyMonitor u) (unMonitor b)))
  return (ast, mkmc ast)

withAST :: (AST.Monitor -> AST.Monitor) -> Monitor ()
withAST f = Monitor $ do
  a <- get
  set (f a)

monitorPutASTHandler :: AST.Handler -> Monitor ()
monitorPutASTHandler a = withAST $
  \s -> s { AST.monitor_handlers = a : AST.monitor_handlers s }

monitorPutModules :: (AST.Monitor -> AST.Tower -> [Module]) -> Monitor ()
monitorPutModules ms = Monitor $ do
  a <- get
  lift $ lift $ towerPutModules $
    \t -> ms (findMonitorAST (AST.monitor_name a) t) t
  where
  findMonitorAST :: Unique -> AST.Tower -> AST.Monitor
  findMonitorAST n twr = maybe err id (AST.towerFindMonitorByName n twr)
  err = error "findMonitorAST failed - broken invariant"

withCode :: (AST.Monitor -> MonitorCode -> MonitorCode) -> Monitor ()
withCode f = Monitor $ do
  a <- lift get
  lift (set (\ctx -> (f ctx (a ctx))))

monitorPutCode :: (AST.Monitor -> ModuleM ()) -> Monitor ()
monitorPutCode f = withCode $ \ctx mc -> insertMonitorCode (f ctx) mc

liftTower :: Tower a -> Monitor a
liftTower a = Monitor $ lift $ lift $ a

monitorPutThreadCode :: (AST.Tower -> [ThreadCode]) -> Monitor ()
monitorPutThreadCode = liftTower . towerPutThreadCode

instance BaseUtils Monitor where
  fresh = liftTower fresh
