{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.OS.FreeRTOS.Tower.STM32
  ( compileTowerSTM32FreeRTOS
  , parseTowerSTM32FreeRTOS
  , compileTowerSTM32FreeRTOSWithOpts
  , parseTowerSTM32FreeRTOSWithOpts
  , module Ivory.BSP.STM32.Config
  ) where

import Prelude ()
import Prelude.Compat hiding (length, foldl, null, concat)

import Control.Monad (forM_)
import Data.List
import qualified Data.Map as Map
import System.FilePath
import MonadLib (put)

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

import           Ivory.OS.FreeRTOS.Tower.System
import           Ivory.OS.FreeRTOS.Tower.Monitor
import           Ivory.OS.FreeRTOS.Tower.Time (time_module)
import qualified Ivory.OS.FreeRTOS.Tower.STM32.Build as STM32

import Ivory.BSP.STM32.VectorTable (reset_handler)
import Ivory.BSP.STM32.ClockConfig.Init (init_clocks)
import Ivory.BSP.STM32.Config

import qualified Ivory.Language.Module as Mod
import qualified Ivory.Language.Monad as Mon
import qualified Ivory.Language.Syntax.AST as IAST
import qualified Ivory.Language.Syntax.Names as IAST
import qualified Ivory.Language.Syntax.Type as TIAST
import Ivory.Language.MemArea (primAddrOf)
import Ivory.Language.Proc (initialClosure, genVar)
import Ivory.Language.MemArea (makeArea)

data STM32FreeRTOSBackend = STM32FreeRTOSBackend

instance TowerBackendTypes STM32FreeRTOSBackend where
  newtype TowerBackendCallback STM32FreeRTOSBackend a = STM32FreeRTOSCallback (forall s. AST.Handler -> AST.Thread -> (Def ('[ConstRef s a] ':-> ()), ModuleDef))
  newtype TowerBackendEmitter STM32FreeRTOSBackend = STM32FreeRTOSEmitter (Maybe (AST.Monitor -> AST.Thread -> EmitterCode))
  data TowerBackendHandler STM32FreeRTOSBackend a = STM32FreeRTOSHandler AST.Handler (forall s. AST.Monitor -> AST.Thread -> (Def ('[ConstRef s a] ':-> ()), ThreadCode))
  newtype TowerBackendMonitor STM32FreeRTOSBackend = STM32FreeRTOSMonitor (AST.Tower -> TowerBackendOutput STM32FreeRTOSBackend)
    deriving Monoid
  data TowerBackendOutput STM32FreeRTOSBackend = STM32FreeRTOSOutput
    { compatoutput_threads :: Map.Map AST.Thread ThreadCode
    , compatoutput_monitors :: Map.Map AST.Monitor ModuleDef
    }

instance TowerBackend STM32FreeRTOSBackend where

  callbackImpl _ ast f = STM32FreeRTOSCallback $ \ h t ->
    let p = proc (callbackProcName ast (AST.handler_name h) t) $ \ r -> body $ noReturn $ f r
    in (p, incl p)

  emitterImpl _ _ [] = (Emitter $ const $ return (), STM32FreeRTOSEmitter Nothing)
  emitterImpl _ ast handlers =
    ( Emitter $ call_ $ trampolineProc ast $ const $ return ()
    , STM32FreeRTOSEmitter $ Just $ \ mon thd -> emitterCode ast thd [ fst $ h mon thd | STM32FreeRTOSHandler _ h <- handlers ]
    )

  handlerImpl _ ast emitters callbacks = STM32FreeRTOSHandler ast $ \ mon thd ->
    let ems = [ e mon thd | STM32FreeRTOSEmitter (Just e) <- emitters ]
        (cbs, cbdefs) = unzip [ c ast thd | STM32FreeRTOSCallback c <- callbacks ]
        runner = handlerProc cbs ems thd mon ast
    in (runner, ThreadCode
      { threadcode_user = sequence_ cbdefs
      , threadcode_emitter = mapM_ emittercode_user ems
      , threadcode_gen = mapM_ emittercode_gen ems >> private (incl runner)
      })

  monitorImpl _ ast handlers moddef = STM32FreeRTOSMonitor $ \ twr -> STM32FreeRTOSOutput
    { compatoutput_threads = Map.fromListWith mappend
        [ (thd, snd $ h ast thd)
        -- handlers are reversed to match old output for convenient diffs
        | SomeHandler (STM32FreeRTOSHandler hast h) <- reverse handlers
        , thd <- AST.handlerThreads twr hast
        ]
    , compatoutput_monitors = Map.singleton ast moddef
    }

  towerImpl _ ast monitors = case mconcat monitors of STM32FreeRTOSMonitor f -> f ast

instance Monoid (TowerBackendOutput STM32FreeRTOSBackend) where
  mempty = STM32FreeRTOSOutput mempty mempty
  mappend a b = STM32FreeRTOSOutput
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

emitterCodeTD :: AST.Emitter 
              -> AST.Thread
              -> [IAST.Proc]
              -> Maybe EmitterCode
emitterCodeTD _ast _thr [] = Nothing
emitterCodeTD ast thr sinks = Just $ EmitterCode
  { emittercode_init = store (addrOf messageCount) 0
  , emittercode_deliver = do
      mc <- deref (addrOf messageCount)
      forM_ (zip messages [0..]) $ \ (m, index) ->
        I.when (fromInteger index <? mc) $
          forM_ sinks $ \ p ->
            let sym = (IAST.NameSym (IAST.procSym p)) in
            let param = TIAST.Typed (emitter_type) $ IAST.ExpAddrOfGlobal (IAST.areaSym m) in
            Mon.emit (IAST.Call (IAST.procRetTy p) Nothing sym [param])

  , emittercode_user = do
      incltrampolineprivate
  , emittercode_gen = do
      incleproc
      mapM_ (\a -> put (mempty { IAST.modAreas = Mod.visAcc Mod.Private a })) messages
      private $ defMemArea messageCount
  }
  where
  emitter_type :: TIAST.Type
  emitter_type = TIAST.tType $ head $ IAST.procArgs $ head sinks
  emitter_type_unconst :: TIAST.Type
  emitter_type_unconst = 
    let (TIAST.TyConstRef tt) = emitter_type in
    TIAST.TyRef tt 
  emitter_type_unconst_unref :: TIAST.Type
  emitter_type_unconst_unref = 
    let (TIAST.TyConstRef tt) = emitter_type in tt 

  max_messages = AST.emitter_bound ast - 1
  messageCount :: MemArea ('Stored Uint32)
  messageCount = area (e_per_thread "message_count") Nothing
  
  messages :: [IAST.Area]
  messages = [makeArea (e_per_thread $ "message_" ++ show d) False emitter_type_unconst_unref IAST.zeroInit | d <- [(0::Integer)..max_messages] ]

  messageAt mc = foldl aux dflt (zip messages [(0::Integer)..])
    where
    dflt = IAST.ExpAddrOfGlobal $ IAST.areaSym (messages !! 0) -- Should be impossible.
    aux basecase (msg, midx) = 
      IAST.ExpOp IAST.ExpCond 
        [booleanCond,IAST.ExpAddrOfGlobal $ IAST.areaSym $ msg,basecase]
      where
        booleanCond = IAST.ExpOp (IAST.ExpEq (TIAST.TyWord TIAST.Word32)) [fromIntegral midx, IAST.ExpVar mc]
  trampoline :: IAST.Proc
  trampoline = 
    IAST.Proc { IAST.procSym      = (emitterProcName ast)
              , IAST.procRetTy    = TIAST.TyVoid
              , IAST.procArgs     = [TIAST.Typed emitter_type var]
              , IAST.procBody     = [IAST.Call TIAST.TyVoid Nothing (IAST.NameSym $ e_per_thread "emit") [TIAST.Typed emitter_type $ IAST.ExpVar var]]
              , IAST.procRequires = []
              , IAST.procEnsures  = []
              }
    where 
    (var,_) = genVar initialClosure

  incltrampolineprivate = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Private trampoline })
  incleproc = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Public eproc })

  eproc :: IAST.Proc
  eproc = 
    IAST.Proc { IAST.procSym      = (e_per_thread "emit")
              , IAST.procRetTy    = TIAST.TyVoid
              , IAST.procArgs     = [TIAST.Typed emitter_type var]
              , IAST.procBody     = eprocblock
              , IAST.procRequires = []
              , IAST.procEnsures  = []
              }
    where 
    (var,_) = genVar initialClosure
    eprocblock = 
      [IAST.Deref (TIAST.TyWord TIAST.Word32) mc (primAddrOf messageCount),
      IAST.IfTE (IAST.ExpOp (IAST.ExpLt True $ TIAST.TyWord TIAST.Word32) [IAST.ExpVar mc, IAST.ExpLit $ IAST.LitInteger $ fromInteger max_messages]) 
        [IAST.Store (TIAST.TyWord TIAST.Word32) (primAddrOf messageCount) (IAST.ExpOp IAST.ExpAdd [IAST.ExpVar mc, IAST.ExpLit $ IAST.LitInteger $ (1::Integer)]),
        IAST.Assign (emitter_type_unconst) r (messageAt mc),
        IAST.RefCopy (emitter_type_unconst) (IAST.ExpVar r) (IAST.ExpVar var)] 
        [] --nothing else
      ]
      where
      mc=IAST.VarName ("deref"++ show (0::Integer))
      r=IAST.VarName ("let"++ show (1::Integer))


  e_per_thread suffix =
    emitterProcName ast ++ "_" ++ AST.threadName thr ++ "_" ++ suffix

callbackImplTD :: Unique -> IAST.Proc -> AST.Handler -> AST.Thread -> (IAST.Proc, ModuleDef)
callbackImplTD ast f = \ h t -> 
  let p = f {IAST.procSym = (callbackProcName ast (AST.handler_name h) t)} in
  let inclp = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Public p }) in
  (p, inclp)

emitterImplTD :: AST.Tower -> AST.Emitter -> AST.Monitor -> AST.Thread -> Maybe EmitterCode
emitterImplTD tow ast =
  let handlers = map (handlerImplTD tow) $ subscribedHandlers in
    \ mon thd -> emitterCodeTD ast thd [ fst $ h mon thd | h <- handlers ]

    where
    subscribedHandlers = filter (\x -> isListening $ AST.handler_chan x) allHandlers
    -- dont know why it works

    allHandlers = concat $ map (AST.monitor_handlers) (AST.tower_monitors tow)

    isListening (AST.ChanSync sc) = sc == (AST.emitter_chan ast)
    isListening _ = False

handlerProcTD :: [IAST.Proc]
              -> [EmitterCode]
              -> AST.Thread -> AST.Monitor -> AST.Handler
              -> IAST.Proc
handlerProcTD [] _emitters _t _m h = error $ "Handler with no callback" ++ (AST.handlerName h)
handlerProcTD callbacks emitters t m h =
  IAST.Proc { IAST.procSym      = (handlerProcName h t)
            , IAST.procRetTy    = TIAST.TyVoid
            , IAST.procArgs     = [TIAST.Typed (TIAST.tType $ head $ IAST.procArgs $ head callbacks) var]
            , IAST.procBody     = blocBody
            , IAST.procRequires = blocReq
            , IAST.procEnsures  = blocEns
            }
  where 
    (var,_) = genVar initialClosure -- initial closure is ok until we have one argument per function
    emitterscodeinit = snd $ Mon.primRunIvory $ mapM_ emittercode_init emitters
    monitorlockproc = snd $ Mon.primRunIvory $ monitorLockProc m h
    monitorunlockproc = snd $ Mon.primRunIvory $ monitorUnlockProc m h
    emittersdeliver = snd $ Mon.primRunIvory $ mapM_ emittercode_deliver emitters
    blocReq = Mon.blockRequires emitterscodeinit ++
      (Mon.blockRequires monitorlockproc) ++
      (Mon.blockRequires monitorunlockproc) ++
      (Mon.blockRequires emittersdeliver)
    blocEns = Mon.blockEnsures emitterscodeinit ++
      (Mon.blockEnsures monitorlockproc) ++
      (Mon.blockEnsures monitorunlockproc) ++
      (Mon.blockEnsures emittersdeliver)
    blocBody = 
      [IAST.Comment $ IAST.UserComment "init emitters"] ++ 
      (Mon.blockStmts $ emitterscodeinit) ++ 
      [IAST.Comment $ IAST.UserComment "take monitor lock(s)"] ++
      (Mon.blockStmts $ monitorlockproc) ++       
      [IAST.Comment $ IAST.UserComment "run callbacks"] ++
      map (\ cb -> (IAST.Call (IAST.procRetTy cb) Nothing (IAST.NameSym $ IAST.procSym cb) [TIAST.Typed (TIAST.tType $ head $ IAST.procArgs $ head callbacks) $ IAST.ExpVar var])) callbacks ++
      [IAST.Comment $ IAST.UserComment "release monitor lock(s)"] ++
      (Mon.blockStmts $ monitorunlockproc) ++ 
      [IAST.Comment $ IAST.UserComment "deliver emitters"] ++
      (Mon.blockStmts $ emittersdeliver)
      

handlerImplTD :: AST.Tower -> AST.Handler -> AST.Monitor -> AST.Thread -> (IAST.Proc, ThreadCode)
handlerImplTD tow ast = \ mon thd ->
  let emitters::([AST.Monitor -> AST.Thread -> Maybe EmitterCode]) = map (emitterImplTD tow) $ AST.handler_emitters ast in
  let callbacks::([AST.Handler -> AST.Thread -> (IAST.Proc, ModuleDef)]) = map (\(x,y) -> callbackImplTD x y) (zip (AST.handler_callbacks ast) (AST.handler_callbacksAST ast)) in
  let ems2 = [ e mon thd | e <- emitters ]
      ems = [e | Just e <- ems2]
      (cbs, cbdefs) = unzip [ c ast thd | c <- callbacks ]
      runner = handlerProcTD cbs ems thd mon ast
  in
  let inclrunner = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Private runner })
  in (runner, ThreadCode
    { threadcode_user = sequence_ cbdefs
    , threadcode_emitter = mapM_ emittercode_user ems
    , threadcode_gen = mapM_ emittercode_gen ems >> (inclrunner)
    })

monitorImplTD :: AST.Tower -> AST.Monitor -> TowerBackendMonitor STM32FreeRTOSBackend
monitorImplTD tow ast = 
  let (moddef::ModuleDef) = put $ AST.monitor_moduledef ast in
  STM32FreeRTOSMonitor $ \ twr -> STM32FreeRTOSOutput
    { compatoutput_threads = Map.fromListWith mappend
        [ (thd, snd $ handlerImplTD tow hast ast thd)
        -- handlers are reversed to match old output for convenient diffs
        | hast <- reverse $ AST.monitor_handlers ast
        , thd <- AST.handlerThreads twr hast
        ]
    , compatoutput_monitors = Map.singleton ast moddef
    }

--------

compileTowerSTM32FreeRTOS :: (e -> STM32Config) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerSTM32FreeRTOS fromEnv getEnv twr = compileTowerSTM32FreeRTOSWithOpts fromEnv getEnv twr []



compileTowerSTM32FreeRTOSWithOpts :: (e -> STM32Config) -> (TOpts -> IO e) -> Tower e () -> [AST.Tower -> IO AST.Tower] -> IO ()
compileTowerSTM32FreeRTOSWithOpts fromEnv getEnv twr optslist = do
  (copts, topts) <- towerGetOpts
  env <- getEnv topts

  let cfg = fromEnv env
  (ast, _monitors, deps, sigs) <- runTower compatBackend twr env optslist
  --let o = towerImpl compatBackend (ast) monitors
  let o = towerImpl compatBackend (ast) (map (monitorImplTD ast) $ AST.tower_monitors ast)
  let mods = dependencies_modules deps
          ++ threadModules deps sigs (thread_codes o) ast
          ++ monitorModules deps (Map.toList (compatoutput_monitors o))
          ++ stm32Modules cfg ast

      givenArtifacts = dependencies_artifacts deps
      as = stm32Artifacts cfg ast mods givenArtifacts
  runCompiler mods (as ++ givenArtifacts) copts 
  where
  compatBackend = STM32FreeRTOSBackend

  thread_codes o = Map.toList
                 $ Map.insertWith mappend (AST.InitThread AST.Init) mempty
                 $ compatoutput_threads o


parseTowerSTM32FreeRTOS :: (e -> STM32Config) -> (TOpts -> IO e) -> Tower e () -> IO AST.Tower
parseTowerSTM32FreeRTOS aa getEnv twr = parseTowerSTM32FreeRTOSWithOpts aa getEnv twr []

parseTowerSTM32FreeRTOSWithOpts :: (e -> STM32Config) -> (TOpts -> IO e) -> Tower e () -> [AST.Tower -> IO AST.Tower] -> IO AST.Tower
parseTowerSTM32FreeRTOSWithOpts _ getEnv twr optslist = do
  (_, topts) <- towerGetOpts
  env <- getEnv topts
  (ast, _, _, _) <- runTower compatBackend twr env optslist
  return ast
  where
  compatBackend = STM32FreeRTOSBackend

                  
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
stm32Artifacts conf ast ms gcas = as --(systemArtifacts ast ms) ++ as
  --NOTA : systemArtifacts : "debug_mods.txt" "debug_ast.txt" "out.dot"
  where
  as = [ STM32.makefile conf makeobjs ] ++ STM32.artifacts conf
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

