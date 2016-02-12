{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.OS.FreeRTOS.Mutex where

import Prelude hiding (take)
import Ivory.Language

import Ivory.OS.FreeRTOS.Types ()

type Mutex = 'Struct "mutex"
type MutexHandle = Ref 'Global Mutex

mutexWrapperHeader :: String
mutexWrapperHeader = "freertos_mutex_wrapper.h"

moddef :: ModuleDef
moddef = do
  incl create
  incl take
  incl give

create :: Def ('[ MutexHandle ] ':-> ())
create =
  importProc "ivory_freertos_mutex_create" mutexWrapperHeader

take :: Def ('[ MutexHandle ] ':-> ())
take = importProc "ivory_freertos_mutex_takeblocking" mutexWrapperHeader

give :: Def('[ MutexHandle ] ':-> ())
give = importProc "ivory_freertos_mutex_give" mutexWrapperHeader

