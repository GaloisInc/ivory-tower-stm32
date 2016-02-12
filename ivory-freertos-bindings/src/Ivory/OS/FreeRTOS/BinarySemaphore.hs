{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.OS.FreeRTOS.BinarySemaphore where

import Prelude hiding (take)
import Ivory.Language

import Ivory.OS.FreeRTOS.Types ()

type BinarySemaphore = 'Struct "binary_semaphore"
type BinarySemaphoreHandle = Ref 'Global BinarySemaphore

semaphoreWrapperHeader :: String
semaphoreWrapperHeader = "freertos_semaphore_wrapper.h"

moddef :: ModuleDef
moddef = do
  incl create
  incl take
  incl give
  incl giveFromISR

create :: Def ('[ BinarySemaphoreHandle ] ':-> ())
create = importProc "ivory_freertos_binary_semaphore_create" semaphoreWrapperHeader

take :: Def ('[ BinarySemaphoreHandle ] ':-> ())
take = importProc "ivory_freertos_binary_semaphore_takeblocking" semaphoreWrapperHeader

give :: Def('[ BinarySemaphoreHandle ] ':-> ())
give = importProc "ivory_freertos_binary_semaphore_give" semaphoreWrapperHeader

giveFromISR :: Def('[ BinarySemaphoreHandle ] ':-> ())
giveFromISR = importProc "ivory_freertos_binary_semaphore_give_from_isr" semaphoreWrapperHeader

