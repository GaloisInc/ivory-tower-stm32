{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.OS.FreeRTOS.Types where

import Ivory.Language

-- Taskarg is a Struct used to stand in for C's void*. It should
-- only be used as an argument to a TaskProc, and never dereferenced.

-- binary_semaphore and mutex are wrappers for the public FreeRTOS types.

[ivory|
abstract struct taskarg            "freertos_task_wrapper.h"
abstract struct binary_semaphore   "freertos_semaphore_wrapper.h"
abstract struct mutex              "freertos_mutex_wrapper.h"
|]
