{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.OS.FreeRTOS.TaskArg where

import Ivory.Language

-- Taskarg is a Struct used to stand in for C's void*. It should
-- only be used as an argument to a TaskProc, and never dereferenced.

[ivory|abstract struct taskarg "freertos_task_wrapper.h"|]
