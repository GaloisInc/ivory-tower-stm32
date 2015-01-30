{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32F405.Interrupt
  -- from Types:
  ( F405.Interrupt(..)
  , F405.interruptIRQn
  , F405.interruptHandlerName
  , F405.interruptTable
  ) where

import qualified Ivory.BSP.STM32F405.Interrupt.Types as F405
import Ivory.BSP.STM32.Interrupt

