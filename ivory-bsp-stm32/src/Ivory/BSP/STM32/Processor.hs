
module Ivory.BSP.STM32.Processor
  ( Processor(..)
  , processorParser
  ) where

import Data.Char (toUpper)
import Ivory.Tower.Config


data Processor
  = STM32F405
  | STM32F427
  deriving (Eq, Show)

processorParser :: ConfigParser Processor
processorParser = string >>= \v ->
  case map toUpper v of
    "STM32F405" -> return STM32F405
    "STM32F427" -> return STM32F427
    _ -> fail ("expected Processor, got " ++ v)

