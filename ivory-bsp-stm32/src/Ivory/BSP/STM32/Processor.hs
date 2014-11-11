
module Ivory.BSP.STM32.Processor
  ( Processor(..)
  ) where

import Data.Char (toUpper)
import Tower.Config

data Processor
  = STM32F405
  | STM32F427
  deriving (Eq, Show)

instance Configurable Processor where
  fromConfig v = case fmap (map toUpper) (fromConfig v) of
    Just "STM32F405" -> Just STM32F405
    Just "STM32F427" -> Just STM32F427
    _ -> Nothing
