{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.BSP.STM32.VectorTable
  ( vector_table
  , reset_handler
  ) where

import qualified Paths_ivory_bsp_stm32 as P
import Ivory.Artifact
import Ivory.Artifact.Template
import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Processor

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified Ivory.BSP.STM32F427.Interrupt as F427

reset_handler :: String
reset_handler = exceptionHandlerName Reset

vector_table :: Processor -> Located Artifact
vector_table processor =
  Src $ artifactCabalFileTemplate P.getDataDir fname as
  where
  fname = "support/vector_table.s.template"
  as = case processor of
    STM32F405 -> attrs F405.WWDG
    STM32F427 -> attrs F427.WWDG

attrs :: forall i . (STM32Interrupt i) => i -> [(String, String)]
attrs i = [("entries", entries)
          ,("weakdefs", weakdefs)
          ,("reset_handler", reset_handler)
          ]
  where
  itable :: [Maybe i]
  itable = interruptTable i
  entries = unlines $
    map (entry . (fmap exceptionHandlerName)) exceptionTable ++
    map (entry . (fmap interruptHandlerName)) itable
  weakdefs = unlines $
    map (weakdef . (fmap exceptionHandlerName)) (drop 1 exceptionTable) ++
    map (weakdef . (fmap interruptHandlerName)) itable

  entry (Just e) = "\t.word " ++ e
  entry Nothing  = "\t.word 0"
  weakdef (Just handler) = "\t.weak " ++ handler ++
    "\n\t.thumb_set " ++ handler ++ ",defaultExceptionHandler\n"
  weakdef Nothing = ""
