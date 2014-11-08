{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.BSP.STM32.VectorTable
  ( vector_table
  , reset_handler
  ) where

import qualified Paths_ivory_bsp_stm32 as P
import Ivory.Language
import Ivory.Artifact
import Ivory.Artifact.Template
import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Processor

import qualified Ivory.BSP.STM32F405.Interrupt as F405

reset_handler :: String
reset_handler = exceptionHandlerName Reset

vector_table :: Processor -> Artifact
vector_table processor =
  artifactCabalFileTemplate P.getDataDir fname as
  where
  fname = "support/vector_table.s.template"
  as = case processor of
    STM32F405 -> attrs (Proxy :: Proxy F405.Interrupt)
    _ -> error ("vector_table: unsupported processor " ++ show processor)


attrs :: forall i . (STM32Interrupt i) => Proxy i -> [(String, String)]
attrs _ = [("entries", entries)
          ,("weakdefs", weakdefs)
          ,("reset_handler", reset_handler)
          ]
  where
  itable :: [Maybe i]
  itable = interruptTable
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
