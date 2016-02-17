module Ivory.OS.FreeRTOS.Config
  ( Config(..)
  , configHeader
  ) where

import Ivory.Artifact
import Ivory.Artifact.Template
import qualified Paths_ivory_freertos_bindings as P

data Config = Config
  { cpu_clock_hz        :: Integer
  , tick_rate_hz        :: Integer
  , max_priorities      :: Integer
  , minimal_stack_size  :: Integer
  , total_heap_size     :: Integer
  }

configHeader :: Config -> Located Artifact
configHeader c = Incl $ artifactCabalFileTemplate P.getDataDir loc attrs
  where
  loc = "freertos-sources/FreeRTOSConfig.h.template"
  attrs = [ ("cpu_clock_hz",       show (cpu_clock_hz c))
          , ("tick_rate_hz",       show (tick_rate_hz c))
          , ("max_priorities",     show (max_priorities c))
          , ("minimal_stack_size", show (minimal_stack_size c))
          , ("total_heap_size",    show (total_heap_size c))
          ]
