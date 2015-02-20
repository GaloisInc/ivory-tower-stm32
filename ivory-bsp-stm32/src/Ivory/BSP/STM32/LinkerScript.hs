
module Ivory.BSP.STM32.LinkerScript
  ( linker_script
  ) where

import Ivory.Artifact
import Ivory.Artifact.Template
import qualified Paths_ivory_bsp_stm32 as P
import Ivory.BSP.STM32.Processor

linker_script :: Processor -> Integer -> String -> Artifact
linker_script p bl_offset reset_handler =
  artifactCabalFileTemplate P.getDataDir path (attrs p)
  where
  path = "support/linker_script.lds.template"
  -- Yeah yeah i know that these parts are available in variations with differnt
  -- flash lengths but i dont really want to encode that kind of complexity
  -- until its actually necessary
  attrs STM32F405 =
    [("flash_origin" , show $ 0x08000000 + bl_offset)
    ,("flash_length" , show $ (kbytes 1024) - bl_offset)
    ,("sram_length"  , show $ sram_length)
    ,("ccsram_length", show $ ccsram_length)
    ,("estack"       , show $ 0x20000000 + sram_length)
    ,("reset_handler", reset_handler)
    ]
  attrs STM32F427 =
    [("flash_origin" , show $ 0x08000000 + bl_offset)
    ,("flash_length" , show $ (kbytes 2048) - bl_offset)
    ,("sram_length"  , show $ sram_length)
    ,("ccsram_length", show $ ccsram_length)
    ,("estack"       , show $ 0x20000000 + sram_length)
    ,("reset_handler", reset_handler)
    ]

  sram_length :: Integer
  sram_length = kbytes 128
  ccsram_length :: Integer
  ccsram_length = kbytes 64

  kbytes n = 1024 * n
