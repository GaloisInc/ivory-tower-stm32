
module Ivory.BSP.STM32.LinkerScript
  ( linker_script
  ) where

import Ivory.Artifact
import Ivory.Artifact.Template
import qualified Paths_ivory_bsp_stm32 as P
import Ivory.BSP.STM32.Processor

linker_script :: FilePath -> Processor -> Integer -> String -> Artifact
linker_script name p bl_offset reset_handler =
  artifactCabalFileTemplate' P.getDataDir path name (attrs p)
  where
  path = "support/linker_script.lds.template"
  -- Yeah yeah i know that these parts are available in variations with differnt
  -- flash lengths but i dont really want to encode that kind of complexity
  -- until its actually necessary
  attrs STM32F405 =
    [("flash_origin" , show $ 0x08000000 + bl_offset)
    ,("flash_length" , show $ (kbytes 1024) - bl_offset)
    ,("sram_length"  , show $ f405_sram_length)
    ,("ccsram_length", show $ ccsram_length)
    ,("estack"       , show $ 0x20000000 + f405_sram_length)
    ,("reset_handler", reset_handler)
    ]
  attrs STM32F427 =
    [("flash_origin" , show $ 0x08000000 + bl_offset)
    ,("flash_length" , show $ (kbytes 2048) - bl_offset)
    ,("sram_length"  , show $ f427_sram_length)
    ,("ccsram_length", show $ ccsram_length)
    ,("estack"       , show $ 0x20000000 + f427_sram_length)
    ,("reset_handler", reset_handler)
    ]

  f405_sram_length :: Integer
  f405_sram_length = sum [sram1_length, sram2_length]
  f427_sram_length :: Integer
  f427_sram_length = sum [sram1_length, sram2_length, sram3_length]
  sram1_length :: Integer
  sram1_length = kbytes 112
  sram2_length :: Integer
  sram2_length = kbytes 16
  sram3_length :: Integer
  sram3_length = kbytes 64
  ccsram_length :: Integer
  ccsram_length = kbytes 64

  kbytes n = 1024 * n
