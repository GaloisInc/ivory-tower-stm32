
module Ivory.OS.FreeRTOS.Tower.STM32.Build
  ( makefile
  , artifacts
  ) where

import qualified Data.List as L

import qualified Paths_tower_freertos_stm32 as P
import Ivory.Artifact
import Ivory.Artifact.Template
import Ivory.BSP.STM32.VectorTable

import Ivory.OS.FreeRTOS.Tower.STM32.Config

makefile :: [FilePath] -> Artifact
makefile userobjs = artifactString "Makefile" $ unlines
  [ "CC := arm-none-eabi-gcc"
  , "CFLAGS := \\"
  , "  -g3 -Wall -Werror -O2 \\"
  , "  -std=gnu99 \\"
  , "  -Wno-parentheses \\"
  , "  -Wno-unused-function \\"
  , "  -Wno-unused-variable \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\"
  , "  -DIVORY_TEST \\"
  , "  -I."
  , ""
  , "LDFLAGS := \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16"
  , ""
  , "LDSCRIPT := linker_script.lds"
  , ""
  , "OBJDIR := obj"
  , "OBJS := $(addprefix $(OBJDIR)/," ++ (L.intercalate " " objects) ++ ")"
  , ""
  , "default: $(OBJDIR) $(OBJS) image"
  , ""
  , "image: $(OBJS)"
  , "\t$(CC) -o $@ $(LDFLAGS) -Wl,--script=$(LDSCRIPT) -Wl,-Map=$@.map $(OBJS)"
  , ""
  , "$(OBJDIR)/%.o : %.c"
  , "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR)/%.o : %.s"
  , "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR):"
  , "\tmkdir -p $(OBJDIR)"
  , ""
  , "clean:"
  , "\t-rm -rf obj"
  , "\t-rm image"
  , "\t-rm image.map"
  , ""
  ]
  where
  objects = userobjs ++  ["stm32_freertos_init.o", "vector_table.o"]

artifacts :: STM32Config -> [Artifact]
artifacts conf = [ vector_table (stm32config_processor conf)
                 , linker_script conf
                 ] ++ init_artifacts
  where
  init_artifacts = map (artifactCabalFile P.getDataDir)
                ["support/stm32_freertos_init.c", "support/stm32_freertos_init.h"]

linker_script :: STM32Config -> Artifact
linker_script _conf = artifactCabalFileTemplate P.getDataDir path attrs
  where path = "support/linker_script.lds.template"
        -- XXX USE CONF:
        attrs = [("flash_origin",  "0x08000000") -- add 0x4000 for bootloader
                ,("flash_length",  "1024K")      -- 1008K for bootloader
                ,("sram_length",   "128K")
                ,("ccsram_length", "64K")
                ,("estack",        "0x20020000") -- sram end, given sram_start = 0x20000000
                ,("reset_handler", reset_handler)
                ]
