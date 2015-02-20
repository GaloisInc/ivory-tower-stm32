
module Ivory.OS.FreeRTOS.Tower.STM32.Build
  ( makefile
  , artifacts
  ) where

import qualified Data.List as L

import qualified Paths_tower_freertos_stm32 as P
import Ivory.Artifact
import Ivory.BSP.STM32.VectorTable
import Ivory.BSP.STM32.LinkerScript

import Ivory.OS.FreeRTOS.Tower.STM32.Config

makefile :: [FilePath] -> Artifact
makefile userobjs = artifactString "Makefile" $ unlines
  [ "CC := arm-none-eabi-gcc"
  , "CFLAGS := -Os"
  , "TOWER_STM32_CFLAGS := \\"
  , "  -g3 -Wall -Werror \\"
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
  , "LDLIBS := -lm"
  , "LDSCRIPT := linker_script.lds"
  , ""
  , "OBJDIR := obj"
  , "OBJS := $(addprefix $(OBJDIR)/," ++ (L.intercalate " " objects) ++ ")"
  , ""
  , "default: $(OBJDIR) $(OBJS) image"
  , ""
  , "image: $(OBJS)"
  , "\t$(CC) -o $@ $(LDFLAGS) -Wl,--script=$(LDSCRIPT) -Wl,-Map=$@.map $(OBJS) $(LDLIBS)"
  , ""
  , "$(OBJDIR)/%.o : %.c"
  , "\t$(CC) $(TOWER_STM32_CFLAGS) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR)/%.o : %.s"
  , "\t$(CC) $(TOWER_STM32_CFLAGS) $(CFLAGS) -c -o $@ $<"
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
                 , lds conf
                 ] ++ init_artifacts
  where
  init_artifacts = map (artifactCabalFile P.getDataDir)
                ["support/stm32_freertos_init.c", "support/stm32_freertos_init.h"]

-- Above makefile assumes this will be called "linker_script.lds"
lds :: STM32Config -> Artifact
lds conf = linker_script (stm32config_processor conf) bl_offset reset_handler
  where
  bl_offset = case stm32config_bootloader conf of
    PX4ProjectBootloader -> 0x4000
    NoBootloader         -> 0

