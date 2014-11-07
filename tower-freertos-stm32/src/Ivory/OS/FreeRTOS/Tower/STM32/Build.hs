
module Ivory.OS.FreeRTOS.Tower.STM32.Build
  ( makefile
  , artifacts
  ) where

import qualified Data.List as L

import qualified Paths_tower_freertos_stm32 as P
import Ivory.Artifact
import Ivory.BSP.STM32.VectorTable

import Ivory.OS.FreeRTOS.Tower.STM32.Config

makefile :: [FilePath] -> Artifact
makefile userobjs = artifactString "Makefile" $ unlines
  [ "CC := arm-none-eabi-gcc"
  , "CFLAGS := \\"
  , "  -g3 -Wall -Werror -O2 \\"
  , "  -std=gnu99 \\"
  , "  -Wno-parenthesis -Wno-unused-variable \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\"
  , "  -DIVORY_TEST"
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
  objects = userobjs ++  ["stm32_init.o", "vector_table.o"]

artifacts :: Config -> [Artifact]
artifacts conf = [ vector_table (config_processor conf)
                 , linker_script conf
                 ] ++ init_artifacts
  where
  init_artifacts = map (artifactCabalFile P.getDataDir)
                ["support/stm32_init.c", "support/stm32_init.h"]

linker_script :: Config -> Artifact
linker_script = error "linker script is undefined"

--linker_script :: FilePath -- XXX FIXME
---linker_script = "support/linker_script.lds.template"

-- XXX many of these can be improved:
--    - stm32_ivory_init is actually generated as an ivory module by
--      ivory_bsp_stm32
--    - stm32f405_vectors is generated as an artifact by ivory_bsp_stm32
--    - some parts of stm32_init.c can be moved to ivory generated code
--    - the linker script can be generalized to a template, and filled in
--      by sone config that specifies the exact chip, whether there is a
--      bootloader, and so on

