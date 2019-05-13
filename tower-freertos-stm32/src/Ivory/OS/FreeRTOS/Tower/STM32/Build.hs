{-# LANGUAGE RecordWildCards #-}

module Ivory.OS.FreeRTOS.Tower.STM32.Build
  ( makefile
  , artifacts
  ) where

import qualified Data.List as L

import qualified Paths_tower_freertos_stm32 as P
import Ivory.Artifact
import Ivory.BSP.STM32.VectorTable
import Ivory.BSP.STM32.LinkerScript
import Ivory.BSP.STM32.MCU

makefile :: [FilePath] -> Located Artifact
makefile userobjs = Root $ artifactString "Makefile" $ unlines
  [ "UNAME_S := $(shell uname -s)"
  , "ifeq ($(UNAME_S),Linux)"
  , "UPLOAD_PORT?=/dev/serial/by-id/usb-3D_Robotics*"
  , "endif"
  , "CC := arm-none-eabi-gcc"
  , "OBJCOPY := arm-none-eabi-objcopy"
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
  , "  -DIVORY_USER_ASSERT_HOOK \\"
  , "  -I."
  , ""
  , "LDFLAGS := \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16"
  , "LDLIBS := -lm"
  , ""
  , "OBJDIR := obj"
  , "OBJS := $(addprefix $(OBJDIR)/," ++ (L.intercalate " " objects) ++ ")"
  , ""
  , "default: $(OBJDIR) $(OBJS) image"
  , ""
  , "image: $(OBJS)"
  , "\t$(CC) -o $@ $(LDFLAGS) -Wl,--script=linker_script.lds -Wl,-Map=$@.map $(OBJS) $(LDLIBS)"
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
  objects = userobjs ++  ["stm32_freertos_init.o", "vector_table.o", "stm32_freertos_user_assert.o"]

artifacts :: MCU -> [Located Artifact]
artifacts mcu =
  [ vector_table (mcuFamily mcu)
  ] ++ init_artifacts ++ aux
  where
  init_artifacts =
    [ Src  $ artifactCabalFile P.getDataDir "support/stm32_freertos_init.c"
    , Incl $ artifactCabalFile P.getDataDir "support/stm32_freertos_init.h"
    , Src  $ artifactCabalFile P.getDataDir "support/stm32_freertos_user_assert.c"
    ]

  aux = [ mk_lds "linker_script.lds" 0 ]

  mk_lds name bl_offset = Root $ linker_script name mcu bl_offset reset_handler

