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
import Data.STM32

makefile :: MCU -> [FilePath] -> Located Artifact
makefile mcu userobjs = Root $ artifactString "Makefile" $ unlines
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
  , "  -mthumb \\"
  , "  -mcpu=" ++ cpu mcucore ++ " \\"
  , "  -mfpu=" ++ fpu mcucore ++ " \\"
  , maybe " \\" (\x -> "  -mfloat-abi=" ++ x ++ " \\") (floatabi mcucore)
  , "  -DIVORY_TEST \\"
  , "  -DIVORY_USER_ASSERT_HOOK \\"
  , "  -I."
  , ""
  , "LDFLAGS := \\"
  , "  -mlittle-endian \\"
  , "  -mthumb \\"
  , "  -mcpu=" ++ cpu mcucore ++ " \\"
  , "  -mfpu=" ++ fpu mcucore ++ " \\"
  , maybe " \\" (\x -> "  -mfloat-abi=" ++ x) (floatabi mcucore)
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
  mcucore = core $ mcuFamily mcu
  objects = userobjs ++  ["stm32_freertos_init.o", "vector_table.o", "stm32_freertos_user_assert.o"]

artifacts :: NamedMCU -> [Located Artifact]
artifacts nmcu@(_name, mcu) =
  [ vector_table nmcu
  ] ++ init_artifacts ++ aux
  where
  init_artifacts =
    [ Src  $ artifactCabalFile P.getDataDir "support/stm32_freertos_init.c"
    , Incl $ artifactCabalFile P.getDataDir "support/stm32_freertos_init.h"
    , Src  $ artifactCabalFile P.getDataDir "support/stm32_freertos_user_assert.c"
    ]

  aux = [ mk_lds "linker_script.lds" 0 ]

  mk_lds name bl_offset = Root $ linker_script name nmcu bl_offset reset_handler

