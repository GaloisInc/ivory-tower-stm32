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
import Ivory.BSP.STM32.Config

makefile :: STM32Config -> [FilePath] -> Located Artifact
makefile STM32Config{..} userobjs = Root $ artifactString "Makefile" $ unlines
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
  , "default: $(OBJDIR) $(OBJS) image" ++ bootloader_default_targets
  , ""
  , bootloader_targets
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
  bootloader_default_targets = case stm32config_px4version of
    Nothing -> ""
    Just _ -> " image.px4"
  bootloader_targets = case stm32config_px4version of
    Nothing -> ""
    Just px4vers -> unlines
      [ "image.px4: bl_image.bin"
      , "\tpython px_mkfw.py --prototype=" ++ px4vers_prototype px4vers 
            ++ "  --image=$< > $@"
      , ""
      , "bl_image.bin: bl_image"
      , "\t$(OBJCOPY) -O binary $< $@"
      , ""
      , "bl_image: $(OBJS)"
      , "\t$(CC) -o $@ $(LDFLAGS) -Wl,--script=bl_linker_script.lds -Wl,-Map=$@.map $(OBJS) $(LDLIBS)"
      , ""
      , "upload: image.px4"
      , "ifndef UPLOAD_PORT"
      , "\t@echo \"*** User expected to set UPLOAD_PORT environment variable, exiting. ***\""
      , "else"
      , "\t@echo \"*** Uploading ***\""
      , "\tpython px_uploader.py --port=$(UPLOAD_PORT) $<"
      , "endif"
      , ""
      ]

artifacts :: STM32Config -> [Located Artifact]
artifacts STM32Config{..} =
  [ vector_table stm32config_processor
  ] ++ init_artifacts ++ aux stm32config_px4version
  where
  init_artifacts =
    [ Src  $ artifactCabalFile P.getDataDir "support/stm32_freertos_init.c"
    , Incl $ artifactCabalFile P.getDataDir "support/stm32_freertos_init.h"
    , Src  $ artifactCabalFile P.getDataDir "support/stm32_freertos_user_assert.c"
    ]

  aux Nothing = [ mk_lds "linker_script.lds" 0 ]
  aux (Just px4vers) =
    [ mk_lds "linker_script.lds" 0
    , mk_lds "bl_linker_script.lds" 0x4000
    ] ++ map (Root . artifactCabalFile P.getDataDir)
              (px_python ++ ["support/" ++ px4vers_prototype px4vers])

  mk_lds name bl_offset = Root $ linker_script name stm32config_processor bl_offset reset_handler
  px_python = [ "support/px_mkfw.py", "support/px_uploader.py"] 

px4vers_prototype :: PX4Version -> String
px4vers_prototype PX4FMU_v1 = "px4fmu-v1.prototype"
px4vers_prototype PX4FMU_v2 = "px4fmu-v2.prototype"
