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

import Ivory.OS.FreeRTOS.Tower.STM32.Config

makefile :: STM32Config -> [FilePath] -> Artifact
makefile STM32Config{..} userobjs = artifactString "Makefile" $ unlines
  [ "CC := arm-none-eabi-gcc"
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
  , "default: $(OBJDIR) $(OBJS) image" ++ bootloader_default_targets
  , ""
  , bootloader_targets
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
  bootloader_default_targets = case stm32config_bootloader of
    NoBootloader -> ""
    PX4ProjectBootloader _ -> " image.px4"
  bootloader_targets = case stm32config_bootloader of
    NoBootloader -> ""
    PX4ProjectBootloader px4vers -> unlines
      [ "image.px4: image.bin"
      , "\tpython px_mkfw.py --prototype=" ++ px4vers_prototype px4vers 
            ++ "  --image=$< > $@"
      , ""
      , "image.bin: image"
      , "\t$(OBJCOPY) -O binary $< $@"
      , ""
      , "upload: image.px4"
      , "\t@echo \"*** User expected to set UPLOAD_PORT environment variable ***\""
      , "\tpython px_uploader.py --port=$(UPLOAD_PORT) $<"
      , ""
      ]

artifacts :: STM32Config -> [Artifact]
artifacts STM32Config{..} =
  [ vector_table stm32config_processor
  , lds
  ] ++ init_artifacts ++ bl_artifacts
  where
  init_artifacts = map (artifactCabalFile P.getDataDir)
    ["support/stm32_freertos_init.c", "support/stm32_freertos_init.h"]

  -- Above makefile assumes this will be called "linker_script.lds"
  lds :: Artifact
  lds = linker_script stm32config_processor bl_offset reset_handler
    where
    bl_offset = case stm32config_bootloader of
      PX4ProjectBootloader _ -> 0x4000
      NoBootloader           -> 0

  bl_artifacts = case stm32config_bootloader of
    NoBootloader -> []
    PX4ProjectBootloader px4vers ->
      map (artifactCabalFile P.getDataDir)
          (px_python ++ ["support/" ++ px4vers_prototype px4vers])
  px_python = [ "support/px_mkfw.py", "support/px_uploader.py"] 

px4vers_prototype :: PX4Version -> String
px4vers_prototype PX4FMU_v1 = "px4fmu-v1.prototype"
px4vers_prototype PX4FMU_v2 = "px4fmu-v2.prototype"
