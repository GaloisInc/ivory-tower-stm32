{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
-- Create eChronos build artifacts.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build.EChronos where

import qualified Paths_tower_echronos_stm32 as P

import Ivory.Artifact
import Ivory.Language
import Ivory.Tower
import Ivory.HW

import Tower.AADL.Config (AADLConfig(..), maybeFP)
import Tower.AADL.Build.Common

import Ivory.BSP.ARMv7M.SysTick
import Ivory.BSP.STM32.Config
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.ClockConfig.Init

--------------------------------------------------------------------------------
-- Ramses build

-- Ramses Makefile ------------------------------------------------------------
ramsesMakefile :: AADLConfig -> [MkStmt]
ramsesMakefile c =
  [ include    aadlFilesMk
  , "RAMSES_PATH" ?= maybeFP (configRamsesPath c)
  , "SMACCM_PATH" ?= "./"
  , export $"RAMSES_DIR" === "$(RAMSES_PATH)/ramses_resource"
  , export $"AADL2RTOS_CONFIG_DIR" === "$(RAMSES_PATH)/aadl2rtos_resource"
  , Target ".PHONY" ["all", "tower-clean"] []
  , Target ".tag.ramses" []
    ["java -jar $(RAMSES_PATH)/ramses.jar -g rtos -i $(AADL2RTOS_CONFIG_DIR) \
          \-o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$(AADL_LIST)"
    ,"touch .tag.ramses"
    ]
  , Target "tower-clean" []
    [ rm ".tag.ramses" ]
  ]
  where
  rm s = "-rm -rf " ++ s

--------------------------------------------------------------------------------
echronosMakefileName :: FilePath
echronosMakefileName = "echronos.mk"

echronosMakefile :: AADLConfig -> [MkStmt]
echronosMakefile c =
  [ "SHELL"        =: "/bin/bash"
  , "ROOT"         =: "$(shell pwd)"
  , "SRC"          =: "$(ROOT)/."
  , "EXE"          =: "image"
  , "UNAME_S"      =: "$(shell uname -s)\n\
    \ifeq ($(UNAME_S),Linux)\n\
    \UPLOAD_PORT?=/dev/serial/by-id/usb-3D_Robotics*\n\
    \endif"
  , "ARM_PATH"     ?= ""
  , "AS"           =: "$(ARM_PATH)arm-none-eabi-as -mthumb -g3 -mlittle-endian -mcpu=cortex-m4 \\\n\
             \      -mfloat-abi=hard -mfpu=fpv4-sp-d16 -I$(SRC)"
  , "CC"          =: "$(ARM_PATH)arm-none-eabi-gcc"
  , "CFLAGS"      =: "-Os -g3 -Wall -Werror              \\\n\
           \          -std=gnu99                         \\\n\
           \          -Wno-parentheses                   \\\n\
           \          -Wno-unused-function               \\\n\
           \          -Wno-unused-variable               \\\n\
           \          -Wno-main                          \\\n\
           \          -mlittle-endian                    \\\n\
           \          -mthumb -mcpu=cortex-m4            \\\n\
           \          -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\\n\
           \          -I$(SRC)                           \\\n\
           \          -I$(SRC)/" ++ configHdrDir c ++  " \\\n\
           \          -I$(SRC)/gen                       \\\n\
           \          -I$(SRC)/echronos_gen"
  , "LDSCRIPT"    =: "$(SRC)/echronos_gen/default.ld"
  , "LDFLAGS"     =: "-Wl,--script=$(LDSCRIPT)           \\\n\
          \           -nodefaultlibs                     \\\n\
          \           -mlittle-endian                    \\\n\
          \           -mthumb -mcpu=cortex-m4            \\\n\
          \           -mfloat-abi=hard -mfpu=fpv4-sp-d16"
  , "LDLIBS"      =: "-lm -lc -lnosys -lgcc"
  , "LD"          =: "$(ARM_PATH)arm-none-eabi-gcc"
  , "SOURCES_GCC" =: "$(wildcard $(SRC)/*.c)                    \\\n\
      \               $(wildcard $(SRC)/gen/*.c)                \\\n\
      \               $(wildcard $(SRC)/echronos_gen/*.c)"
  , "SOURCES_AS"  =: "$(wildcard $(SRC)*.s)                     \\\n\
       \              $(wildcard $(SRC)/gen/*.s)                \\\n\
       \              $(wildcard $(SRC)/echronos_gen/*.s)"
  , "OBJECTS_GCC" =: "$(SOURCES_GCC:.c=.o)"
  , "OBJECTS_AS"  =: "$(SOURCES_AS:.s=.o)"
  , "VPATH"       =: "$(SRC)"
  , "OBJCOPY"     =: "$(ARM_PATH)arm-none-eabi-objcopy"
  , Target "$(EXE)" ["$(OBJECTS_GCC)", "$(OBJECTS_AS)"]
    ["@echo building executable from assembly files: $(OBJECTS_AS) and .c files: $(OBJECTS_GCC)"
    ,"@echo linking executable"
    ,"$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS)"
    ]
  , Target "bl_image.bin" ["$(EXE)"]
    ["$(OBJCOPY) -O binary $< $@"]
  , Target "image.px4" ["bl_image.bin"]
    ["python px_mkfw.py --prototype=px4fmu-v2.prototype  --image=$< > $@"]
  , IfNDef "UPLOAD_PORT" 
      [Target "upload" ["image.px4"]
        ["@echo \"*** User expected to set UPLOAD_PORT environment variable, exiting. ***\""]
      ]
      [Target "upload" ["image.px4"]
        ["@echo \"*** Uploading ***\""
        , "python px_uploader.py --port=$(UPLOAD_PORT) $<"]
      ]
  , Target ".PHONY" ["echronos-clean"] []
  , Target "echronos-clean" []
    ["@echo remove all the object files"
    ,"rm -f *.o"
    ,"@echo remove the executable, if any"
    ,"rm -f $(SYS)"]
  ]

makefile :: AADLConfig -> [MkStmt]
makefile c =
  [ Comment "Make sure 'all' is the first target by putting it before any includes"
  , Target "all" ["generate"]
    [ "# This sub-make is here to deal with bl_image.bin depending on"
    , "# files that have to be generated first. This requires us"
    , "# to do the build in two phases."
    , "make bl_image.bin image.px4" ]
  , includeOpt ramsesMakefileName
  , Comment "We assume ECHRONOS_LOCATION and PRJ are set in PRJ_CMD.mk \\\n\
            \ECHRONOS_LOCATION should be the path to the echronos install where\\\n\
            \the setenv script and packages can be found. For example, the top of\\\n\
            \your echronos repository. PRJ should point to the prj tool."
  , "ECHRONOS_LOCATION" ?= maybeFP (configEchronosPath c)
  , "PRJ" ?= "$(ECHRONOS_LOCATION)/prj/app/prj.py"
  , Target ".PHONY" ["generate", "clean"] []
  , Target "generate" [".tag.echronos", ".tag.ramses"]
    ["-mv " ++ configSrcsDir c ++ "/*.[cs] gen/"
    ,"-rmdir " ++ configSrcsDir c
    ]
  , Target ".tag.echronos" [".tag.ramses"]
    [ "pushd $(ECHRONOS_LOCATION) && source setenv && popd &&  \\\n\
    \  $(ECHRONOS_LOCATION)/x.py build packages            &&  \\\n\
    \  $(PRJ) --output echronos_gen                            \\\n\
    \         --search-path $(ECHRONOS_LOCATION)/packages      \\\n\
    \         --no-project                                     \\\n\
    \         gen                                              \\\n\
    \         sys_impl.prx"
    , "touch .tag.echronos"
    ]
  , Target "clean" ["echronos-clean", "tower-clean"]
    [ "rm -f .tag.echronos" ]
  , include echronosMakefileName ]

echronosArtifacts :: AADLConfig -> [Located Artifact]
echronosArtifacts cfg = map Root ls ++ hw_artifacts ++ artifacts
  where
  ls :: [Artifact]
  ls = artifactString
         ramsesMakefileName
         (renderMkStmts (ramsesMakefile cfg))
     : osSpecific
  osSpecific =
      [ artifactString
          makefileName
          (renderMkStmts (makefile cfg))
      , artifactString
          echronosMakefileName
          (renderMkStmts (echronosMakefile cfg)) ]

defaultEChronosOS :: STM32Config -> OSSpecific STM32Config
defaultEChronosOS cfg =
  OSSpecific
    { osSpecificName       = "eChronos"
    , osSpecificConfig     = cfg
    , osSpecificArtifacts  = \_ c _ -> echronosArtifacts c
    , osSpecificSrcDir     = \_ x   -> x
    , osSpecificTower      = eChronosModules cfg
    , osSpecificOptsApps   = defaultOptsUpdate
    , osSpecificOptsLibs   = defaultOptsUpdate
    }

artifacts :: [Located Artifact]
artifacts = map (Root . artifactCabalFile P.getDataDir)
                [ "support/px_mkfw.py", "support/px_uploader.py", "support/px4fmu-v1.prototype", "support/px4fmu-v2.prototype"]

----------------------------------------------------------------------------
-- eChronos requires a custom main() function ------------------------------
----------------------------------------------------------------------------
mainProc :: STM32Config -> Def ('[] ':-> ())
mainProc cfg = proc "main" $ body $ do
  let rate = fromIntegral (clockSysClkHz (stm32config_clock cfg))
  call_ (init_clocks (stm32config_clock cfg))
  call_ clock_set_cpu_rate_in_hz rate
  result <- call initialize_periodic_dispatcher
  ifte_ (iNot result)
    (do call_ debug_println "Unable to initialize periodic dispatcher."
        call_ fatal (-1))
    (return ())

  call_ debug_println "Starting RTOS"
  call_ rtos_start
  forever (return ())

eChronosModules :: STM32Config -> Tower e ()
eChronosModules cfg = do
  towerModule  towerDepModule
  towerDepends towerDepModule
  towerModule  (mainMod cfg)
  towerDepends (mainMod cfg)
  towerModule  timeModule
  towerDepends timeModule

initialize_periodic_dispatcher :: Def('[] ':-> IBool)
initialize_periodic_dispatcher =
  importProc "initialize_periodic_dispatcher" "smaccm_decls.h"

clock_set_cpu_rate_in_hz  :: Def('[Sint64] ':-> ())
clock_set_cpu_rate_in_hz  =
  importProc "clock_set_cpu_rate_in_hz" "clock_driver.h"

debug_println :: Def('[IString] ':-> ())
debug_println = importProc "debug_println" "debug.h"

debug_printhex8 :: Def('[Uint8] ':-> ())
debug_printhex8 = importProc "debug_printhex8" "debug.h"

debug_print :: Def('[IString] ':-> ())
debug_print = importProc "debug_print" "debug.h"

type RTosErrorId = Uint8

fatal :: Def('[RTosErrorId] ':-> ())
fatal = proc "fatal" $ \error_id -> body $ do
  call_ debug_print "FATAL ERROR: "
  call_ debug_printhex8 error_id
  call_ debug_println ""
  forever (return ())

rtos_start :: Def ('[] ':-> ())
rtos_start = importProc "rtos_start" "rtos-kochab.h"

[ivory|
import (stdio.h, printf) void printf(string x, uint8_t y)
|]

inclSysTick :: STM32Config -> ModuleDef
inclSysTick cfg = do
  let config = SysTickConfig
        { sysTickProcessorHz   = clockSysClkHz (stm32config_clock cfg)
        , sysTickClockFactor   = Processor
        , sysTickInterruptMode = Interrupt
        }
  let storedInterval = area "stored_interval" (Just (ival 0))
      elapsedTicks = area "elapsed_ticks" (Just (ival 0))
  private $ do
    defMemArea storedInterval
    defMemArea elapsedTicks
  incl (clock_set_interval_in_us config (addrOf storedInterval))
  incl (clock_set_interval_in_ms config (addrOf storedInterval))
  incl (clock_start_timer (addrOf elapsedTicks))
  incl (clock_get_time (addrOf storedInterval) (addrOf elapsedTicks))
  incl (clock_irq_callback (addrOf elapsedTicks))
  incl clock_init

mainMod :: STM32Config -> Module
mainMod cfg = package "main" $ do
  incl (init_clocks (stm32config_clock cfg))
  hw_moduledef
  incl debug_println
  incl debug_print
  incl debug_printhex8
  incl fatal
  incl (mainProc cfg)
--  inclSysTick cfg
  incl initialize_periodic_dispatcher
  incl rtos_start
  incl clock_set_cpu_rate_in_hz

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf

-- Tower's time function

timeModule :: Module
timeModule = package "tower_time" $ do
  -- T.moddef
  incl getTimeProc
  incl clock_get_time'
  where
  getTimeProc :: Def('[] ':-> ITime)
  getTimeProc = proc "tower_get_time" $ body $ do
    t <- call clock_get_time'
    ret (fromIMilliseconds t)
  -- XXX: This is really Uint64 not ITime aka Sint64
  clock_get_time' :: Def('[]':->ITime)
  clock_get_time' = importProc "clock_get_time" "clock_driver.h"
