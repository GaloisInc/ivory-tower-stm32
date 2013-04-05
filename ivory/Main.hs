module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.Tower
--import Ivory.Tower.Graphviz
import qualified Ivory.Tower.Compile.FreeRTOS as FreeRTOS

import Ivory.BSP.HWF4 (hwf4Modules)
import Ivory.BSP.HWF4.USART (usart1)
import qualified Ivory.BSP.HWF4.GPIO as GPIO

import SMACCMPilot.Flight.Types (typeModules)

import SMACCMPilot.Flight.UserInput.Decode (userInputDecodeModule)
import SMACCMPilot.Flight.Control (controlModules)

import SMACCMPilot.Console           (consoleModule)
import SMACCMPilot.Storage.Partition (partitionModule)
import SMACCMPilot.Param             (paramModule)
import SMACCMPilot.Util.IvoryCString (cstringModule)

import SMACCMPilot.Flight.Control.Task
import SMACCMPilot.Flight.Motors.Task
import SMACCMPilot.Flight.Sensors.Task
import SMACCMPilot.Flight.UserInput.Task
import SMACCMPilot.Flight.BlinkTask
import SMACCMPilot.Flight.GCS.Tower

import SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import SMACCMPilot.Mavlink.Pack (packModule)
import SMACCMPilot.Mavlink.CRC (mavlinkCRCModule)

import Arm32SizeMap (sizeMap)

otherms :: [Module]
otherms = 
  -- flight types
  typeModules ++
  -- control subsystem
  controlModules ++
  -- mavlink system
  mavlinkMessageModules ++ [packModule, mavlinkCRCModule] ++
  -- bsp subsystem
  hwf4Modules ++
  -- the rest:
  [ userInputDecodeModule
  , cstringModule
  , consoleModule
  , partitionModule
  , paramModule
  ]

main :: IO ()
main = do 
  let objs = FreeRTOS.compile app
  compileWithSizeMap sizeMap objs
  --gviz asm


app :: Tower ()
app = do
  (src_userinput, snk_userinput)   <- dataport
  (src_sensors, snk_sensors)       <- dataport
  (src_control, snk_control)       <- dataport
  (src_flightmode, snk_flightmode) <- dataport
  (src_servos, snk_servos)         <- dataport
  (_, snk_position)                <- dataport


  task "sensors"   $ sensorsTask src_sensors
  task "userInput" $ userInputTask src_userinput src_flightmode
  task "blink"     $ blinkTask GPIO.pin_b13 snk_flightmode
  task "control"   $ controlTask snk_flightmode snk_userinput snk_sensors src_control
  task "motors"    $ motorsTask snk_control snk_flightmode src_servos

  gcsTower "usart1" usart1 snk_flightmode snk_sensors snk_position snk_control snk_servos

  mapM_ addModule otherms

--gviz :: Assembly -> IO ()
--gviz a = graphvizToFile "out.dot" a

