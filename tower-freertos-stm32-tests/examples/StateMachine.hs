
module Main where

import Ivory.Tower
import Ivory.Tower.StateMachine.Example

import Ivory.Tower.Compile
import Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

t :: Tower e ()
t = do
  monitor "test" $ do
    _sm <- ex
    return ()

main :: IO ()
main = towerCompile p t
  where p topts = do
            cfg <- getConfig topts stm32ConfigParser
            return $ stm32FreeRTOS id cfg
