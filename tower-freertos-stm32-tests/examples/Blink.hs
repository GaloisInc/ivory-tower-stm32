module Main where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32.Peripheral.GPIOF4
import qualified Ivory.BSP.STM32F405.GPIO as F405

-- | This is a Tower program that blinks an LED connected to a GPIO
-- pin.  It consists of two monitors, one that emits a stream of
-- Boolean values and another that reads those values and updates the
-- LED correspondingly.
blink :: Tower e ()
blink = do
  -- Create a channel for the two monitors to communicate on, and a
  -- period that fires once a second:
  (cin, cout) <- channel
  everySec <- period (Milliseconds 1000)

  -- The @stateMgmt@ monitor maintains a Boolean value which indicates
  -- the state of the LED.  Using the 'period' from above, its handler
  -- negates the value and emits the updated value to the channel.
  monitor "stateMgmt" $ do
    ledlit <- stateInit "ledlit" (ival false)

    handler everySec "flipflop" $ do
      e <- emitter cin 1

      callback $ \_ -> do
        comment "Emit next bool into the channel."
        toggled <- fmap iNot (deref ledlit)
        store ledlit toggled
        emitV e toggled

  -- The @ledMgmt@ monitor maintains the state of the hardware (LED).
  -- It has two handlers, one to initialize the GPIO pin that the LED
  -- is connected to, and another to change the LED state.
  --
  -- When the @flipflop@ handler from above emits a value on the
  -- channel, the @updateLED@ handler below will be called and passed
  -- the emitted value.
  monitor "ledMgmt" $ do
    handler systemInit "initLED" $
      callback $ \_ -> do
        comment "Configure the LED GPIO pin."
        pinEnable ledPin
        pinSetMode ledPin gpio_mode_output

    handler cout "updateLED" $
      callback $ \incomming -> do
        comment "Process request to change LED state."
        requestedState <- deref incomming
        ifte_ requestedState (pinSet ledPin) (pinClear ledPin)

-- | Change to suit your needs.
ledPin :: GPIOPin
ledPin = F405.pinA7

-- | Main entry point.  Compiles the @blink@ Tower program into C.
main :: IO ()
main = compileTowerSTM32FreeRTOS id p blink
  where p topts = getConfig topts $ stm32ConfigParser $ stm32f405Defaults 24
