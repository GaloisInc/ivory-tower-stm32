{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.BSP.STM32.Interrupt where

import Ivory.Language
import Ivory.BSP.ARMv7M.Exception
import Ivory.Tower

class STM32Interrupt i where
  interruptIRQn             :: i -> IRQn
  interruptTable            :: [Maybe i]
  interruptHandlerName      :: i -> String
  interrupt_enable          :: forall eff . i -> Ivory eff ()
  interrupt_disable         :: forall eff . i -> Ivory eff ()
  interrupt_set_pending     :: forall eff . i -> Ivory eff ()
  interrupt_clear_pending   :: forall eff . i -> Ivory eff ()
  interrupt_set_priority    :: forall eff . i -> Uint8 -> Ivory eff ()

data IRQ i = Exception Exception
           | Interrupt i
           deriving (Eq, Show)

instance (STM32Interrupt i) => Signalable (IRQ i) where
  signalName = irqHandlerName
  signalHandler i b = incl $ proc (irqHandlerName i) $ body $ b >> retVoid
  signalInit (Exception _) = return ()
  signalInit (Interrupt i) =
    interrupt_set_priority i max_syscall_priority
    -- XXX MAGIC NUMBER: in tower/freertos, syscalls must be lower (numerically greater
    -- than) level 11. XXX how to make this cross OS platform safely?
    where max_syscall_priority = (12::Uint8)

irqn :: (STM32Interrupt i) => IRQ i -> IRQn
irqn (Exception e) = exceptionIRQn e
irqn (Interrupt i) = interruptIRQn i

irqHandlerName :: (STM32Interrupt i) => IRQ i -> String
irqHandlerName (Exception e) = exceptionHandlerName e
irqHandlerName (Interrupt i) = interruptHandlerName i
