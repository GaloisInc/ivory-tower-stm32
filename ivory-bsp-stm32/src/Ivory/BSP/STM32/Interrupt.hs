{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.BSP.STM32.Interrupt where

import Ivory.Language
import Ivory.Tower
import Ivory.HW
import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.ARMv7M.SystemControl.NVIC


class STM32Interrupt i where
  interruptIRQn             :: i -> IRQn
  interruptTable            :: i -> [Maybe i]
  interruptHandlerName      :: i -> String

data HasSTM32Interrupt = forall i. (STM32Interrupt i)
                       => HasSTM32Interrupt i

instance STM32Interrupt HasSTM32Interrupt where
  interruptIRQn (HasSTM32Interrupt i) = interruptIRQn i
  interruptTable (HasSTM32Interrupt i) = map (fmap HasSTM32Interrupt) (interruptTable i)
  interruptHandlerName (HasSTM32Interrupt i) = interruptHandlerName i

data IRQ i = Exception Exception
           | Interrupt i
           deriving (Eq, Show)

instance (STM32Interrupt i) => Signalable (IRQ i) where
  signalName = irqHandlerName
  signalHandler i b = incl $ voidProc (irqHandlerName i) $ body $ noReturn b
  signalInit (Exception _) = return ()
  signalInit (Interrupt i) =
    interrupt_set_priority i max_syscall_priority
    -- XXX MAGIC NUMBER: in tower/freertos, syscalls must be lower (numerically greater
    -- than) level 11. XXX how to make this cross OS platform safely?
    where max_syscall_priority = (12::Uint8)
  signalNumber = fromIntegral . unIRQn . irqn

irqn :: (STM32Interrupt i) => IRQ i -> IRQn
irqn (Exception e) = exceptionIRQn e
irqn (Interrupt i) = interruptIRQn i

irqHandlerName :: (STM32Interrupt i) => IRQ i -> String
irqHandlerName (Exception e) = exceptionHandlerName e
irqHandlerName (Interrupt i) = interruptHandlerName i

----------------------------------------------------------------------
-- High Level Interface

interrupt_enable :: STM32Interrupt i => i -> Ivory eff ()
interrupt_enable i = do
  let (reg, bitN) = nvic_ISER (interruptIRQn i)
  setReg reg $ do
    setBit (nvic_iser_setena #> bitIx bitN)

interrupt_disable :: STM32Interrupt i => i -> Ivory eff ()
interrupt_disable i = do
  let (reg, bitN) = nvic_ICER (interruptIRQn i)
  setReg reg $ do
    setBit (nvic_icer_clrena #> bitIx bitN)

interrupt_set_pending :: STM32Interrupt i => i -> Ivory eff ()
interrupt_set_pending i = do
  let (reg, bitN) = nvic_ISPR (interruptIRQn i)
  setReg reg $ do
    setBit (nvic_ispr_setpend #> bitIx bitN)

interrupt_clear_pending :: STM32Interrupt i => i -> Ivory eff ()
interrupt_clear_pending i = do
  let (reg, bitN) = nvic_ICPR (interruptIRQn i)
  setReg reg $ do
    setBit (nvic_icpr_clrpend #> bitIx bitN)

-- | interrupt_set_priority: always give the priority as level 0 (highest) to 16
--   (lowest).
interrupt_set_priority :: STM32Interrupt i => i -> Uint8 -> Ivory eff ()
interrupt_set_priority i pri = do
  assert (pri <? (1 `iShiftL` nvic_prio_shift))
  let pri' = pri `iShiftL` nvic_prio_shift
  writeReg (nvic_IPR (interruptIRQn i)) pri'
  where
  -- | The STM32F4 NVIC ignores writes to the low 4 bits of the
  -- interrupt priority registers.  We hide this from callers, so the
  -- API accepts interrupt priority levels from 0 to 15.
  nvic_prio_shift :: Uint8
  nvic_prio_shift = 4
