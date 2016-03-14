-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2016 Galois, Inc.
-- License     :  BSD3
-- Maintainer  :  smaccmpilot@galois.com
-- Stability   :  provisional
-- Portability :  portable
-- 
-- SysTick timer registers for ARMv7M. Note that while the layout and
-- addresses of these registers are consistent across the
-- architecture, different specific implementations may have different
-- semantics, particularly for the calibration register @STK_CALIB@.
--
-- If the names of functions in this module seem a bit strange, it's
-- because we want to match echronos' conventions.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Ivory.BSP.ARMv7M.SysTick where

import Data.Ratio

import Ivory.Language
import Ivory.HW

import Ivory.BSP.ARMv7M.MemoryMap (systick_base)

--------------------------------------------------------------------------------
-- SysTick Interface

-- | Clock source for SysTick is either the processor clock, or the
-- processor clock divided by 8.
data ClockFactor = Processor | ProcessorDiv8 deriving (Eq, Show)

-- | SysTick can be configured either to raise an interrupt for each
-- tick, or to simply set a flag in the 'regSTK_CTRL'.
data InterruptMode = Interrupt | FlagOnly deriving (Eq, Show)

data SysTickConfig = SysTickConfig
  { -- | Processor clock speed in hertz
    sysTickProcessorHz   :: Integer
  , sysTickClockFactor   :: ClockFactor
  , sysTickInterruptMode :: InterruptMode
  } deriving (Eq, Show)

-- | Sets the SysTick interval to the given number of microseconds,
-- and then starts the SysTick counter.
clock_set_interval_in_us :: SysTickConfig
                         -> Ref 'Global ('Stored Uint64)
                         -> Def('[Uint64]':->())
clock_set_interval_in_us sysTick storedInterval =
  proc "clock_set_interval_in_us" $ \(interval :: Uint64) -> body $ do
    comment (show sysTick)
    let cyclesPerUs :: Integer
        cyclesPerUs =
          case sysTickClockFactor sysTick of
            Processor     -> round (sysTickProcessorHz sysTick % 1000000)
            ProcessorDiv8 -> round (sysTickProcessorHz sysTick % 1000000 / 8)
        cyclesPerInterval = interval * fromIntegral cyclesPerUs
    -- Procedure from DocID022708 Rev 4, section 4.5.5
    -- Step 1: Program reload value
    modifyReg regSTK_LOAD $ do
      -- subtract 1 per section 4.5.2
      setField stk_load_reload $ fromRep $ lbits $ cyclesPerInterval - 1
    -- Step 2: Clear current value
    modifyReg regSTK_VAL $ do
      setField stk_val_current $ fromRep 0
    -- Step 3: Program Control and Status register
    modifyReg regSTK_CTRL $ do
      case sysTickClockFactor sysTick of
        Processor     -> setBit   stk_ctrl_clksource
        ProcessorDiv8 -> clearBit stk_ctrl_clksource
      case sysTickInterruptMode sysTick of
        Interrupt -> setBit   stk_ctrl_tickint
        FlagOnly  -> clearBit stk_ctrl_tickint
      setBit stk_ctrl_enable
    -- Now save the interval so that clock_get_time can
    -- calculate correctly
    store storedInterval interval

-- | Sets the SysTick interval to the given number of milliseconds,
-- and then starts the SysTick counter.
clock_set_interval_in_ms :: SysTickConfig
                         -> Ref 'Global ('Stored Uint64)
                         -> Def('[Uint32]':->())
clock_set_interval_in_ms sysTick storedInterval =
  proc "clock_set_interval_in_ms" $ \(interval :: Uint32) -> body $ do
    call_ (clock_set_interval_in_us sysTick storedInterval)
          (safeCast interval * 1000)

-- | Call this function from the SysTick interrupt handler if you want
-- to be able to use 'clock_get_time'.
-- 
-- XXX: These should really be in the echronos glue code, not the SysTick setup
clock_irq_callback :: Ref 'Global ('Stored Uint64) -> Def('[]':->())
clock_irq_callback elapsedTicks = proc "clock_irq_callback" $ body $ do
  ticks <- deref elapsedTicks
  store elapsedTicks (ticks + 1)

-- | Reset the time reported by 'clock_get_time' to 0.
-- 
-- XXX: These should really be in the echronos glue code, not the SysTick setup
clock_start_timer :: Ref 'Global ('Stored Uint64) -> Def('[]':->())
clock_start_timer elapsedTicks = proc "clock_start_timer" $ body $ do
  store elapsedTicks 0

-- | Get the elapsed time in microseconds since 'clock_start_timer'
-- was called. This only works if 'clock_set_interval_in_us' or
-- 'clock_set_interval_in_ms' has been called.
-- 
-- XXX: These should really be in the echronos glue code, not the SysTick setup
clock_get_time :: Ref 'Global ('Stored Uint64)
               -> Ref 'Global ('Stored Uint64)
               -> Def('[]':->Uint64)
clock_get_time storedInterval elapsedTicks =
  proc "clock_get_time" $ body $ do
    interval <- deref storedInterval
    ticks <- deref elapsedTicks
    ret (interval * ticks)

-- | Unfortunate noop to match the echronos interface.
-- XXX: These should really be in the echronos glue code, not the SysTick setup
clock_init :: Def('[]':->())
clock_init = proc "clock_init" $ body $ return ()

--------------------------------------------------------------------------------
-- SysTick Registers

[ivory|
 bitdata STK_CTRL :: Bits 32 = stk_ctrl
  { _                  :: Bits 15
  , stk_ctrl_countflag :: Bit
  , _                  :: Bits 13
  , stk_ctrl_clksource :: Bit
  , stk_ctrl_tickint   :: Bit
  , stk_ctrl_enable    :: Bit
  }
|]

regSTK_CTRL :: BitDataReg STK_CTRL
regSTK_CTRL = mkBitDataRegNamed (systick_base + 0x00) "stk_ctrl"

[ivory|
 bitdata STK_LOAD :: Bits 32 = stk_load
  { _               :: Bits 8
  , stk_load_reload :: Bits 24
  }
|]

regSTK_LOAD :: BitDataReg STK_LOAD
regSTK_LOAD = mkBitDataRegNamed (systick_base + 0x04) "stk_load"

[ivory|
 bitdata STK_VAL :: Bits 32 = stk_val
  { _               :: Bits 8
  , stk_val_current :: Bits 24
  }
|]

regSTK_VAL :: BitDataReg STK_VAL
regSTK_VAL = mkBitDataRegNamed (systick_base + 0x08) "stk_val"

[ivory|
 bitdata STK_CALIB :: Bits 32 = stk_calib
  { stk_calib_noref :: Bit
  , stk_calib_skew  :: Bit
  , _               :: Bits 6
  , stk_calib_tenms :: Bits 24
  }
|]

regSTK_CALIB :: BitDataReg STK_CALIB
regSTK_CALIB = mkBitDataRegNamed (systick_base + 0x0C) "stk_calib"
