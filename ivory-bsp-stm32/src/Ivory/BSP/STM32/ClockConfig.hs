
module Ivory.BSP.STM32.ClockConfig where

import Ivory.Tower.Config

data ClockSource = External Integer | Internal deriving (Eq, Show)

data PLLFactor = PLLFactor
  { pll_m :: Integer
  , pll_n :: Integer
  , pll_p :: Integer
  , pll_q :: Integer
  } deriving (Eq, Show)

data ClockConfig =
  ClockConfig
    { clockconfig_source        :: ClockSource
    , clockconfig_pll           :: PLLFactor
    , clockconfig_hclk_divider  :: Integer    -- HPRE
    , clockconfig_pclk1_divider :: Integer    -- PPRE1
    , clockconfig_pclk2_divider :: Integer    -- PPRE2
    } deriving (Eq, Show)

clockSourceHz :: ClockSource -> Integer
clockSourceHz (External rate) = rate
clockSourceHz Internal        = 16 * 1000 * 1000

clockSysClkHz :: ClockConfig -> Integer
clockSysClkHz cc = ((source `div` m) * n) `div` p
  where
  source = clockSourceHz (clockconfig_source cc)
  m      = pll_m         (clockconfig_pll cc)
  n      = pll_n         (clockconfig_pll cc)
  p      = pll_p         (clockconfig_pll cc)

clockPLL48ClkHz :: ClockConfig -> Integer
clockPLL48ClkHz cc = ((source `div` m) * n) `div` q
  where
  source = clockSourceHz (clockconfig_source cc)
  m      = pll_m         (clockconfig_pll cc)
  n      = pll_n         (clockconfig_pll cc)
  q      = pll_q         (clockconfig_pll cc)

clockHClkHz :: ClockConfig -> Integer
clockHClkHz cc = clockSysClkHz cc `div` (clockconfig_hclk_divider cc)

clockPClk1Hz :: ClockConfig -> Integer
clockPClk1Hz cc = clockHClkHz cc `div` (clockconfig_pclk1_divider cc)

clockPClk2Hz :: ClockConfig -> Integer
clockPClk2Hz cc = clockHClkHz cc `div` (clockconfig_pclk2_divider cc)

data PClk = PClk1 | PClk2

clockPClkHz :: PClk -> ClockConfig -> Integer
clockPClkHz PClk1 = clockPClk1Hz
clockPClkHz PClk2 = clockPClk2Hz

externalXtal :: Integer -> Integer -> ClockConfig
externalXtal xtal_mhz sysclk_mhz = ClockConfig
  { clockconfig_source = External (xtal_mhz * 1000 * 1000)
  , clockconfig_pll    = PLLFactor
      { pll_m = xtal_mhz
      , pll_n = sysclk_mhz * p
      , pll_p = p
      , pll_q = 7
      }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 4
  , clockconfig_pclk2_divider = 2
  }
  where p = 2

clockConfigParser :: ConfigParser ClockConfig
clockConfigParser = do
  xtal_mhz   <- subsection "xtalMHz" integer
  sysclk_mhz <- subsection "sysclkMHz" (integer `withDefault` 168)
  return (externalXtal xtal_mhz sysclk_mhz)

