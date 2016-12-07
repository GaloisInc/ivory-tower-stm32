{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.BSP.STM32.Driver.I2C.I2CDriverState
  ( I2CDriverState
  , i2cInactive
  , i2cEV5
  , i2cEV6
  , i2cEV7
  , i2cEV7_N2
  , i2cEV7_N1
  , i2cEV7_rx1
  , i2cEV8
  , i2cEV8_2
  ) where

import Ivory.Language

newtype I2CDriverState = I2CDriverState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

-- seems like a useful pattern; would be nice to have TH for it
-- because the concrete syntax sure looks weird...

-- | I2C driver states from Figures 243 and 244 of the Reference Manual
i2cInactive
  , i2cEV5
  , i2cEV6
  , i2cEV7
  , i2cEV7_N2
  , i2cEV7_N1
  , i2cEV7_rx1
  , i2cEV8
  , i2cEV8_2 :: I2CDriverState

[   i2cInactive
  , i2cEV5
  , i2cEV6
  , i2cEV7
  , i2cEV7_N2
  , i2cEV7_N1
  , i2cEV7_rx1
  , i2cEV8
  , i2cEV8_2
  ]
  = map (I2CDriverState . fromInteger) [0..8]
