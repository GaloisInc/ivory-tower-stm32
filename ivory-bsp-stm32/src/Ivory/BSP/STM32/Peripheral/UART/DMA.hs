
module Ivory.BSP.STM32.Peripheral.UART.DMA where

import Ivory.BSP.STM32.Peripheral.UART.Peripheral
import Ivory.BSP.STM32.Peripheral.DMA
import Ivory.BSP.STM32.Driver.DMA

data DMAUART = DMAUART
  { dmaUARTPeriph    :: UART
  , dmaUARTDMAPeriph :: DMA
  , dmaUARTTxStream  :: DMATowerStreams -> DMATowerStream
  , dmaUARTTxChannel :: Int -- 0..8
  , dmaUARTRxStream  :: DMATowerStreams -> DMATowerStream
  , dmaUARTRxChannel :: Int -- 0..8
  }
