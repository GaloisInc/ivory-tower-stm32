
module Ivory.BSP.STM32.Peripheral.UART.DMA where

import Ivory.BSP.STM32.Peripheral.UART.Peripheral
import Ivory.BSP.STM32.Peripheral.DMA

data DMAUART = DMAUART
  { dmaUARTPeriph    :: UART
  , dmaUARTDMAPeriph :: DMA
  , dmaUARTTxStream  :: DMAStream -- int 0..7
  , dmaUARTTxChannel :: DMAChannel -- int 0..7
  , dmaUARTRxStream  :: DMAStream -- int 0..7
  , dmaUARTRxChannel :: DMAChannel -- int 0..7
  }
