--
-- UART/DMA.hs --- DMA UART Driver
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--


module Ivory.BSP.STM32F427.UART.DMA
  ( dmaUART1, dmaUART2, dmaUART3, dmaUART4, dmaUART5, dmaUART6, dmaUART7, dmaUART8
  ) where

import Ivory.BSP.STM32.Peripheral.DMA.Peripheral
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32F427.UART
import Ivory.BSP.STM32F427.DMA

-- Please be careful to not use multiple dma uarts that expect to use the same
-- DMA periph & stream.
-- There are more choices available for mapping each tx/rx to a given
-- stream/channel - I just ended up making some pretty arbitrary decisions here
-- because I didn't want to encode the entire DMA stream/channel matrix and then
-- create some way to choose which one you wanted.

dmaUART1, dmaUART2, dmaUART3, dmaUART4, dmaUART5, dmaUART6, dmaUART7, dmaUART8 :: DMAUART
dmaUART1 = DMAUART
  { dmaUARTPeriph    = uart1
  , dmaUARTDMAPeriph = dma2
  , dmaUARTTxStream  = DMAStream  7
  , dmaUARTTxChannel = DMAChannel 4
  , dmaUARTRxStream  = DMAStream  2
  , dmaUARTRxChannel = DMAChannel 4
  }

dmaUART2 = DMAUART
  { dmaUARTPeriph    = uart2
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = DMAStream  6
  , dmaUARTTxChannel = DMAChannel 4
  , dmaUARTRxStream  = DMAStream  5
  , dmaUARTRxChannel = DMAChannel 4
  }

dmaUART3 = DMAUART
  { dmaUARTPeriph    = uart3
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = DMAStream  3
  , dmaUARTTxChannel = DMAChannel 4
  , dmaUARTRxStream  = DMAStream  1
  , dmaUARTRxChannel = DMAChannel 4
  }

dmaUART4 = DMAUART
  { dmaUARTPeriph    = uart4
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = DMAStream  4
  , dmaUARTTxChannel = DMAChannel 4
  , dmaUARTRxStream  = DMAStream  2
  , dmaUARTRxChannel = DMAChannel 4
  }

dmaUART5 = DMAUART
  { dmaUARTPeriph    = uart5
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = DMAStream  7
  , dmaUARTTxChannel = DMAChannel 4
  , dmaUARTRxStream  = DMAStream  0
  , dmaUARTRxChannel = DMAChannel 4
  }

dmaUART6 = DMAUART
  { dmaUARTPeriph    = uart6
  , dmaUARTDMAPeriph = dma2
  , dmaUARTTxStream  = DMAStream  6
  , dmaUARTTxChannel = DMAChannel 5
  , dmaUARTRxStream  = DMAStream  1
  , dmaUARTRxChannel = DMAChannel 5
  }

dmaUART7 = DMAUART
  { dmaUARTPeriph    = uart7
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = DMAStream  1
  , dmaUARTTxChannel = DMAChannel 5
  , dmaUARTRxStream  = DMAStream  3
  , dmaUARTRxChannel = DMAChannel 5
  }

dmaUART8 = DMAUART
  { dmaUARTPeriph    = uart8
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = DMAStream  0
  , dmaUARTTxChannel = DMAChannel 5
  , dmaUARTRxStream  = DMAStream  6
  , dmaUARTRxChannel = DMAChannel 5
  }

