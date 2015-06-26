--
-- UART/DMA.hs --- DMA UART Driver
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--


module Ivory.BSP.STM32F427.UART.DMA
  ( dmaUART1, dmaUART2, dmaUART3, dmaUART4, dmaUART5, dmaUART6, dmaUART7, dmaUART8
  ) where

import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.Driver.DMA
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
  , dmaUARTTxStream  = dma_stream_7
  , dmaUARTTxChannel = 4
  , dmaUARTRxStream  = dma_stream_2
  , dmaUARTRxChannel = 4
  }

dmaUART2 = DMAUART
  { dmaUARTPeriph    = uart2
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = dma_stream_6
  , dmaUARTTxChannel = 4
  , dmaUARTRxStream  = dma_stream_5
  , dmaUARTRxChannel = 4
  }

dmaUART3 = DMAUART
  { dmaUARTPeriph    = uart3
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = dma_stream_3
  , dmaUARTTxChannel = 4
  , dmaUARTRxStream  = dma_stream_1
  , dmaUARTRxChannel = 4
  }

dmaUART4 = DMAUART
  { dmaUARTPeriph    = uart4
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = dma_stream_4
  , dmaUARTTxChannel = 4
  , dmaUARTRxStream  = dma_stream_2
  , dmaUARTRxChannel = 4
  }

dmaUART5 = DMAUART
  { dmaUARTPeriph    = uart5
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = dma_stream_7
  , dmaUARTTxChannel = 4
  , dmaUARTRxStream  = dma_stream_0
  , dmaUARTRxChannel = 4
  }

dmaUART6 = DMAUART
  { dmaUARTPeriph    = uart6
  , dmaUARTDMAPeriph = dma2
  , dmaUARTTxStream  = dma_stream_6
  , dmaUARTTxChannel = 5
  , dmaUARTRxStream  = dma_stream_1
  , dmaUARTRxChannel = 5
  }

dmaUART7 = DMAUART
  { dmaUARTPeriph    = uart7
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = dma_stream_1
  , dmaUARTTxChannel = 5
  , dmaUARTRxStream  = dma_stream_3
  , dmaUARTRxChannel = 5
  }

dmaUART8 = DMAUART
  { dmaUARTPeriph    = uart8
  , dmaUARTDMAPeriph = dma1
  , dmaUARTTxStream  = dma_stream_0
  , dmaUARTTxChannel = 5
  , dmaUARTRxStream  = dma_stream_6
  , dmaUARTRxChannel = 5
  }

