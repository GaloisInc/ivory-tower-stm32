{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Driver.DMA
  ( DMATowerStreams(..)
  , DMATowerStream(..)
  , dmaTower
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.DMA

data DMATowerStreams =
  DMATowerStreams
    { dma_stream_periph :: String
    , dma_stream_0 :: DMATowerStream
    , dma_stream_1 :: DMATowerStream
    , dma_stream_2 :: DMATowerStream
    , dma_stream_3 :: DMATowerStream
    , dma_stream_4 :: DMATowerStream
    , dma_stream_5 :: DMATowerStream
    , dma_stream_6 :: DMATowerStream
    , dma_stream_7 :: DMATowerStream
    }

data DMATowerStream =
  DMATowerStream
    { dma_stream_init           :: ChanOutput (Stored ITime)
    , dma_stream_signal         :: ChanOutput (Stored ITime)
    , dma_stream_regs           :: DMAStreamRegs
    , dma_stream_enable_int     :: forall eff . Ivory eff ()
    , dma_stream_get_isrflags   :: forall eff . Ivory eff DMA_ISRFlags
    , dma_stream_clear_isrflags :: forall eff . Ivory eff ()
    }

dmaTower :: DMA -> Tower e DMATowerStreams
dmaTower periph = do

  init_chan <- channel
  dma_stream_0 <- mkTowerStream 0 (snd init_chan)
  dma_stream_1 <- mkTowerStream 1 (snd init_chan)
  dma_stream_2 <- mkTowerStream 2 (snd init_chan)
  dma_stream_3 <- mkTowerStream 3 (snd init_chan)
  dma_stream_4 <- mkTowerStream 4 (snd init_chan)
  dma_stream_5 <- mkTowerStream 5 (snd init_chan)
  dma_stream_6 <- mkTowerStream 6 (snd init_chan)
  dma_stream_7 <- mkTowerStream 7 (snd init_chan)

  monitor "dmaPeriph" $ do
    handler systemInit "dmaPeriphInit" $ do
      init_e <- emitter (fst init_chan) 1
      callback $ \t -> do
        dmaRCCEnable periph
        emit init_e t

  let dma_stream_periph = dmaName periph
  return DMATowerStreams{..}

  where

  mkTowerStream :: DMAStream
                -> ChanOutput (Stored ITime)
                -> Tower e DMATowerStream
  mkTowerStream s init_chan = do
    c <- signalUnsafe (Interrupt int)
                      (Microseconds 10)
                      (interrupt_disable int)

    return (stream c)
    where
    int = streamInterrupt periph s
    stream c = DMATowerStream
      { dma_stream_init           = init_chan
      , dma_stream_signal         = c
      , dma_stream_regs           = getStreamRegs periph s
      , dma_stream_enable_int     = interrupt_enable int
      , dma_stream_get_isrflags   = getISRFlags   periph s
      , dma_stream_clear_isrflags = clearISRFlags periph s
      }


