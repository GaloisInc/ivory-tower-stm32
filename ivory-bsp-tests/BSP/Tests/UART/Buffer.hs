{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BSP.Tests.UART.Buffer where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

uartUnbuffer :: forall b e
              . (IvoryString b)
             => BackpressureTransmit b ('Stored IBool)
             -> Tower e (ChanInput ('Stored Uint8))
uartUnbuffer (BackpressureTransmit req res) = do
  c <- channel
  p <- period (Milliseconds 10)
  monitor "uart_unbuffer" $ do
    flush_defer <- state "flush_defer"
    buf <- state "buffer"
    let ready_buf :: Ivory eff IBool
        ready_buf = fmap (>? 0) (deref (buf ~> stringLengthL))

        send_buf :: Emitter b -> Ivory eff ()
        send_buf e = do
          emit e (constRef buf)
          store (buf ~> stringLengthL) 0
          store flush_defer true

    handler (snd c) "uart_byte_tosend" $ do
      callbackV $ \byte -> do
        pos <- deref (buf ~> stringLengthL)
        when (pos <? arrayLen (buf ~> stringDataL)) $ do
          store (buf ~> stringDataL ! toIx pos) byte
          store (buf ~> stringLengthL) (pos + 1)

    handler p "uart_tx_flush" $ do
      e <- emitter req 1
      callback $ const $ do
        defer <- deref flush_defer
        ready <- ready_buf
        when (ready .&& iNot defer) (send_buf e)

    handler res "uart_tx_res" $ do
      callback $ const $ store flush_defer false

  return (fst c)

  where

