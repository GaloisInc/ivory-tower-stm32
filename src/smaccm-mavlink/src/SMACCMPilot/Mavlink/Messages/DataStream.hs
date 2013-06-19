{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.DataStream where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

dataStreamMsgId :: Uint8
dataStreamMsgId = 67

dataStreamCrcExtra :: Uint8
dataStreamCrcExtra = 21

dataStreamModule :: Module
dataStreamModule = package "mavlink_data_stream_msg" $ do
  depend packModule
  incl dataStreamUnpack
  defStruct (Proxy :: Proxy "data_stream_msg")

[ivory|
struct data_stream_msg
  { message_rate :: Stored Uint16
  ; stream_id :: Stored Uint8
  ; on_off :: Stored Uint8
  }
|]

mkDataStreamSender :: SizedMavlinkSender 4
                       -> Def ('[ ConstRef s (Struct "data_stream_msg") ] :-> ())
mkDataStreamSender sender =
  proc ("mavlink_data_stream_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    dataStreamPack (senderMacro sender) msg

instance MavlinkSendable "data_stream_msg" 4 where
  mkSender = mkDataStreamSender

dataStreamPack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 4
                  -> ConstRef s1 (Struct "data_stream_msg")
                  -> Ivory eff ()
dataStreamPack sender msg = do
  arr <- local (iarray [] :: Init (Array 4 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> message_rate)
  call_ pack buf 2 =<< deref (msg ~> stream_id)
  call_ pack buf 3 =<< deref (msg ~> on_off)
  sender dataStreamMsgId (constRef arr) dataStreamCrcExtra
  retVoid

instance MavlinkUnpackableMsg "data_stream_msg" where
    unpackMsg = ( dataStreamUnpack , dataStreamMsgId )

dataStreamUnpack :: Def ('[ Ref s1 (Struct "data_stream_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
dataStreamUnpack = proc "mavlink_data_stream_unpack" $ \ msg buf -> body $ do
  store (msg ~> message_rate) =<< call unpack buf 0
  store (msg ~> stream_id) =<< call unpack buf 2
  store (msg ~> on_off) =<< call unpack buf 3

