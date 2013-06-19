{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.DebugVect where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

debugVectMsgId :: Uint8
debugVectMsgId = 250

debugVectCrcExtra :: Uint8
debugVectCrcExtra = 49

debugVectModule :: Module
debugVectModule = package "mavlink_debug_vect_msg" $ do
  depend packModule
  incl debugVectUnpack
  defStruct (Proxy :: Proxy "debug_vect_msg")

[ivory|
struct debug_vect_msg
  { time_usec :: Stored Uint64
  ; x :: Stored IFloat
  ; y :: Stored IFloat
  ; z :: Stored IFloat
  ; name :: Array 10 (Stored Uint8)
  }
|]

mkDebugVectSender :: SizedMavlinkSender 30
                       -> Def ('[ ConstRef s (Struct "debug_vect_msg") ] :-> ())
mkDebugVectSender sender =
  proc ("mavlink_debug_vect_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    debugVectPack (senderMacro sender) msg

instance MavlinkSendable "debug_vect_msg" 30 where
  mkSender = mkDebugVectSender

debugVectPack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 30
                  -> ConstRef s1 (Struct "debug_vect_msg")
                  -> Ivory eff ()
debugVectPack sender msg = do
  arr <- local (iarray [] :: Init (Array 30 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> time_usec)
  call_ pack buf 8 =<< deref (msg ~> x)
  call_ pack buf 12 =<< deref (msg ~> y)
  call_ pack buf 16 =<< deref (msg ~> z)
  arrayPack buf 20 (msg ~> name)
  sender debugVectMsgId (constRef arr) debugVectCrcExtra
  retVoid

instance MavlinkUnpackableMsg "debug_vect_msg" where
    unpackMsg = ( debugVectUnpack , debugVectMsgId )

debugVectUnpack :: Def ('[ Ref s1 (Struct "debug_vect_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
debugVectUnpack = proc "mavlink_debug_vect_unpack" $ \ msg buf -> body $ do
  store (msg ~> time_usec) =<< call unpack buf 0
  store (msg ~> x) =<< call unpack buf 8
  store (msg ~> y) =<< call unpack buf 12
  store (msg ~> z) =<< call unpack buf 16
  arrayUnpack buf 20 (msg ~> name)

