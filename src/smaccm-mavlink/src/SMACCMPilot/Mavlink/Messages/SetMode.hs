{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.SetMode where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

setModeMsgId :: Uint8
setModeMsgId = 11

setModeCrcExtra :: Uint8
setModeCrcExtra = 89

setModeModule :: Module
setModeModule = package "mavlink_set_mode_msg" $ do
  depend packModule
  incl setModeUnpack
  defStruct (Proxy :: Proxy "set_mode_msg")

[ivory|
struct set_mode_msg
  { custom_mode :: Stored Uint32
  ; target_system :: Stored Uint8
  ; base_mode :: Stored Uint8
  }
|]

mkSetModeSender :: SizedMavlinkSender 6
                       -> Def ('[ ConstRef s (Struct "set_mode_msg") ] :-> ())
mkSetModeSender sender =
  proc ("mavlink_set_mode_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    setModePack (senderMacro sender) msg

instance MavlinkSendable "set_mode_msg" 6 where
  mkSender = mkSetModeSender

setModePack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 6
                  -> ConstRef s1 (Struct "set_mode_msg")
                  -> Ivory eff ()
setModePack sender msg = do
  arr <- local (iarray [] :: Init (Array 6 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> custom_mode)
  call_ pack buf 4 =<< deref (msg ~> target_system)
  call_ pack buf 5 =<< deref (msg ~> base_mode)
  sender setModeMsgId (constRef arr) setModeCrcExtra
  retVoid

instance MavlinkUnpackableMsg "set_mode_msg" where
    unpackMsg = ( setModeUnpack , setModeMsgId )

setModeUnpack :: Def ('[ Ref s1 (Struct "set_mode_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
setModeUnpack = proc "mavlink_set_mode_unpack" $ \ msg buf -> body $ do
  store (msg ~> custom_mode) =<< call unpack buf 0
  store (msg ~> target_system) =<< call unpack buf 4
  store (msg ~> base_mode) =<< call unpack buf 5

