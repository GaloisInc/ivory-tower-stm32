{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.ManualSetpoint where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

manualSetpointMsgId :: Uint8
manualSetpointMsgId = 81

manualSetpointCrcExtra :: Uint8
manualSetpointCrcExtra = 106

manualSetpointModule :: Module
manualSetpointModule = package "mavlink_manual_setpoint_msg" $ do
  depend packModule
  incl manualSetpointUnpack
  defStruct (Proxy :: Proxy "manual_setpoint_msg")

[ivory|
struct manual_setpoint_msg
  { time_boot_ms :: Stored Uint32
  ; roll :: Stored IFloat
  ; pitch :: Stored IFloat
  ; yaw :: Stored IFloat
  ; thrust :: Stored IFloat
  ; mode_switch :: Stored Uint8
  ; manual_override_switch :: Stored Uint8
  }
|]

mkManualSetpointSender :: SizedMavlinkSender 22
                       -> Def ('[ ConstRef s (Struct "manual_setpoint_msg") ] :-> ())
mkManualSetpointSender sender =
  proc ("mavlink_manual_setpoint_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    manualSetpointPack (senderMacro sender) msg

instance MavlinkSendable "manual_setpoint_msg" 22 where
  mkSender = mkManualSetpointSender

manualSetpointPack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 22
                  -> ConstRef s1 (Struct "manual_setpoint_msg")
                  -> Ivory eff ()
manualSetpointPack sender msg = do
  arr <- local (iarray [] :: Init (Array 22 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> time_boot_ms)
  call_ pack buf 4 =<< deref (msg ~> roll)
  call_ pack buf 8 =<< deref (msg ~> pitch)
  call_ pack buf 12 =<< deref (msg ~> yaw)
  call_ pack buf 16 =<< deref (msg ~> thrust)
  call_ pack buf 20 =<< deref (msg ~> mode_switch)
  call_ pack buf 21 =<< deref (msg ~> manual_override_switch)
  sender manualSetpointMsgId (constRef arr) manualSetpointCrcExtra
  retVoid

instance MavlinkUnpackableMsg "manual_setpoint_msg" where
    unpackMsg = ( manualSetpointUnpack , manualSetpointMsgId )

manualSetpointUnpack :: Def ('[ Ref s1 (Struct "manual_setpoint_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
manualSetpointUnpack = proc "mavlink_manual_setpoint_unpack" $ \ msg buf -> body $ do
  store (msg ~> time_boot_ms) =<< call unpack buf 0
  store (msg ~> roll) =<< call unpack buf 4
  store (msg ~> pitch) =<< call unpack buf 8
  store (msg ~> yaw) =<< call unpack buf 12
  store (msg ~> thrust) =<< call unpack buf 16
  store (msg ~> mode_switch) =<< call unpack buf 20
  store (msg ~> manual_override_switch) =<< call unpack buf 21

