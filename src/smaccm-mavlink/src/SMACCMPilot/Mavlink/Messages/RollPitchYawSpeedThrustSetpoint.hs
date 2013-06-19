{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.RollPitchYawSpeedThrustSetpoint where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

rollPitchYawSpeedThrustSetpointMsgId :: Uint8
rollPitchYawSpeedThrustSetpointMsgId = 59

rollPitchYawSpeedThrustSetpointCrcExtra :: Uint8
rollPitchYawSpeedThrustSetpointCrcExtra = 238

rollPitchYawSpeedThrustSetpointModule :: Module
rollPitchYawSpeedThrustSetpointModule = package "mavlink_roll_pitch_yaw_speed_thrust_setpoint_msg" $ do
  depend packModule
  incl rollPitchYawSpeedThrustSetpointUnpack
  defStruct (Proxy :: Proxy "roll_pitch_yaw_speed_thrust_setpoint_msg")

[ivory|
struct roll_pitch_yaw_speed_thrust_setpoint_msg
  { time_boot_ms :: Stored Uint32
  ; roll_speed :: Stored IFloat
  ; pitch_speed :: Stored IFloat
  ; yaw_speed :: Stored IFloat
  ; thrust :: Stored IFloat
  }
|]

mkRollPitchYawSpeedThrustSetpointSender :: SizedMavlinkSender 20
                       -> Def ('[ ConstRef s (Struct "roll_pitch_yaw_speed_thrust_setpoint_msg") ] :-> ())
mkRollPitchYawSpeedThrustSetpointSender sender =
  proc ("mavlink_roll_pitch_yaw_speed_thrust_setpoint_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    rollPitchYawSpeedThrustSetpointPack (senderMacro sender) msg

instance MavlinkSendable "roll_pitch_yaw_speed_thrust_setpoint_msg" 20 where
  mkSender = mkRollPitchYawSpeedThrustSetpointSender

rollPitchYawSpeedThrustSetpointPack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 20
                  -> ConstRef s1 (Struct "roll_pitch_yaw_speed_thrust_setpoint_msg")
                  -> Ivory eff ()
rollPitchYawSpeedThrustSetpointPack sender msg = do
  arr <- local (iarray [] :: Init (Array 20 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> time_boot_ms)
  call_ pack buf 4 =<< deref (msg ~> roll_speed)
  call_ pack buf 8 =<< deref (msg ~> pitch_speed)
  call_ pack buf 12 =<< deref (msg ~> yaw_speed)
  call_ pack buf 16 =<< deref (msg ~> thrust)
  sender rollPitchYawSpeedThrustSetpointMsgId (constRef arr) rollPitchYawSpeedThrustSetpointCrcExtra
  retVoid

instance MavlinkUnpackableMsg "roll_pitch_yaw_speed_thrust_setpoint_msg" where
    unpackMsg = ( rollPitchYawSpeedThrustSetpointUnpack , rollPitchYawSpeedThrustSetpointMsgId )

rollPitchYawSpeedThrustSetpointUnpack :: Def ('[ Ref s1 (Struct "roll_pitch_yaw_speed_thrust_setpoint_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
rollPitchYawSpeedThrustSetpointUnpack = proc "mavlink_roll_pitch_yaw_speed_thrust_setpoint_unpack" $ \ msg buf -> body $ do
  store (msg ~> time_boot_ms) =<< call unpack buf 0
  store (msg ~> roll_speed) =<< call unpack buf 4
  store (msg ~> pitch_speed) =<< call unpack buf 8
  store (msg ~> yaw_speed) =<< call unpack buf 12
  store (msg ~> thrust) =<< call unpack buf 16

