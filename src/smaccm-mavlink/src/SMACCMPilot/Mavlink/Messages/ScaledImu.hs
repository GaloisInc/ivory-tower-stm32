{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.ScaledImu where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

scaledImuMsgId :: Uint8
scaledImuMsgId = 26

scaledImuCrcExtra :: Uint8
scaledImuCrcExtra = 170

scaledImuModule :: Module
scaledImuModule = package "mavlink_scaled_imu_msg" $ do
  depend packModule
  incl scaledImuUnpack
  defStruct (Proxy :: Proxy "scaled_imu_msg")

[ivory|
struct scaled_imu_msg
  { time_boot_ms :: Stored Uint32
  ; xacc :: Stored Sint16
  ; yacc :: Stored Sint16
  ; zacc :: Stored Sint16
  ; xgyro :: Stored Sint16
  ; ygyro :: Stored Sint16
  ; zgyro :: Stored Sint16
  ; xmag :: Stored Sint16
  ; ymag :: Stored Sint16
  ; zmag :: Stored Sint16
  }
|]

mkScaledImuSender :: SizedMavlinkSender 22
                       -> Def ('[ ConstRef s (Struct "scaled_imu_msg") ] :-> ())
mkScaledImuSender sender =
  proc ("mavlink_scaled_imu_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    scaledImuPack (senderMacro sender) msg

instance MavlinkSendable "scaled_imu_msg" 22 where
  mkSender = mkScaledImuSender

scaledImuPack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 22
                  -> ConstRef s1 (Struct "scaled_imu_msg")
                  -> Ivory eff ()
scaledImuPack sender msg = do
  arr <- local (iarray [] :: Init (Array 22 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> time_boot_ms)
  call_ pack buf 4 =<< deref (msg ~> xacc)
  call_ pack buf 6 =<< deref (msg ~> yacc)
  call_ pack buf 8 =<< deref (msg ~> zacc)
  call_ pack buf 10 =<< deref (msg ~> xgyro)
  call_ pack buf 12 =<< deref (msg ~> ygyro)
  call_ pack buf 14 =<< deref (msg ~> zgyro)
  call_ pack buf 16 =<< deref (msg ~> xmag)
  call_ pack buf 18 =<< deref (msg ~> ymag)
  call_ pack buf 20 =<< deref (msg ~> zmag)
  sender scaledImuMsgId (constRef arr) scaledImuCrcExtra
  retVoid

instance MavlinkUnpackableMsg "scaled_imu_msg" where
    unpackMsg = ( scaledImuUnpack , scaledImuMsgId )

scaledImuUnpack :: Def ('[ Ref s1 (Struct "scaled_imu_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
scaledImuUnpack = proc "mavlink_scaled_imu_unpack" $ \ msg buf -> body $ do
  store (msg ~> time_boot_ms) =<< call unpack buf 0
  store (msg ~> xacc) =<< call unpack buf 4
  store (msg ~> yacc) =<< call unpack buf 6
  store (msg ~> zacc) =<< call unpack buf 8
  store (msg ~> xgyro) =<< call unpack buf 10
  store (msg ~> ygyro) =<< call unpack buf 12
  store (msg ~> zgyro) =<< call unpack buf 14
  store (msg ~> xmag) =<< call unpack buf 16
  store (msg ~> ymag) =<< call unpack buf 18
  store (msg ~> zmag) =<< call unpack buf 20

