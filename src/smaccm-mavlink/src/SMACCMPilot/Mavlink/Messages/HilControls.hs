{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.HilControls where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

hilControlsMsgId :: Uint8
hilControlsMsgId = 91

hilControlsCrcExtra :: Uint8
hilControlsCrcExtra = 63

hilControlsModule :: Module
hilControlsModule = package "mavlink_hil_controls_msg" $ do
  depend packModule
  incl hilControlsUnpack
  defStruct (Proxy :: Proxy "hil_controls_msg")

[ivory|
struct hil_controls_msg
  { time_usec :: Stored Uint64
  ; roll_ailerons :: Stored IFloat
  ; pitch_elevator :: Stored IFloat
  ; yaw_rudder :: Stored IFloat
  ; throttle :: Stored IFloat
  ; aux1 :: Stored IFloat
  ; aux2 :: Stored IFloat
  ; aux3 :: Stored IFloat
  ; aux4 :: Stored IFloat
  ; mode :: Stored Uint8
  ; nav_mode :: Stored Uint8
  }
|]

mkHilControlsSender :: SizedMavlinkSender 42
                       -> Def ('[ ConstRef s (Struct "hil_controls_msg") ] :-> ())
mkHilControlsSender sender =
  proc ("mavlink_hil_controls_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    hilControlsPack (senderMacro sender) msg

instance MavlinkSendable "hil_controls_msg" 42 where
  mkSender = mkHilControlsSender

hilControlsPack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 42
                  -> ConstRef s1 (Struct "hil_controls_msg")
                  -> Ivory eff ()
hilControlsPack sender msg = do
  arr <- local (iarray [] :: Init (Array 42 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> time_usec)
  call_ pack buf 8 =<< deref (msg ~> roll_ailerons)
  call_ pack buf 12 =<< deref (msg ~> pitch_elevator)
  call_ pack buf 16 =<< deref (msg ~> yaw_rudder)
  call_ pack buf 20 =<< deref (msg ~> throttle)
  call_ pack buf 24 =<< deref (msg ~> aux1)
  call_ pack buf 28 =<< deref (msg ~> aux2)
  call_ pack buf 32 =<< deref (msg ~> aux3)
  call_ pack buf 36 =<< deref (msg ~> aux4)
  call_ pack buf 40 =<< deref (msg ~> mode)
  call_ pack buf 41 =<< deref (msg ~> nav_mode)
  sender hilControlsMsgId (constRef arr) hilControlsCrcExtra
  retVoid

instance MavlinkUnpackableMsg "hil_controls_msg" where
    unpackMsg = ( hilControlsUnpack , hilControlsMsgId )

hilControlsUnpack :: Def ('[ Ref s1 (Struct "hil_controls_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
hilControlsUnpack = proc "mavlink_hil_controls_unpack" $ \ msg buf -> body $ do
  store (msg ~> time_usec) =<< call unpack buf 0
  store (msg ~> roll_ailerons) =<< call unpack buf 8
  store (msg ~> pitch_elevator) =<< call unpack buf 12
  store (msg ~> yaw_rudder) =<< call unpack buf 16
  store (msg ~> throttle) =<< call unpack buf 20
  store (msg ~> aux1) =<< call unpack buf 24
  store (msg ~> aux2) =<< call unpack buf 28
  store (msg ~> aux3) =<< call unpack buf 32
  store (msg ~> aux4) =<< call unpack buf 36
  store (msg ~> mode) =<< call unpack buf 40
  store (msg ~> nav_mode) =<< call unpack buf 41

