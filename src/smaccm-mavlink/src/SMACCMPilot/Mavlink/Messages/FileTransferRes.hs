{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Autogenerated Mavlink v1.0 implementation: see smavgen_ivory.py

module SMACCMPilot.Mavlink.Messages.FileTransferRes where

import SMACCMPilot.Mavlink.Pack
import SMACCMPilot.Mavlink.Unpack
import SMACCMPilot.Mavlink.Send

import Ivory.Language

fileTransferResMsgId :: Uint8
fileTransferResMsgId = 112

fileTransferResCrcExtra :: Uint8
fileTransferResCrcExtra = 124

fileTransferResModule :: Module
fileTransferResModule = package "mavlink_file_transfer_res_msg" $ do
  depend packModule
  incl fileTransferResUnpack
  defStruct (Proxy :: Proxy "file_transfer_res_msg")

[ivory|
struct file_transfer_res_msg
  { transfer_uid :: Stored Uint64
  ; result :: Stored Uint8
  }
|]

mkFileTransferResSender :: SizedMavlinkSender 9
                       -> Def ('[ ConstRef s (Struct "file_transfer_res_msg") ] :-> ())
mkFileTransferResSender sender =
  proc ("mavlink_file_transfer_res_msg_send" ++ (senderName sender)) $ \msg -> body $ do
    fileTransferResPack (senderMacro sender) msg

instance MavlinkSendable "file_transfer_res_msg" 9 where
  mkSender = mkFileTransferResSender

fileTransferResPack :: (eff `AllocsIn` s, eff `Returns` ())
                  => SenderMacro eff s 9
                  -> ConstRef s1 (Struct "file_transfer_res_msg")
                  -> Ivory eff ()
fileTransferResPack sender msg = do
  arr <- local (iarray [] :: Init (Array 9 (Stored Uint8)))
  let buf = toCArray arr
  call_ pack buf 0 =<< deref (msg ~> transfer_uid)
  call_ pack buf 8 =<< deref (msg ~> result)
  sender fileTransferResMsgId (constRef arr) fileTransferResCrcExtra
  retVoid

instance MavlinkUnpackableMsg "file_transfer_res_msg" where
    unpackMsg = ( fileTransferResUnpack , fileTransferResMsgId )

fileTransferResUnpack :: Def ('[ Ref s1 (Struct "file_transfer_res_msg")
                             , ConstRef s2 (CArray (Stored Uint8))
                             ] :-> () )
fileTransferResUnpack = proc "mavlink_file_transfer_res_unpack" $ \ msg buf -> body $ do
  store (msg ~> transfer_uid) =<< call unpack buf 0
  store (msg ~> result) =<< call unpack buf 8

