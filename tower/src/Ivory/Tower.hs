module Ivory.Tower
  -- Channel API:
  ( ChannelSource
  , ChannelSink
  , ChannelEmitter
  , ChannelReceiver
  , src
  , snk
  , withChannelEmitter
  , withChannelReceiver
  , emit_
  , emitV_
  , receive
  , receiveV
  , withChannelEvent

  -- Task API:
  , Task
  , task
  , taskInit
  , taskLocal
  , taskLocalInit
  , taskModuleDef
  , taskChannel
  , taskChannelWithSize

  -- Tower API:
  , Tower
  , towerArtifact
  , towerModule
  , towerDepends
  , towerGroup
  , channel
  , channelWithSize

  -- Event API:
  , Event
  , handle

  -- Time API:
  , module Ivory.Tower.Types.Time
  , getTime
  , timerEvent

  -- Signal API:
  , Signalable(..)
  , NoSignals
  , withSignalEvent

  -- Artifact API:
  , Artifact(..)

  -- Naming utilities:
  , Unique
  , BaseUtils(fresh, freshname)
  , showUnique
  ) where

import Ivory.Tower.Types.Channels
import Ivory.Tower.Channel

import Ivory.Tower.Task

import Ivory.Tower.Tower

import Ivory.Tower.Event

import Ivory.Tower.Types.Time
import Ivory.Tower.Timer

import Ivory.Tower.Types.Signalable
import Ivory.Tower.Signal

import Ivory.Tower.Types.Artifact

import Ivory.Tower.Monad.Base
import Ivory.Tower.Types.Unique
