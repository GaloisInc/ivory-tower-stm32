{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32.Driver.CAN.Sched
  ( CANTask()
  , canTask
  , canScheduler
  ) where

import Control.Monad (forM, forM_)
import Ivory.BSP.STM32.Driver.CAN
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

[ivory|
struct can_transmit_result
  { task_idx :: Stored Uint8
  ; task_success :: Stored IBool
  }

struct can_reschedule_request
  { reschedule_mailbox :: Stored Uint8
  ; reschedule_task :: Stored Uint8
  }
|]

shiftUp :: Def ('[ Ref s0 ('Stored Uint8)
                 , Ref s1 ('Stored Uint32), Ref s2 ('Stored Uint8)
                 , Ref s3 ('Stored Uint32), Ref s4 ('Stored Uint8)
                 ] :-> IBool)
shiftUp = proc "shift_task_up" $ \ insert_position new_prio new_task current_prio current_task -> body $ do
  new <- deref new_prio
  when (new ==? maxBound) $ ret true
  current <- deref current_prio
  assert (new /=? current)
  ifte_ (new >? current) (insert_position %= (+ 1)) $ do
    temp_task <- deref current_task
    refCopy current_prio new_prio
    refCopy current_task new_task
    store new_prio current
    store new_task temp_task
  ret false

shiftDown :: Def ('[ Ref s0 ('Stored Uint8)
                   , Ref s1 ('Stored Uint32), Ref s2 ('Stored Uint8)
                   , ConstRef s3 ('Stored Uint32), ConstRef s4 ('Stored Uint8)
                   ] :-> IBool)
shiftDown = proc "shift_task_down" $ \ target_ref current_prio current_task next_prio next_task -> body $ do
  target <- deref target_ref
  when (target ==? maxBound) $ ret true
  current <- deref current_task
  when (current ==? target) $ do
    refCopy current_prio next_prio
    refCopy current_task next_task
    comment "We just duplicated the next task, so arrange to delete that next."
    refCopy target_ref next_task
  ret false

schedulerHelperModule :: Module
schedulerHelperModule = package "can_scheduler_helper" $ do
  defStruct (Proxy :: Proxy "can_transmit_result")
  defStruct (Proxy :: Proxy "can_reschedule_request")
  incl shiftUp
  incl shiftDown

data CANTask = CANTask
  { canTaskReq :: ChanOutput (Struct "can_transmit_request")
  , canTaskRes :: ChanInput (Stored IBool)
  , canTaskAbortReq :: ChanOutput (Stored IBool)
  }

canTask :: Tower e (CANTask, CANTransmitAPI)
canTask = do
  (canTXReq, canTaskReq) <- channel
  (canTaskRes, canTXRes) <- channel
  (canTXAbortReq, canTaskAbortReq) <- channel
  return (CANTask { .. }, CANTransmitAPI { .. })

canScheduler :: [CANTransmitAPI]
             -> [CANTask]
             -> Tower e ()
canScheduler mailboxes tasks = do
  (doResched, reschedChan) <- channel
  (doTaskComplete, taskCompleteChan) <- channel
  (doTaskAbort, taskAbortChan) <- channel

  towerModule schedulerHelperModule
  towerDepends schedulerHelperModule

  monitor "can_scheduler" $ do
    -- Maintain a priority queue of tasks. The priority is stored in
    -- state variables in prio_queue, and the corresponding task index
    -- in task_queue. We use linear-time insertion in sorted order to
    -- maintain the priority queue invariants, rather than a more
    -- complex data structure like a heap.
    --
    -- The state variables are allocated in separate groups to minimize
    -- padding waste, since task-ID is 8-bit and priority is 32-bit.
    --
    -- The highest-priority tasks are those which *should* be in the
    -- hardware mailboxes right now. However, since we have to wait for
    -- completion notifications after hardware aborts, the current
    -- contents of each mailbox can lag behind the current set of
    -- highest priority tasks.

    prio_queue <- forM (zipWith const [(0 :: Int) ..] tasks) $ \ idx -> do
      stateInit ("prio_" ++ show idx) $ ival maxBound

    sentinel_prio <- fmap constRef $ stateInit "prio_sentinel" $ ival maxBound

    task_queue <- forM (zipWith const [(0 :: Int) ..] tasks) $ \ idx -> do
      stateInit ("task_" ++ show idx) $ ival maxBound

    sentinel_task <- fmap constRef $ stateInit "task_sentinel" $ ival maxBound

    -- For each mailbox, we need to track which task is actually in the
    -- mailbox right now. That's the task to notify when that mailbox
    -- reports completion. One extra value (maxBound) is reserved to
    -- indicate that the mailbox is currently empty.
    mbox_states <- forM (zip [0..] mailboxes) $ \ (idx, mbox) -> do
      current <- stateInit ("current_task_in_" ++ show idx) $ ival maxBound
      return (idx, mbox, current)

    -- Procedures for manipulating the priority queue:

    let isTaskQueued = proc "is_task_queued" $ \ task -> body $ do
          forM_ task_queue $ \ current_task -> do
            current <- deref current_task
            when (current ==? maxBound) $ ret false
            when (current ==? task) $ ret true
          ret false

    let isTaskCurrent = proc "is_task_current" $ \ task -> body $ do
          forM_ mbox_states $ \ (_, _, current) -> do
            current_task <- deref current
            when (task ==? current_task) $ ret true
          ret false

    let nextTask = proc "next_task" $ body $ do
          forM_ (zipWith const task_queue mbox_states) $ \ task -> do
            target_task <- deref task
            comment "Stop at the end of the list."
            when (target_task ==? maxBound) $ ret maxBound
            comment "Skip tasks that are already on the hardware."
            is_current <- call isTaskCurrent target_task
            unless is_current $ ret target_task
          ret maxBound

    let insertTask :: Def ('[ Uint8
                            , Ref s1 ('Struct "can_reschedule_request")
                            , Ref s ('Struct "can_transmit_request")
                            , ConstRef sFrom0 ('Struct "can_transmit_request")
                            ] :-> IBool)
        insertTask = proc "insert_task" $ \ task resched_req last_request req -> body $ do
          comment "Task must not have an outstanding request already."
          last_id <- deref $ last_request ~> tx_id
          assert (last_id ==? maxBound)

          comment "Save this request until we can deliver it."
          refCopy last_request req

          insert_position_ref <- local (izero :: Init (Stored Uint8))

          new_prio <- local =<< do
            req_id <- deref $ req ~> tx_id
            req_ide <- deref $ req ~> tx_ide
            req_rtr <- deref $ req ~> tx_rtr
            -- CAN arbitration rules:
            -- * Higher priorities have lower numeric values.
            -- * Convert standard-frame 11-bit IDs to 29 bits by
            --   choosing the highest-priority 29-bit ID that has the
            --   same most-significant 11 bits as the original ID. In
            --   other words, pad with 18 zeroes on the right.
            -- * Standard frames are higher priority than extended
            --   frames with the same 29-bit ID, so append a 1 for
            --   extended frames or a 0 for standard frames.
            -- * Remote frames are lower priority than data frames of
            --   the same ID, so append a 1 for remote frames or a 0 for
            --   data frames.
            return $ ival $
              (req_ide ? ((req_id `iShiftL` 2) .| 2, req_id `iShiftL` 20)) .|
              (req_rtr ? (1, 0))
          new_task <- local $ ival task

          let checkPlace (current_prio, current_task) next = do
                done <- call shiftUp insert_position_ref new_prio new_task current_prio current_task
                unless done next
          foldr checkPlace (return ()) $ zip prio_queue task_queue

          comment "Check if we overflowed the queue and still have a task left to insert."
          final_task <- deref new_task
          assert (final_task ==? maxBound)

          insert_position <- deref insert_position_ref

          let positions = fromIntegral (length tasks)
          let mbox_count = fromIntegral (length mailboxes)
          assert (insert_position <? positions)

          when (positions <=? mbox_count .|| insert_position <? mbox_count) $ do
            comment "Priority is high enough to get a mailbox immediately."

            bounce_task <- case drop (length mailboxes) task_queue of
              [] -> do
                comment "No more tasks than mailboxes, so there must be a free mailbox."
                return maxBound
              bounce_task : _ -> do
                comment "Reschedule the task that we just shoved out of the high-priority group."
                deref bounce_task

            when (bounce_task ==? maxBound) $ do
              comment "Put this new task in a free mailbox."
              store (resched_req ~> reschedule_task) task
            conds <- forM mbox_states $ \ (mbox_idx, _, current) -> do
              pending_task <- deref current
              return
                (pending_task ==? bounce_task ==> do
                  store (resched_req ~> reschedule_mailbox) (fromInteger mbox_idx)
                  ret true
                )
            cond_ conds

          ret false

    let removeTask = proc "remove_task" $ \ initial_task -> body $ do
          target <- local $ ival initial_task

          let current_queue = zip prio_queue task_queue
          let next_queue = [ (constRef prio, constRef task) | (prio, task) <- drop 1 current_queue ] ++ [(sentinel_prio, sentinel_task)]

          let checkPlace ((current_prio, current_task), (next_prio, next_task)) next = do
                done <- call shiftDown target current_prio current_task next_prio next_task
                unless done next
          foldr checkPlace (return ()) (zip current_queue next_queue)

          final_task <- deref target
          when (initial_task ==? final_task) $ do
            comment "Task not found, hopefully because it was previously aborted."
            ret false

          comment "Task found; check that we reached the end of the list."
          assert (final_task ==? maxBound)
          ret true

    monitorModuleDef $ do
      incl isTaskQueued
      incl nextTask
      incl insertTask
      incl removeTask
      private $ do
        incl isTaskCurrent

    -- Channel handlers:

    forM_ mbox_states $ \ (idx, mbox, current) -> do
      handler (canTXRes mbox) ("mailbox_" ++ show idx ++ "_complete") $ do
        taskComplete <- emitter doTaskComplete 1
        resched <- emitter doResched 1

        callbackV $ \ success -> do
          current_task <- deref current
          assert (current_task /=? maxBound)

          res <- fmap constRef $ local $ istruct
            [ task_idx .= ival current_task
            , task_success .= ival success
            ]

          ifte_ success
            (do
              comment "On success, always report back to the task."
              call_ removeTask current_task
              emit taskComplete res
            ) (do
              comment "On failure: did the task abort, or are we rescheduling?"
              still_queued <- call isTaskQueued current_task
              unless still_queued $ do
                comment "Task aborted and is no longer queued. Complete it."
                emit taskComplete res
            )

          store current maxBound

          next <- call nextTask
          when (next /=? maxBound) $ do
            resched_req <- fmap constRef $ local $ istruct
              [ reschedule_mailbox .= ival (fromInteger idx)
              , reschedule_task .= ival next
              ]
            emit resched resched_req

    handler taskAbortChan "task_abort" $ do
      emitters <- forM mbox_states $ \ (_, mbox, current) -> do
        e <- emitter (canTXAbortReq mbox) 1
        return (current, e)
      taskComplete <- emitter doTaskComplete 1
      callbackV $ \ task -> do
        removed <- call removeTask task
        when removed $ do
          -- If this task is current in some mailbox, abort that mailbox.
          abort_msg <- fmap constRef $ local $ ival true
          current_conds <- forM emitters $ \ (current, e) -> do
            current_task <- deref current
            return (task ==? current_task ==> emit e abort_msg)

          -- Otherwise, we hadn't handed the task off to the hardware yet,
          -- so we can immediately report that it wasn't sent, without
          -- waiting for a hardware abort.
          cond_ $ current_conds ++ [ true ==> do
              res <- fmap constRef $ local $ istruct
                [ task_idx .= ival task
                , task_success .= ival false
                ]
              emit taskComplete res
            ]

    task_states <- forM (zip [0..] tasks) $ \ (idx, task) -> do
      -- We buffer one request from each task. They aren't allowed to
      -- send another until we send them a completion notification,
      -- although they can trigger that early by sending us an abort
      -- request. A sentinel message ID (maxBound) indicates that there
      -- is no pending request from this task.
      last_request <- stateInit ("last_request_for_" ++ show idx) $ istruct
        [ tx_id .= ival maxBound ]

      handler (canTaskReq task) ("task_" ++ show idx ++ "_request") $ do
        resched <- emitter doResched 1
        callback $ \ req -> do
          resched_req <- local $ istruct
            [ reschedule_mailbox .= ival maxBound
            , reschedule_task .= ival maxBound
            ]
          needs_resched <- call insertTask (fromInteger idx) resched_req last_request req
          when needs_resched $ emit resched $ constRef resched_req

      handler (canTaskAbortReq task) ("task_" ++ show idx ++ "_abort") $ do
        taskAbort <- emitter doTaskAbort 1
        callback $ const $ emitV taskAbort $ fromInteger idx

      return (idx, task, last_request)

    handler reschedChan "task_resched" $ do
      emitters <- forM mbox_states $ \ (idx, mbox, current) -> do
        sendReq <- emitter (canTXReq mbox) 1
        abort <- emitter (canTXAbortReq mbox) 1
        return (idx, current, sendReq, abort)

      callback $ \ resched_req -> do
        task <- deref $ resched_req ~> reschedule_task
        task_req <- cond $
          [ task ==? fromInteger idx ==> return (refToPtr last_request)
          | (idx, _, last_request) <- task_states
          ] ++
          [ true ==> do
              assert (task ==? maxBound)
              return nullPtr
          ]

        abortReq <- fmap constRef $ local $ ival true
        mailbox <- deref $ resched_req ~> reschedule_mailbox
        cond_
          [ mailbox ==? fromInteger idx ==> do
              withRef task_req
                (\ ref -> do
                  store current task
                  emit sendReq $ constRef ref
                ) (emit abort abortReq)
          | (idx, current, sendReq, abort) <- emitters
          ]

    handler taskCompleteChan "task_complete" $ do
      emitters <- forM task_states $ \ (idx, task, last_request) -> do
        e <- emitter (canTaskRes task) 1
        return (idx, e, last_request)

      callback $ \ res -> do
        task <- deref $ res ~> task_idx
        assert (task <? fromIntegral (length tasks))
        cond_
          [ task ==? fromInteger idx ==> do
              store (last_request ~> tx_id) maxBound
              emit e $ res ~> task_success
          | (idx, e, last_request) <- emitters
          ]
