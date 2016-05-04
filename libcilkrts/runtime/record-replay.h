/* record_replay.h                  -*-C++-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2012-2016, Intel Corporation
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *  
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *  
 *  *********************************************************************
 *  
 *  PLEASE NOTE: This file is a downstream copy of a file mainitained in
 *  a repository at cilkplus.org. Changes made to this file that are not
 *  submitted through the contribution process detailed at
 *  http://www.cilkplus.org/submit-cilk-contribution will be lost the next
 *  time that a new version is released. Changes only submitted to the
 *  GNU compiler collection or posted to the git repository at
 *  https://bitbucket.org/intelcilkruntime/intel-cilk-runtime.git are
 *  not tracked.
 *  
 *  We welcome your contributions to this open source project. Thank you
 *  for your assistance in helping us improve Cilk Plus.
 *
 **************************************************************************/

/**
 * @file record-replay.h
 *
 * @brief record-replay.h and .cpp encapsulate most of the functionality to
 * record and play back a Cilk Plus application.
 *
 * Recording is directed by the setting of the CILK_RECORD_LOG environment
 * variable.  If it's defined, the value specifies the root we'll use to
 * generate files for each worker using the following format string:
 * "%s%d.cilklog", where the integer is the value of w->self.
 *
 * Replay is directed by the setting of the CILK_REPLAY_LOG environment
 * variable, interpreted the same way as CILK_RECORD_LOG.  If both
 * CILK_RECORD_LOG and CILK_REPLAY_LOG are defined, a warning will be given
 * and the attempt to record a log will be ignored.
 *
 * Recording is relatively straightforward.  We write all information about a
 * worker to a per-worker file.
 *
 * Each pedigree record consists of the following fields.  All fields must be
 * present in every record to make parsing easy.
 *    - Type - A string identifying the pedigree record.  See the PED_TYPE_STR_
 *      macros for the currently defined values.
 *    - Pedigree - A string of pedigree values, with underscores between
 *      adjacent values.
 *    - i1 - Record type-specific value.  -1 if not used.
 *    - i2 - Record type-specific value.  -1 if not used.
 *
 * WORKERS record - only written to the file for worker 0.  Note that this is
 * the first worker in the workers array. Worker 0 is the first system worker,
 * *NOT* a user worker.
 *  - Type: "Workers"
 *  - Pedigree: Always "0" - ignored
 *  - i1: Number of workers (g->P) when we recorded the log.  A mismatch when
 *        we attempt to replay the log will result in aborting the execution.
 *  - i2: Log version number - Specified by PED_VERSION in record-replay.cpp
 *
 * STEAL record - written after a successful steal.
 *  - Type: "Steal"
 *  - Pedigree: Pedigree of stolen frame
 *  - i1: Worker the frame was stolen from
 *  - i2: -1
 *
 * SYNC record - written after a worker continues from a sync.
 *  - Type: "Sync"
 *  - Pedigree: Pedigree of sync.  Note that this is the pedigree *before*
 *        the pedigree in incremented in setup_for_execution_pedigree().
 *  - i1: -1
 *  - i2: -1
 *
 * ORPHANED record - saved on a return to a stolen parent.
 *  - Type: "Orphaned"
 *  - Pedigree: Pedigree of the parent frame *before* the pedigree is
 *        incremented by the return
 *  - i1: -1
 *  - i2: -1
 *
 * On replay, the data is loaded into a per-worker array, and the data is
 * consumed in order as needed.
 */

#ifndef INCLUDED_RECORD_REPLAY_DOT_H
#define INCLUDED_RECORD_REPLAY_DOT_H

#include "cilk/common.h"
#include "global_state.h"

/**
 * Define CILK_RECORD_REPLAY to enable record/replay functionality.  If
 * CILK_RECORD_REPLAY is not defined, all of the record/replay functions in
 * record-replay.h will be stubbed out.  Since they're declared as inline,
 * functions, the resulting build should have no performance impact due to
 * the implementation or record/replay.
 */
 #define CILK_RECORD_REPLAY 1

/**
 * Define RECORD_ON_REPLAY=1 to write logs when we're replaying a log.  This
 * should only be needed when debugging the replay functionality.  This should
 * always be defined as 0 when record-replay.h is checked in.
 */
#define RECORD_ON_REPLAY 0

__CILKRTS_BEGIN_EXTERN_C

#ifdef CILK_RECORD_REPLAY
// Declarations of internal record/replay functions.  The inlined versions
// further down do some preliminary testing (like if we're not recording or
// replaying) and will stub out the functionality if we've compiled out the
// record/replay feature
int replay_match_sync_pedigree_internal(__cilkrts_worker *w);
void replay_wait_for_steal_if_parent_was_stolen_internal(__cilkrts_worker *w);
void replay_record_steal_internal(__cilkrts_worker *w, int32_t victim_id);
void replay_record_sync_internal(__cilkrts_worker *w);
void replay_record_orphaned_internal(__cilkrts_worker *w);
int replay_match_victim_pedigree_internal(__cilkrts_worker *w, __cilkrts_worker *victim);
void replay_advance_from_sync_internal (__cilkrts_worker *w);
int replay_get_next_recorded_victim_internal(__cilkrts_worker *w);
#endif //  CILK_RECORD_REPLAY

// Publically defined record/replay API

/**
 * If we're replaying a log, wait for our parent to be stolen if it was when
 * the log was recorded.  If record/replay is compiled out, this is a noop.
 *
 * @param w The __cilkrts_worker we're executing on.  The worker's replay
 * list will be checked for a ORPHANED record with a matching pedigree.  If
 * there is a match, the ORPHANED record will be consumed.
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
void replay_wait_for_steal_if_parent_was_stolen(__cilkrts_worker *w)
{
    // Only check if we're replaying a log
    if (REPLAY_LOG == w->g->record_or_replay)
        replay_wait_for_steal_if_parent_was_stolen_internal(w);
}
#else
__CILKRTS_INLINE
void replay_wait_for_steal_if_parent_was_stolen(__cilkrts_worker *w)
{
    // If record/replay is disabled, we never wait
}
#endif //  CILK_RECORD_REPLAY

/**
 * Called from random_steal() to override the ID of the randomly chosen victim
 * worker which this worker will attempt to steal from. Returns the worker id
 * of the next victim this worker was recorded stealing from, or -1 if the
 * next record in the log is not a STEAL.
 *
 * @note This call does NOT attempt to match the pedigree.  That will be done
 * by replay_match_victim_pedigree() after random_steal() has locked the victim
 * worker.
 *
 * @param w The __cilkrts_worker we're executing on.  The worker's replay log
 * is checked for a STEAL record.  If we've got one, the stolen worker ID is
 * returned.
 * @param id The randomly chosen victim worker ID.  If we're not replaying a
 * log, or if record/replay has been compiled out, this is the value that
 * will be returned.
 *
 * @return id if we're not replaying a log
 * @return -1 if the next record is not a STEAL
 * @return recorded stolen worker ID if we've got a matching STEAL record
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
int replay_get_next_recorded_victim(__cilkrts_worker *w, int id)
{
    // Only check if we're replaying a log
    if (REPLAY_LOG == w->g->record_or_replay)
        return replay_get_next_recorded_victim_internal(w);
    else
        return id;
}
#else
__CILKRTS_INLINE
int replay_get_next_recorded_victim(__cilkrts_worker *w, int id)
{
    // Record/replay is disabled.  Always return the original worker id
    return id;
}
#endif //  CILK_RECORD_REPLAY

/**
 * Initialize per-worker data for record/replay.  A noop if record/replay
 * is disabled, or if we're not recording or replaying anything.
 *
 * If we're recording a log, this will ready us to create the per-worker
 * logs.
 *
 * If we're replaying a log, this will read the logs into the per-worker
 * structures.
 *
 * @param g Cilk runtime global state
 */
void replay_init_workers(global_state_t *g);

/**
 * Record a record on a successful steal.  A noop if record/replay is
 * diabled, or if we're not recording anything
 *
 * @param w The __cilkrts_worker we're executing on.  The pedigree of
 * the stolen frame will be walked to generate the STEAL record.
 *
 * @param victim_id The worker ID of the worker w stole from.
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
void replay_record_steal(__cilkrts_worker *w, int32_t victim_id)
{
#if RECORD_ON_REPLAY
    // If we're recording on replay, write the record if we're recording or
    // replaying
    if (RECORD_REPLAY_NONE == w->g->record_or_replay)
        return;
#else
    // Only write the record if we're recording
    if (RECORD_LOG != w->g->record_or_replay)
        return;
#endif

    replay_record_steal_internal(w, victim_id);
}
#else
__CILKRTS_INLINE
void replay_record_steal(__cilkrts_worker *w, int32_t victim_id)
{
}
#endif //  CILK_RECORD_REPLAY

/**
 * Record a record when continuing after a sync.  A noop if record/replay is
 * diabled, or if we're not recording anything, or if the sync was abandoned,
 * meaning this isn't the worker that continues from the sync.
 *
 * @param w The __cilkrts_worker for we're executing on.  The pedigree of
 * the sync-ing frame will be walked to generate the SYNC record.
 *
 * @param continuing True if this worker will be continuing from the
 * cilk_sync.  A SYNC record will only be generated if continuing is true.
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
void replay_record_sync(__cilkrts_worker *w, int continuing)
{
    // If this was not the last worker to the syn, return
    if (! continuing)
        return;

#if RECORD_ON_REPLAY
    // If we're recording on replay, write the record if we're recording or
    // replaying
    if (RECORD_REPLAY_NONE == w->g->record_or_replay)
        return;
#else
    // Only write the record if we're recording
    if (RECORD_LOG != w->g->record_or_replay)
        return;
#endif

    replay_record_sync_internal(w);
}
#else
__CILKRTS_INLINE
void replay_record_sync(__cilkrts_worker *w, int abandoned)
{
}
#endif //  CILK_RECORD_REPLAY

/**
 * Record a record on a return to a stolen parent.  A noop if record/replay is
 * diabled, or if we're not recording anything.
 *
 * @param w The __cilkrts_worker for we're executing on.  The pedigree of the
 * frame that has discovered that its parent has been stolken will be walked
 * to generate the ORPHANED record.
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
void replay_record_orphaned(__cilkrts_worker *w)
{
#if RECORD_ON_REPLAY
    // If we're recording on replay, write the record if we're recording or
    // replaying
    if (RECORD_REPLAY_NONE == w->g->record_or_replay)
        return;
#else
    // Only write the record if we're recording
    if (RECORD_LOG != w->g->record_or_replay)
        return;
#endif

    replay_record_orphaned_internal(w);
}
#else
__CILKRTS_INLINE
void replay_record_orphaned(__cilkrts_worker *w)
{
}
#endif //  CILK_RECORD_REPLAY

/**
 * Test whether the frame at the head of the victim matches the pedigree of
 * the frame that was recorded being stolen.  Called in random steal to verify
 * that we're about to steal the correct frame.
 *
 * @param w The __cilkrts_worker for we're executing on.  The current worker
 * is needed to find the replay entry to be checked.
 *
 * @param victim The __cilkrts_worker for we're proposing to steal a frame
 * from.  The victim's head entry is 
 * is needed to find the replay entry to be checked.
 *
 * @return 0 if we're replaying a log and the victim's pedigree does NOT match
 * the next frame the worker is expected to steal.
 *
 * @return 1 in all other cases to indicate that the steal attempt should
 * continue
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
int replay_match_victim_pedigree(__cilkrts_worker *w, __cilkrts_worker *victim)
{
    // We're not replaying a log. The victim is always acceptable
    if (REPLAY_LOG != w->g->record_or_replay)
        return 1;

    // Return 1 if the victim's pedigree matches the frame the worker stole
    // when we recorded the log
    return replay_match_victim_pedigree_internal(w, victim);
}
#else
__CILKRTS_INLINE
int replay_match_victim_pedigree(__cilkrts_worker *w, __cilkrts_worker *victim)
{
    // Record/replay is disabled.  The victim is always acceptable
    return 1;
}
#endif //  CILK_RECORD_REPLAY

/**
 * Test whether the current replay entry is a sync record matching the
 * worker's pedigree.
 *
 * @param w The __cilkrts_worker for we're executing on.
 *
 * @return 1 if the current replay entry matches the current pedigree.
 * @return 0 if there's no match, or if we're not replaying a log.
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
int replay_match_sync_pedigree(__cilkrts_worker *w)
{
    // If we're not replaying, assume no match
    if (REPLAY_LOG != w->g->record_or_replay)
        return 0;

    return replay_match_sync_pedigree_internal(w);
}
#else
__CILKRTS_INLINE
int replay_match_sync_pedigree(__cilkrts_worker *w)
{
    // Record/replay is disabled.  Assume no match
    return 0;
}
#endif

/**
 * Marks a sync record seen, advancing to the next record in the replay list.
 *
 * This function will only advance to the next record if:
 *   - Record/replay hasn't been compiled out AND
 *   - We're replaying a log AND
 *   - A match was found AND
 *   - The sync is not being abandoned
 *
 * @param w The __cilkrts_worker for we're executing on.
 * @param match_found The value returned by replay_match_sync_pedigree().  If
 * match_found is false, nothing is done.
 * @param continuing  Flag indicating whether this worker will continue from
 * the sync (it's the last worker to the sync) or if it will abandon the work
 * and go to the scheduling loop to look for more work it can steal.
 */
#ifdef CILK_RECORD_REPLAY
__CILKRTS_INLINE
void replay_advance_from_sync(__cilkrts_worker *w, int match_found, int continuing)
{
    // If we're replaying a log, and the current sync wasn't abandoned, and we
    // found a match in the log, mark the sync record seen.
    if ((REPLAY_LOG == w->g->record_or_replay) && match_found && continuing)
        replay_advance_from_sync_internal(w);
}
#else
__CILKRTS_INLINE
void replay_advance_from_sync(__cilkrts_worker *w, int match_found, int continuing)
{
}
#endif

/**
 * Release any resources used to read or write a replay log.
 *
 * @param g Cilk runtime global state
 */
void replay_term(global_state_t *g);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_RECORD_REPLAY_DOT_H)
