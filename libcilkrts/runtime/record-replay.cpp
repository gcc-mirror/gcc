/* record-replay.cpp                  -*-C++-*-
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

/*
 * Implementation of the record/replay functionality for Cilk Plus
 */

#include <cstring>
#include <vector>
#include <stdlib.h>

// clang is really strict about printf formats, so use the annoying integer
// printf macros.  Unfortunately they're not available on Windows (or on VxWorks)
#if defined(_WIN32) || defined(_WRS_KERNEL)
#define PRIu64 "llu"
#else
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
#endif

#include "record-replay.h"
#include "bug.h"
#include "internal/abi.h"
#include "local_state.h"
#include "full_frame.h"
#include "global_state.h"
#include "cilk_malloc.h"
#include "os.h"  // for cilkos_error()

#if RECORD_ON_REPLAY
#pragma message ("*** Record on Replay is enabled!")
#endif

// Defined to write sequence number to the logs.  Note that you cannot
// diff logs with sequence numbers because the numbers may increment in
// different orders.
//#define INCLUDE_SEQUENCE_NUMBER 1

const int PED_VERSION = 1;      // Log recording version

// Log types
enum ped_type_t
{
    ped_type_unknown,
    ped_type_steal,
    ped_type_sync,
    ped_type_orphaned,
    ped_type_last               // Flags end of the list
};

// Log type strings
#define PED_TYPE_STR_STEAL "Steal"
#define PED_TYPE_STR_SYNC "Sync"
#define PED_TYPE_STR_WORKERS "Workers"
#define PED_TYPE_STR_ORPHANED "Orphaned"

#define PED_TYPE_SIZE 16        // Buffer size for the type of pedigree.  Must
                                // hold largest pedigree record type string.
#define PEDIGREE_BUFF_SIZE 512  // Buffer size for the string representation
                                // of a pedigree.

/**
 * Data we store for a replay log entry
 */
typedef struct replay_entry_t
{
    uint64_t *m_reverse_pedigree;   /**< Reverse pedigree for replay log entry */
    ped_type_t m_type;              /**< Type of replay log entry */
    int16_t m_pedigree_len;         /**< Number of terms in reverse pedigree */
    int16_t m_value;                /**< Victim for STEALs, 0 if matching steal found for ORPHANs */

   /**
    * Load data read from the log into the entry
    */
    bool load(const char *type, const char *pedigee_str, int32_t value1, int32_t value2)
    {
        // Convert the type into an enum
        if (0 == strcmp(type, PED_TYPE_STR_STEAL))
        {
            m_type = ped_type_steal;
            m_value = (int16_t)value1;   // Victim
        }
        else
        {
            m_value = -1;      // Victim not valid
            if (0 == strcmp(type, PED_TYPE_STR_SYNC))
                m_type = ped_type_sync;
            else if (0 == strcmp(type, PED_TYPE_STR_ORPHANED))
                m_type = ped_type_orphaned;
            else
            {
                m_type = ped_type_unknown;
                return false;
            }
        }

        // Parse the pedigree
        m_pedigree_len = 0;

        const char *p = pedigee_str;
        char *end;

        uint64_t temp_pedigree[PEDIGREE_BUFF_SIZE/2];

        while(1)
        {
            temp_pedigree[m_pedigree_len++] = (uint64_t)strtol(p, &end, 10);
            if ('\0' == *end)
                break;
            p = end + 1;
        }

        // Allocate memory to hold the pedigree.
        // Copy the pedigree in reverse order since that's the order we'll
        // traverse it
        m_reverse_pedigree =
            (uint64_t *)__cilkrts_malloc(sizeof(int64_t) * m_pedigree_len);
        for (int n = 0; n < m_pedigree_len; n++)
            m_reverse_pedigree[n] = temp_pedigree[(m_pedigree_len - 1) - n];

        return true;
    }

   /**
    * Match this entry against the data supplied.  This includes walking the
    * pedigree from the specified node.
    */
    bool match (ped_type_t type, const __cilkrts_pedigree *node, int victim = -1)
    {
        int i = 0;

        // If the type isn't what they're seeking, we don't have a match
        if (type != m_type)
            return false;

        // If we're looking for a STEAL, then the victim must match
        if ((type == ped_type_steal) && (victim != m_value))
            return false;

        // Compare the current pedigree against what was recorded
        while ((NULL != node) && (i < m_pedigree_len))
        {
            // If we've got a pedigree rank difference, then we don't have
            // a match
            if (node->rank != m_reverse_pedigree[i])
                return false;
            node = node->parent;
            i++;
        }

        // Make sure we exhausted both the pedigree chain and the recorded
        // pedigree
        return ((NULL == node) && (i == m_pedigree_len));
    }

   /**
    * Advance to the next entry, skipping any ORPHANED records we didn't see
    * a matching STEAL for
    */
    replay_entry_t *next_entry()
    {
        replay_entry_t *entry = this;

        // You can't go beyond the end
        if (ped_type_last == entry->m_type)
            return entry;

        // Advance to the next entry
        entry++;

        // Skip any ORPHANED records that don't have a matching steal. We
        // initialized the value field to -1 for ORPHANED.  After loading all
        // the log data, we iterated through all the STEAL records setting the
        // matching ORPHANED record's value field to 0. So if an ORPHANED
        // record's value field is still -1, it doesn't have a matching STEAL
        // record, and I don't know why we chose not to return from the
        // spawned function.
        while ((ped_type_orphaned == entry->m_type) && (-1 == entry->m_value))
        {
            entry++;
        }

        return entry;
    }

   /**
    * Release any allocated resources
    */
    void unload()
    {
        __cilkrts_free(m_reverse_pedigree);
        m_reverse_pedigree = NULL;
    }

} replay_entry_t;

__CILKRTS_BEGIN_EXTERN_C

/**
 * Walk the pedigree and generate a string representation with underscores
 * between terms.  Currently does a recursive walk to generate a forward
 * pedigree.
 *
 * @param p The buffer that is to be filled.  Assumed to be PEDIGREE_BUFF_SIZE
 * characters long
 * @param pnode The initial pedigree term to be written.
 *
 * @return A pointer into the pedigree string buffer after a term has been
 * written.
 */
static
char * walk_pedigree_nodes(char *p, const __cilkrts_pedigree *pnode)
{
    CILK_ASSERT(pnode);
    if (pnode->parent)
    {
        p = walk_pedigree_nodes(p, pnode->parent);
        p += cilk_snprintf_s(p, PEDIGREE_BUFF_SIZE, "%s", (char *)"_");
    }
    return p + cilk_snprintf_l(p, PEDIGREE_BUFF_SIZE, "%" PRIu64, pnode->rank);
}

/**
 * Write a record to a replay log file.
 *
 * @param w The worker we're writing the pedigree for.
 * @param type The type of the pedigree record, as a string
 * @param initial_node The initial pedigree node to be written, or NULL if
 * there is no pedigree for this record type.
 * @param i1 First integer value to be written to the record.
 * @param i2 Second integer value to be written to the record. Only applies
 * to STEAL records. Defaults to -1 (unused).  The second value is always
 * written to make parsing easier.
 */
static
void write_to_replay_log (__cilkrts_worker *w, const char *type,
                          const __cilkrts_pedigree *initial_node,
                          int i1 = -1, int i2 = -1)
{
    char pedigree[PEDIGREE_BUFF_SIZE];

    // If we don't have an initial pedigree node, just use "0" to fill the slot
    if (NULL == initial_node)
        cilk_strcpy_s(pedigree, PEDIGREE_BUFF_SIZE, "0");
    else
        walk_pedigree_nodes(pedigree, initial_node);

#ifndef INCLUDE_SEQUENCE_NUMBER
    // Simply write the record
    fprintf(w->l->record_replay_fptr, "%s %s %d %d\n",
            type, pedigree, i1, i2);
#else
    // Write the record with a sequence number.  The sequence number should
    // always be the last term, and ignored on read

    static long volatile seq_num = 0;
    long write_num;

    // Atomic increment functions are compiler/OS-specific
#ifdef _WIN32
    write_num = _InterlockedIncrement(&seq_num);
#else /* GCC */
    write_num = __sync_add_and_fetch(&seq_num, 1);
#endif // _WIN32

    fprintf(w->l->record_replay_fptr, "%s %s %d %d %ld\n",
            type, pedigree, i1, i2, write_num);
#endif // INCLUDE_SEQUENCE_NUMBER

    fflush(w->l->record_replay_fptr);
}

/**
 * Record data for a successful steal.
 *
 * The pedigree for a STEAL record is the pedigree of the stolen frame.
 *
 * @note It's assumed that replay_record_steal() has already checked that we're
 * recording a log and that the record/replay functionality has not been
 * compiled out.
 *
 * @param w The worker stealing a frame.
 * @param victim_id The ID of the worker which had it's frame stolen.
 */
void replay_record_steal_internal(__cilkrts_worker *w, int32_t victim_id)
{
    // Follow the pedigree chain using worker's stack frame
    CILK_ASSERT(w->l->next_frame_ff);
    CILK_ASSERT(w->l->next_frame_ff->call_stack);

    // Record steal: STEAL pedigree victim_id thief_id
    write_to_replay_log (w, PED_TYPE_STR_STEAL,
                         &(w->l->next_frame_ff->call_stack->parent_pedigree),
                         victim_id);
}

/**
 * Record data for the worker that continues from a sync
 *
 * The pedigree for a SYNC record is the pedigree at the sync.
 *
 * @note It's assumed that replay_record_sync() has already checked that we're
 * recording a log and that the record/replay functionality has not been
 * compiled out.
 *
 * @param w The worker continuing from a sync.
 */
void replay_record_sync_internal(__cilkrts_worker *w)
{
    // Record sync: SYNC pedigree last_worker_id
    write_to_replay_log (w, PED_TYPE_STR_SYNC, &w->pedigree);
}

/**
 * Record the pedigree of an attempt to return to a stolen parent
 *
 * The pedigree for an ORPHANED record is the pedigree of our parent
 *
 * @note It's assumed that replay_record_orphaned() has already checked that
 * we're recording a log and that the record/replay functionality has not
 * been compiled out.
 *
 * @param w The worker continuing noting that it has been orphaned.
 */
void replay_record_orphaned_internal(__cilkrts_worker *w)
{
    // Record steal: ORPHANED pedigree self
    write_to_replay_log (w, PED_TYPE_STR_ORPHANED, w->pedigree.parent);
}

/**
 * Attempt to match a SYNC record.  We have a match when this worker was
 * recorded returning from the current call to __cilkrts_sync() with the
 * same pedigree and this was the worker that continued from the sync, since
 * it was the last to sync.
 *
 * If we find a match, the caller is expected to stall it is the last worker
 * to reach a sync so it will be the worker to continue from the sync.
 *
 * @note It's assumed that replay_match_sync_pedigree() has already returned
 * if we're not replaying a log, or if record/replay functionality has
 * been compiled out.
 *
 * @param w The worker we're checking to see if we've got a match
 */
int replay_match_sync_pedigree_internal(__cilkrts_worker *w)
{
    // Return true if we have a match
    if (w->l->replay_list_entry->match(ped_type_sync, &w->pedigree))
        return 1;
    else
        return 0;
}

/**
 * Advance to the next log entry from a SYNC record.  Consume the current
 * SYNC record on this worker and advance to the next one.
 *
 * @note It's assumed that replay_advance_from_sync() has already returned if
 * we're not replaying a log, or if record/replay functionality has been
 * compiled out.
 *
 * @param w The worker whose replay log we're advancing.
 */
void replay_advance_from_sync_internal (__cilkrts_worker *w)
{
    // The current replay entry must be a SYNC
    CILK_ASSERT(ped_type_sync == w->l->replay_list_entry->m_type);

    // Advance to the next entry
    w->l->replay_list_entry = w->l->replay_list_entry->next_entry();
}

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
 *
 * @return -1 if the next record is not a STEAL
 * @return recorded stolen worker ID if we've got a matching STEAL record
 */
int replay_get_next_recorded_victim_internal(__cilkrts_worker *w)
{
    // If the next record isn't a STEAL, abort the attempt to steal work
    if (ped_type_steal != w->l->replay_list_entry->m_type)
        return -1;

    // Return the victim's worker ID from the STEAL record.  We'll check
    // the pedigree after random_steal has locked the victim worker.
    return w->l->replay_list_entry->m_value;
}

/**
 * Called from random_steal() to determine if we have a STEAL record that
 * matches the pedigree at the head of the victim worker.  If we do have a
 * match, the STEAL record is consumed.
 *
 * @note It's assumed that replay_match_victim_pedigree() has already returned if
 * we're not replaying a log, or if record/replay functionality has been
 * compiled out.
 *
 * @return 1 if we have a match
 * @return 0 if the current replay record isn't a STEAL record, or the victim
 * isn't correct, or the pedigree doesn't match.
 */
int replay_match_victim_pedigree_internal(__cilkrts_worker *w, __cilkrts_worker *victim)
{
    // If we don't have a match, return 0
    if (! w->l->replay_list_entry->match(ped_type_steal,
                                             &((*victim->head)->parent_pedigree),
                                             victim->self))
        return 0;

    // Consume this entry
    w->l->replay_list_entry = w->l->replay_list_entry->next_entry();

    // Return success
    return 1;
}

/**
 * If the frame we're about to return to was recorded as being stolen,
 * stall until it is.
 *
 * @note It's assumed that replay_wait_for_steal_if_parent_was_stolen() has
 * already returned if we're not replaying a log, or if record/replay
 * functionality has been compiled out.
 *
 * @param w The worker we're executing on.
 */
void replay_wait_for_steal_if_parent_was_stolen_internal(__cilkrts_worker *w)
{
    // If our parent wasn't recorded orphanen, return now
    if (! w->l->replay_list_entry->match (ped_type_orphaned,
                                              w->pedigree.parent))
        return;

    // Stall until our parent is stolen.  Note that we're comparing head
    // and tail, not head and exc.  The steal is not completed until tail
    // is modified.
    while (!((w->tail - 1) < w->head))
        __cilkrts_sleep();

    // Consume the entry
    w->l->replay_list_entry = w->l->replay_list_entry->next_entry();
}

/**
 * Allocate memory for the list of logged events.
 *
 * This function will read through the file and count the number of records
 * so it can estimate how big a buffer to allocate for the array or replay
 * entries.  It will then rewind the file to the beginning so it can be
 * loaded into memory.
 *
 * @param w The worker we're loading the file for.
 * @param f The file of replay data we're scanning.
 */
static
void allocate_replay_list(__cilkrts_worker *w, FILE *f)
{
    // Count the number of entries - yeah, it's a hack, but it lets me
    // allocate the space all at once instead of in chunks
    char buf[1024];
    int entries = 1;    // Include "LAST" node

    while (! feof(f))
    {
        if (fgets(buf, 1024, f))
        {
            // Skip the Workers record - should only be in file for Worker 0
            if (0 != strncmp(PED_TYPE_STR_WORKERS, buf, sizeof(PED_TYPE_STR_WORKERS)-1))
                entries++;
        }
    }

    w->l->replay_list_root =
        (replay_entry_t *)__cilkrts_malloc(entries * sizeof(replay_entry_t));
    w->l->replay_list_root[entries - 1].m_type = ped_type_last;

    // Reset the file to the beginning
    rewind(f);
}

/**
 * Load the replay log for a worker into memory.
 *
 * @param w The worker we're loading the replay for.
 */
static
void load_recorded_log(__cilkrts_worker *w)
{
    char ped_type[PED_TYPE_SIZE];
    char ped_str[PEDIGREE_BUFF_SIZE];
    int32_t i1 = -1, i2 = -1;
    int fret;
    char local_replay_file_name[512];
    FILE *f;

    // Open the log for reading
    cilk_snprintf_si(local_replay_file_name, sizeof(local_replay_file_name),
                     "%s%d.cilklog", w->g->record_replay_file_name,  w->self);

    f = fopen(local_replay_file_name, "r");

    // Make sure we found a log!
    CILK_ASSERT (NULL != f);

    // Initialize the replay_list
    allocate_replay_list(w, f);
    replay_entry_t *entry = w->l->replay_list_root;

    // Read the data out and add it to our tables
    while (! feof(f))
    {
#ifndef INCLUDE_SEQUENCE_NUMBER
        fret = fscanf(f, "%s %s %d %d\n", ped_type, ped_str, &i1, &i2);
        if(EOF == fret)
            break;

        // We must have read 4 fields
        CILK_ASSERT(4 == fret);
#else
        int32_t write_num;
        fret = fscanf(f, "%s %s %d %d %d\n", ped_type, ped_str,
                      &i1, &i2, &write_num);
        if(EOF == fret)
            break;

        // We must have read 5 fields
        CILK_ASSERT(5 == fret);
#endif // INCLUDE_SEQUENCE_NUMBER

        // Load the data into the entry
        if (0 == strcmp(ped_type, PED_TYPE_STR_WORKERS))
        {
            // Verify we're replaying with the same number of workers we recorded with
            if (i1 != w->g->P)
            {
                // Fatal error - does not return
                cilkos_error("Cannot continue replay: number of workers(%d) doesn't match "
                             "that from the recording(%d).\n", w->g->P, i1);
            }

            // Verify that we understand this version of the pedigree file
            if (PED_VERSION != i2)
            {
                // Fatal error - does not return
                cilkos_error("Pedigree file version %d doesn't match current "
                             "version %d - cannot continue.\n",
                             i2, PED_VERSION);
            }
        }
        else
        {
            entry->load(ped_type, ped_str, i1, i2);
            entry++;
        }
    }

    // Make sure we've filled the allocated memory.  We initialized the last
    // entry in 
    CILK_ASSERT(ped_type_last == entry->m_type);
    w->l->replay_list_entry = w->l->replay_list_root;

    // Close the log and return
    fclose(f);
}

/**
 * Scan a recorded log to match STEALs againsted ORPHANED records.
 *
 * @param g Cilk Runtime global state.  Passed to access the worker array so
 * we can scan a worker's ORPHANED entries for one that matches a STEAL entry.
 * @param entry The root of a replay_list for a worker.
 */
static
void scan_for_matching_steals(global_state_t *g, replay_entry_t *entry)
{
    // Iterate over all of the entries
    while (ped_type_last != entry->m_type)
    {
        // Look for STEALs.  That will tell us which worker the frame was
        // stolen from
        if (ped_type_steal == entry->m_type)
        {
            bool found = false;

            // Validate the worker ID and make sure we've got a list
            CILK_ASSERT((entry->m_value >= 0) && (entry->m_value < g->total_workers));
            replay_entry_t *victim_entry = g->workers[entry->m_value]->l->replay_list_root;
            CILK_ASSERT(NULL != victim_entry);

            // Scan the victim's list for the matching ORPHANED record
            while ((ped_type_last != victim_entry->m_type) && ! found)
            {
                if (ped_type_orphaned == victim_entry->m_type)
                {
                    if (entry->m_pedigree_len == victim_entry->m_pedigree_len)
                    {
                        if (0 == memcmp(entry->m_reverse_pedigree,
                                        victim_entry->m_reverse_pedigree,
                                        entry->m_pedigree_len * sizeof(int64_t)))
                        {
                            // Note that this ORPHANED record has a matching steal
                            victim_entry->m_value = 0;
                            found = true;
                        }
                    }
                }
                victim_entry++;
            }
        }
        entry++;
    }
}


/*
 * Initialize per-worker data for record or replay - See record-replay.h
 * for full routine header.
 */
void replay_init_workers(global_state_t *g)
{
    int i;
    char worker_file_name[512];

    // If we're not recording or replaying a log, we're done.  All of the
    // fields in the global_state_t or local_state_t are already initialized
    // to default values.
    if (RECORD_REPLAY_NONE == g->record_or_replay)
        return;

    // If we're replaying a log, read each worker's log and construct the
    // in-memory log
    if (REPLAY_LOG == g->record_or_replay)
    {
        // Read all of the data
        for (i = 0; i < g->total_workers; ++i)
        {
            // This function will also initialize and fill the worker's 
            // replay list
            load_recorded_log(g->workers[i]);
        }

        // Scan for orphans with no matching steal.  Mark them so they'll be
        // skipped as we advance through the log.
        for (i = 0; i < g->total_workers; ++i)
        {
            scan_for_matching_steals(g, g->workers[i]->l->replay_list_root);
        }

        // If we're recording the logs while replaying, create the log files.
        // This will only be used for debugging.  Create the logs in the
        // current directory.  It should be as good a place as any...
#if RECORD_ON_REPLAY
        for(i = 0; i < g->total_workers; ++i)
        {
            __cilkrts_worker *w = g->workers[i];
            cilk_snprintf_i(worker_file_name, sizeof(worker_file_name),
                            "replay_log_%d.cilklog",  w->self);
            w->l->record_replay_fptr = fopen(worker_file_name, "w+");
            CILK_ASSERT(NULL != w->l->record_replay_fptr);
        }

        // Record the number of workers, file version in Worker 0's file
        write_to_replay_log (g->workers[0], PED_TYPE_STR_WORKERS, NULL, g->P, PED_VERSION);
#endif // RECORD_ON_REPLAY
    }

    // If we're recording, create the log files
    if (RECORD_LOG == g->record_or_replay)
    {
        for(i = 0; i < g->total_workers; ++i)
        {
            __cilkrts_worker *w = g->workers[i];
            cilk_snprintf_si(worker_file_name, sizeof(worker_file_name),
                             "%s%d.cilklog", g->record_replay_file_name, w->self);
            w->l->record_replay_fptr = fopen(worker_file_name, "w+");
            CILK_ASSERT(NULL != w->l->record_replay_fptr);
        }

        // Record the number of workers, file version in Worker 0's file
        write_to_replay_log (g->workers[0], PED_TYPE_STR_WORKERS, NULL, g->P, PED_VERSION);
    }
}

/*
 * Do any necessary cleanup for the logs - See record-replay.h for full
 * routine header.
 */
void replay_term(global_state_t *g)
{
    // Free memory for the record/replay log file name, if we've got one
    if (g->record_replay_file_name)
        __cilkrts_free(g->record_replay_file_name);

    // Per-worker cleanup
    for(int i = 0; i < g->total_workers; ++i)
    {
        __cilkrts_worker *w = g->workers[i];

        // Close the log files, if we've opened them
        if(w->l->record_replay_fptr)
            fclose(w->l->record_replay_fptr);

        if (w->l->replay_list_root)
        {
            // We should have consumed the entire list
            CILK_ASSERT(ped_type_last == w->l->replay_list_entry->m_type);

            replay_entry_t *entry = w->l->replay_list_root;
            while (ped_type_last != entry->m_type)
            {
                // Free the pedigree memory for each entry
                entry->unload();
                entry++;
            }
            __cilkrts_free(w->l->replay_list_root);
            w->l->replay_list_root = NULL;
            w->l->replay_list_entry = NULL;
        }
    }
}

__CILKRTS_END_EXTERN_C
