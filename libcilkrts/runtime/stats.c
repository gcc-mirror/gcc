/* stats.c                  -*-C-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2009-2013, Intel Corporation
 *  All rights reserved.
 *  
 *  @copyright
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
 *  @copyright
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
 **************************************************************************/

#include "stats.h"
#include "bug.h"
#include "os.h"
#include "local_state.h"

#include <stdio.h>

#define INVALID_START (0ULL - 1ULL)

#ifdef CILK_PROFILE
/* MSVC does not support designated initializers, grrrr... */
static const char *names[] = {
    /*[INTERVAL_IN_SCHEDULER]*/                 "in scheduler",
    /*[INTERVAL_WORKING]*/                      "  of which: working",
    /*[INTERVAL_IN_RUNTIME]*/                   "  of which: in runtime",
    /*[INTERVAL_STEALING]*/                     "     of which: stealing",
    /*[INTERVAL_STEAL_SUCCESS]*/                "steal success: detach",
    /*[INTERVAL_STEAL_FAIL_EMPTYQ]*/            "steal fail: empty queue",
    /*[INTERVAL_STEAL_FAIL_LOCK]*/              "steal fail: victim locked",
    /*[INTERVAL_STEAL_FAIL_USER_WORKER]*/       "steal fail: user worker",
    /*[INTERVAL_STEAL_FAIL_DEKKER]*/            "steal fail: dekker",
    /*[INTERVAL_SYNC_CHECK]*/                   "sync check",
    /*[INTERVAL_THE_EXCEPTION_CHECK]*/          "THE exception check",
    /*[INTERVAL_THE_EXCEPTION_CHECK_USELESS]*/  "  of which: useless",
    /*[INTERVAL_RETURNING]*/                    "returning",
    /*[INTERVAL_FINALIZE_CHILD]*/               "finalize child",
    /*[INTERVAL_PROVABLY_GOOD_STEAL]*/          "provably good steal",
    /*[INTERVAL_UNCONDITIONAL_STEAL]*/          "unconditional steal",
    /*[INTERVAL_ALLOC_FULL_FRAME]*/             "alloc full frame",
    /*[INTERVAL_FRAME_ALLOC_LARGE]*/            "large frame alloc",
    /*[INTERVAL_FRAME_ALLOC]*/                  "small frame alloc",
    /*[INTERVAL_FRAME_ALLOC_GLOBAL]*/           "  of which: to global pool",
    /*[INTERVAL_FRAME_FREE_LARGE]*/             "large frame free",
    /*[INTERVAL_FRAME_FREE]*/                   "small frame free",
    /*[INTERVAL_FRAME_FREE_GLOBAL]*/            "  of which: to global pool",
    /*[INTERVAL_MUTEX_LOCK]*/                   "mutex lock",
    /*[INTERVAL_MUTEX_LOCK_SPINNING]*/          "  spinning",
    /*[INTERVAL_MUTEX_LOCK_YIELDING]*/          "  yielding",
    /*[INTERVAL_MUTEX_TRYLOCK]*/                "mutex trylock",
    /*[INTERVAL_FIBER_ALLOCATE]*/               "fiber_allocate",
    /*[INTERVAL_FIBER_DEALLOCATE]*/             "fiber_deallocate", 
    /*[INTERVAL_FIBER_ALLOCATE_FROM_THREAD]*/   "fiber_allocate_from_thread",
    /*[INTERVAL_FIBER_DEALLOCATE_FROM_THREAD]*/ "fiber_deallocate (thread)", 
    /*[INTERVAL_SUSPEND_RESUME_OTHER]*/         "fiber suspend self + resume",
    /*[INTERVAL_DEALLOCATE_RESUME_OTHER]*/      "fiber deallocate self + resume", 
};
#endif

void __cilkrts_init_stats(statistics *s)
{
    int i;
    for (i = 0; i < INTERVAL_N; ++i) {
        s->start[i] = INVALID_START;
        s->accum[i] = 0;
        s->count[i] = 0;
    }

    s->stack_hwm = 0;
}

#ifdef CILK_PROFILE
void __cilkrts_accum_stats(statistics *to, statistics *from)
{
    int i;

    for (i = 0; i < INTERVAL_N; ++i) {
        to->accum[i] += from->accum[i];
        to->count[i] += from->count[i];
        from->accum[i] = 0;
        from->count[i] = 0;
    }

    if (from->stack_hwm > to->stack_hwm)
        to->stack_hwm = from->stack_hwm;
    from->stack_hwm = 0;
}

void __cilkrts_note_interval(__cilkrts_worker *w, enum interval i)
{
    if (w) {
        statistics *s = w->l->stats;
        CILK_ASSERT(s->start[i] == INVALID_START);
        s->count[i]++;
    }
}

void __cilkrts_start_interval(__cilkrts_worker *w, enum interval i)
{
    if (w) {
        statistics *s = w->l->stats;
        CILK_ASSERT(s->start[i] == INVALID_START);
        s->start[i] = __cilkrts_getticks();
        s->count[i]++;
    }
}

void __cilkrts_stop_interval(__cilkrts_worker *w, enum interval i)
{
    if (w) {
        statistics *s = w->l->stats;
        CILK_ASSERT(s->start[i] != INVALID_START);
        s->accum[i] += __cilkrts_getticks() - s->start[i];
        s->start[i] = INVALID_START;
    }
}

void dump_stats_to_file(FILE *stat_file, statistics *s)
{
    int i;
    fprintf(stat_file, "\nCILK PLUS RUNTIME SYSTEM STATISTICS:\n\n");

    fprintf(stat_file,
            "  %-32s: %15s %10s %12s %10s\n",
            "event",
            "count",
            "ticks",
            "ticks/count",
            "%total"
        );
    for (i = 0; i < INTERVAL_N; ++i) {
        fprintf(stat_file, "  %-32s: %15llu", names[i], s->count[i]);
        if (s->accum[i]) {
            fprintf(stat_file, " %10.3g %12.3g %10.2f",
                    (double)s->accum[i],
                    (double)s->accum[i] / (double)s->count[i],
                    100.0 * (double)s->accum[i] / 
                    (double)s->accum[INTERVAL_IN_SCHEDULER]);
        }
        fprintf(stat_file, "\n");
    }
}
#endif // CILK_PROFILE

/* End stats.c */
