/* worker_mutex.c                  -*-C-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2009-2016, Intel Corporation
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
 **************************************************************************/

#include "worker_mutex.h"
#include "bug.h"
#include "os.h"
#include "stats.h"

/* m->lock == 1 means that mutex M is locked */
#define TRY_ACQUIRE(m) (__cilkrts_xchg(&(m)->lock, 1) == 0)

/* ICC 11.1+ understands release semantics and generates an
   ordinary store with a software memory barrier. */
#if __ICC >= 1110
#define RELEASE(m) __sync_lock_release(&(m)->lock)
#else
#define RELEASE(m) __cilkrts_xchg(&(m)->lock, 0)
#endif

void __cilkrts_mutex_init(struct mutex *m)
{
    m->owner = 0;

    // Use a simple assignment so Inspector doesn't bug us about the
    // interlocked exchange doing a read of an uninitialized variable.
    // By definition there can't be a race when we're initializing the
    // lock...
    m->lock = 0;
}

void __cilkrts_mutex_lock(__cilkrts_worker *w, struct mutex *m)
{
    int count;
    const int maxspin = 1000; /* SWAG */

    NOTE_INTERVAL(w, INTERVAL_MUTEX_LOCK);
    if (!TRY_ACQUIRE(m)) {
        START_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
        count = 0;
        do {
            do {
                __cilkrts_short_pause();
                if (++count >= maxspin) {
                    STOP_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
                    START_INTERVAL(w, INTERVAL_MUTEX_LOCK_YIELDING);
                    /* let the OS reschedule every once in a while */
                    __cilkrts_yield();
                    STOP_INTERVAL(w, INTERVAL_MUTEX_LOCK_YIELDING);
                    START_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
                    count = 0;
                }
            } while (m->lock != 0);
        } while (!TRY_ACQUIRE(m));
        STOP_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
    }

    CILK_ASSERT(m->owner == 0);
    m->owner = w;
}

int __cilkrts_mutex_trylock(__cilkrts_worker *w, struct mutex *m)
{
    NOTE_INTERVAL(w, INTERVAL_MUTEX_TRYLOCK);
    if (TRY_ACQUIRE(m)) {
        CILK_ASSERT(m->owner == 0);
        m->owner = w;
        return 1;
    } else {
        return 0;
    }
}

void __cilkrts_mutex_unlock(__cilkrts_worker *w, struct mutex *m)
{
    CILK_ASSERT(m->owner == w);
    m->owner = 0;
    RELEASE(m);
}

void __cilkrts_mutex_destroy(__cilkrts_worker *w, struct mutex *m)
{
    (void)w; /* unused */
    (void)m; /* unused */
}

/* End worker_mutex.c */
