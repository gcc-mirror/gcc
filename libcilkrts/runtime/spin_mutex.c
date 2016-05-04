/* spin_mutex.c                  -*-C-*-
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

#include "spin_mutex.h"
#include "bug.h"
#include "os.h"
#include "stats.h"

// TBD (11/30/12): We should be doing a conditional test-xchg instead
// of an unconditional xchg operation for the spin mutex.

/* m->lock == 1 means that mutex M is locked */
#define TRY_ACQUIRE(m) (__cilkrts_xchg(&(m)->lock, 1) == 0)

/* ICC 11.1+ understands release semantics and generates an
   ordinary store with a software memory barrier. */
#if __ICC >= 1110
#define RELEASE(m) __sync_lock_release(&(m)->lock)
#else
#define RELEASE(m) __cilkrts_xchg(&(m)->lock, 0)
#endif


spin_mutex* spin_mutex_create() 
{
    spin_mutex* mutex = (spin_mutex*)__cilkrts_malloc(sizeof(spin_mutex));
    spin_mutex_init(mutex);
    return mutex;
}

void spin_mutex_init(struct spin_mutex *m)
{
    // Use a simple assignment so Inspector doesn't bug us about the
    // interlocked exchange doing a read of an uninitialized variable.
    // By definition there can't be a race when we're initializing the
    // lock...
    m->lock = 0;
}

void spin_mutex_lock(struct spin_mutex *m)
{
    int count;
    const int maxspin = 1000; /* SWAG */
    if (!TRY_ACQUIRE(m)) {
        count = 0;
        do {
            do {
                __cilkrts_short_pause();
                if (++count >= maxspin) {
                    /* let the OS reschedule every once in a while */
                    __cilkrts_yield();
                    count = 0;
                }
            } while (m->lock != 0);
        } while (!TRY_ACQUIRE(m));
    }
}

int spin_mutex_trylock(struct spin_mutex *m)
{
    return TRY_ACQUIRE(m);
}

void spin_mutex_unlock(struct spin_mutex *m)
{
    RELEASE(m);
}

void spin_mutex_destroy(struct spin_mutex *m)
{
    __cilkrts_free(m);
}

/* End spin_mutex.c */
