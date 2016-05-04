/* spin_mutex.h                  -*-C++-*-
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

/**
 * @file spin_mutex.h
 *
 * @brief Support for Cilk runtime mutexes.
 *
 * Cilk runtime mutexes are implemented as simple spin loops.
 *
 * This file is similar to a worker_mutex, except it does not have an
 * owner field.
 *
 * TBD: This class, worker_mutex, and os_mutex overlap quite a bit in
 * functionality.  Can we unify these mutexes somehow?
 */
#ifndef INCLUDED_SPIN_MUTEX_DOT_H
#define INCLUDED_SPIN_MUTEX_DOT_H

#include <cilk/common.h>
#include "rts-common.h"
#include "cilk_malloc.h"

__CILKRTS_BEGIN_EXTERN_C

/**
 * Mutexes are treated as an abstract data type within the Cilk
 * runtime system.  They are implemented as simple spin loops.
 */
typedef struct spin_mutex {
    /** Mutex spin loop variable. 0 if unowned, 1 if owned. */
    volatile int lock;

    /** Padding so the mutex takes up a cache line. */
    char pad[64/sizeof(int) - 1];
} spin_mutex;


/**
 * @brief Create a new Cilk spin_mutex.
 *
 * @return Returns an initialized spin mutex.  
 */
COMMON_PORTABLE
spin_mutex* spin_mutex_create();

/**
 * @brief Initialize a Cilk spin_mutex.
 *
 * @param m Spin_Mutex to be initialized.
 */
COMMON_PORTABLE
void spin_mutex_init(spin_mutex *m);

/**
 * @brief Acquire a Cilk spin_mutex.
 *
 * If statistics are being gathered, the time spent
 * acquiring the spin_mutex will be attributed to the specified worker.
 *
 * @param m Spin_Mutex to be initialized.
 */
COMMON_PORTABLE
void spin_mutex_lock(struct spin_mutex *m);
/**
 * @brief Attempt to lock a Cilk spin_mutex and fail if it isn't available.
 *
 * @param m Spin_Mutex to be acquired.
 *
 * @return 1 if the spin_mutex was acquired.
 * @return 0 if the spin_mutex was not acquired.
 */
COMMON_PORTABLE
int spin_mutex_trylock(struct spin_mutex *m);

/**
 * @brief Release a Cilk spin_mutex.
 *
 * @param m Spin_Mutex to be released.
 */
COMMON_PORTABLE
void spin_mutex_unlock(struct spin_mutex *m);

/**
 * @brief Deallocate a Cilk spin_mutex.  Currently does nothing.
 *
 * @param m Spin_Mutex to be deallocated.
 */
COMMON_PORTABLE
void spin_mutex_destroy(struct spin_mutex *m);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_SPIN_MUTEX_DOT_H)
