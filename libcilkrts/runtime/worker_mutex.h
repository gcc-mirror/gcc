/* worker_mutex.h                  -*-C++-*-
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

/**
 * @file worker_mutex.h
 *
 * @brief Support for Cilk runtime mutexes.
 *
 * Cilk runtime mutexes are implemented as simple spin loops.
 */

#ifndef INCLUDED_WORKER_MUTEX_DOT_H
#define INCLUDED_WORKER_MUTEX_DOT_H

#include <cilk/common.h>
#include "rts-common.h"

__CILKRTS_BEGIN_EXTERN_C

/**
 * Mutexes are treated as an abstract data type within the Cilk
 * runtime system.  They are implemented as simple spin loops and
 * owned by a __cilkrts_worker.
 */
typedef struct mutex {
    /** Mutex spin loop variable. 0 if unowned, 1 if owned. */
    volatile int lock;

    /** Worker that owns the mutex.  Must be 0 if mutex is unowned. */
    __cilkrts_worker *owner;
} mutex;

/**
 * @brief Initialize a Cilk mutex.
 *
 * @param m Mutex to be initialized.
 */
COMMON_PORTABLE
void __cilkrts_mutex_init(struct mutex *m);

/**
 * @brief Acquire a Cilk mutex.
 *
 * If statistics are being gathered, the time spent
 * acquiring the mutex will be attributed to the specified worker.
 *
 * @param w Worker that will become the owner of this mutex.
 * @param m Mutex to be initialized.
 */
COMMON_PORTABLE
void __cilkrts_mutex_lock(__cilkrts_worker *w,
                          struct mutex *m);
/**
 * @brief Attempt to lock a Cilk mutex and fail if it isn't available.
 *
 * If statistics are being gathered, the time spent acquiring the
 * mutex will be attributed to the specified worker.
 *
 * @param w Worker that will become the owner of this mutex.
 * @param m Mutex to be acquired.
 *
 * @return 1 if the mutex was acquired.
 * @return 0 if the mutex was not acquired.
 */
COMMON_PORTABLE
int __cilkrts_mutex_trylock(__cilkrts_worker *w,
                            struct mutex *m);

/**
 * @brief Release a Cilk mutex.
 * 
 * If statistics are being gathered, the time spent
 * acquiring the mutex will be attributed to the specified worker.
 *
 * @pre The mutex must be owned by the worker.
 *
 * @param w Worker that owns this mutex.
 * @param m Mutex to be released.
 */
COMMON_PORTABLE
void __cilkrts_mutex_unlock(__cilkrts_worker *w,
                            struct mutex *m);

/**
 * @brief Deallocate a Cilk mutex.  Currently does nothing.
 *
 * @param w Unused.
 * @param m Mutex to be deallocated.
 */
COMMON_PORTABLE
void __cilkrts_mutex_destroy(__cilkrts_worker *w,
                             struct mutex *m);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_WORKER_MUTEX_DOT_H)
