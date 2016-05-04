/* os_mutex-unix.c                  -*-C-*-
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

#include "os_mutex.h"
#include "bug.h"

#include <stdlib.h>
#include <errno.h>
#include <pthread.h>

// contains notification macros for VTune.
#include "cilk-ittnotify.h"

/*
 * OS Mutex functions.
 *
 * Not to be confused with the spinlock mutexes implemented in cilk_mutex.c
 */

struct os_mutex {
    pthread_mutex_t mutex;  ///< On Linux, os_mutex is implemented with a pthreads mutex
};

// Unix implementation of the global OS mutex.  This will be created by the
// first call to global_os_mutex_lock() and *NEVER* destroyed.  On gcc-based
// systems there's no way to guarantee the ordering of constructors and
// destructors, so we can't be guaranteed that our destructor for a static
// object will be called *after* any static destructors that may use Cilk
// in the user's application
static struct os_mutex *global_os_mutex = NULL;

/* Sometimes during shared library load malloc doesn't work.
   To handle that case, preallocate space for one mutex. */
static struct os_mutex static_mutex;
static int static_mutex_used;

struct os_mutex *__cilkrts_os_mutex_create(void)
{
    int status;
    struct os_mutex *mutex = (struct os_mutex *)malloc(sizeof(struct os_mutex));
    pthread_mutexattr_t attr;

    ITT_SYNC_CREATE(mutex, "OS Mutex");

    if (!mutex) {
        if (static_mutex_used) {
            __cilkrts_bug("Cilk RTS library initialization failed");
        } else {
            static_mutex_used = 1;
            mutex = &static_mutex;
        }
    }

    status = pthread_mutexattr_init(&attr);
    CILK_ASSERT (status == 0);
#if defined DEBUG || CILK_LIB_DEBUG 
#ifdef PTHREAD_MUTEX_ERRORCHECK
    status = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
#else
    status = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK_NP);
#endif
    CILK_ASSERT (status == 0);
#endif
    status = pthread_mutex_init (&mutex->mutex, &attr);
    CILK_ASSERT (status == 0);
    pthread_mutexattr_destroy(&attr);

    return mutex;
}

void __cilkrts_os_mutex_lock(struct os_mutex *p)
{
    int status;
    status = pthread_mutex_lock (&p->mutex);
    ITT_SYNC_ACQUIRED(p);
    if (__builtin_expect(status, 0) == 0)
        return;
    if (status == EDEADLK)
        __cilkrts_bug("Cilk runtime error: deadlock acquiring mutex %p\n",
                      p);
    else
        __cilkrts_bug("Cilk runtime error %d acquiring mutex %p\n",
                      status, p);
}

int __cilkrts_os_mutex_trylock(struct os_mutex *p)
{
    int status;
    status = pthread_mutex_trylock (&p->mutex);
    return (status == 0);
}

void __cilkrts_os_mutex_unlock(struct os_mutex *p)
{
    int status;
    ITT_SYNC_RELEASING(p);
    status = pthread_mutex_unlock (&p->mutex);
    CILK_ASSERT(status == 0);
}

void __cilkrts_os_mutex_destroy(struct os_mutex *p)
{
    pthread_mutex_destroy (&p->mutex);
    if (p == &static_mutex) {
        static_mutex_used = 0;
    } else {
        free(p);
    }
}

/*
 * create_global_os_mutex
 *
 * Function used with pthread_once to initialize the global OS mutex.  Since
 * pthread_once requires a function which takes no parameters and has no
 * return value, the global OS mutex will be stored in the static (global
 * to the compilation unit) variable "global_os_mutex."
 * 
 * 
 * global_os_mutex will never be destroyed.
 */
static void create_global_os_mutex(void)
{
    CILK_ASSERT(NULL == global_os_mutex);
    global_os_mutex = __cilkrts_os_mutex_create();
}

void global_os_mutex_lock(void)
{
    // pthread_once_t used with pthread_once to guarantee that
    // create_global_os_mutex() is only called once
    static pthread_once_t global_os_mutex_is_initialized = PTHREAD_ONCE_INIT;

    // Execute create_global_os_mutex once in a thread-safe manner
    // Note that create_global_os_mutex returns the mutex in the static
    // (global to the module) variable "global_os_mutex"
    pthread_once(&global_os_mutex_is_initialized,
		 create_global_os_mutex);

    // We'd better have allocated a global_os_mutex
    CILK_ASSERT(NULL != global_os_mutex);
    
    // Acquire the global OS mutex
    __cilkrts_os_mutex_lock(global_os_mutex);
}

void global_os_mutex_unlock(void)
{
    // We'd better have allocated a global_os_mutex.  This means you should
    // have called global_os_mutex_lock() before calling
    // global_os_mutex_unlock(), but this is the only check for it.
    CILK_ASSERT(NULL != global_os_mutex);

    // Release the global OS mutex
    __cilkrts_os_mutex_unlock(global_os_mutex);
}

/* End os_mutex-unix.c */
