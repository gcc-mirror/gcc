/* os_mutex.h                  -*-C++-*-
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
 * @file os_mutex.h
 *
 * @brief Portable interface to operating-system mutexes.
 *
 * Do not confuse os_mutex with Cilk runtime-specific spinlock mutexes.
 */

#ifndef INCLUDED_OS_MUTEX_DOT_H
#define INCLUDED_OS_MUTEX_DOT_H

#include <cilk/common.h>
#include "rts-common.h"

__CILKRTS_BEGIN_EXTERN_C

/// Opaque type
typedef struct os_mutex os_mutex;

/**
 * Allocate and initialize an os_mutex
 *
 * @return A pointer to the initialized os_mutex
 */
COMMON_SYSDEP os_mutex* __cilkrts_os_mutex_create(void);

/**
 * Acquire the os_mutex for exclusive use
 *
 * @param m The os_mutex that is to be acquired.
 */
COMMON_SYSDEP void __cilkrts_os_mutex_lock(os_mutex *m);

/**
 * Try to acquire the os_mutex.
 *
 * @param   m  The os_mutex to try to acquire
 * @return  0      if the lock acquire failed
 * @return nonzero if the lock was acquired
 */
COMMON_SYSDEP int __cilkrts_os_mutex_trylock(os_mutex *m);

/**
 * Release the os_mutex
 *
 * @param m The os_mutex that is to be released.
 */
COMMON_SYSDEP void __cilkrts_os_mutex_unlock(os_mutex *m);

/**
 * Release any resources and deallocate the os_mutex.
 *
 * @param m The os_mutex that is to be deallocated.
 */
COMMON_SYSDEP void __cilkrts_os_mutex_destroy(os_mutex *m);

/**
 * Acquire the global os_mutex for exclusive use.  The global os_mutex
 * will be initialized the first time this function is called in a
 * thread-safe manner.
 */
COMMON_SYSDEP void global_os_mutex_lock();

/**
 * Release the global os_mutex.  global_os_mutex_lock() must have been
 * called first.
 */
COMMON_SYSDEP void global_os_mutex_unlock();


#ifdef _MSC_VER

/**
 * @brief Create the global OS mutex - Windows only.
 *
 * On Windows we use DllMain() to create the global OS mutex when cilkrts20.dll
 * is loaded. As opposed to Linux/MacOS where we use pthread_once to implement
 * a singleton since there are no guarantees about constructor or destructor
 * ordering between shared objects.
 */
NON_COMMON void global_os_mutex_create();

/**
 * @brief Destroy the global OS mutex - Windows only
 *
 * On Windows we use DllMain() to destroy the global OS mutex when
 * cilkrts20.dll is unloaded.  As opposed to Linux/MacOS where we cannot
 * know when it's safe to destroy the global OS mutex since there are no
 * guarantees about constructor or destructor ordering.
 */
NON_COMMON void global_os_mutex_destroy();

#endif  // _MSC_VER

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_OS_MUTEX_DOT_H)
