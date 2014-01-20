/* os-unix.c                  -*-C-*-
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

#ifdef __linux__
    // define _GNU_SOURCE before *any* #include.
    // Even <stdint.h> will break later #includes if this macro is not
    // already defined when it is #included.
#   define _GNU_SOURCE
#endif

#include "os.h"
#include "bug.h"
#include "cilk_malloc.h"
#include <internal/abi.h>

#if defined __linux__
#   include <sys/sysinfo.h>
#   include <sys/syscall.h>
#elif defined __APPLE__
#   include <sys/sysctl.h>
    // Uses sysconf(_SC_NPROCESSORS_ONLN) in verbose output
#elif defined  __FreeBSD__
// No additional include files
#elif defined __CYGWIN__
// Cygwin on Windows - no additional include files
#elif defined  __VXWORKS__
#   include <vxWorks.h>   
#   include <vxCpuLib.h>   
#   include <taskLib.h>   
// Solaris
#elif defined __sun__ && defined __svr4__
#   include <sched.h>
#else
#   error "Unsupported OS"
#endif

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>



// /* Thread-local storage */
// #ifdef _WIN32
// typedef unsigned cilkos_tls_key_t;
// #else
// typedef pthread_key_t cilkos_tls_key_t;
// #endif
// cilkos_tls_key_t cilkos_allocate_tls_key();
// void cilkos_set_tls_pointer(cilkos_tls_key_t key, void* ptr);
// void* cilkos_get_tls_pointer(cilkos_tls_key_t key);

#if !defined CILK_WORKER_TLS
static int cilk_keys_defined;
static pthread_key_t worker_key, pedigree_leaf_key, tbb_interop_key;

#if SUPPORT_GET_CURRENT_FIBER > 0
static pthread_key_t fiber_key;
#endif

static void *serial_worker;


// This destructor is called when a pthread dies to deallocate the
// pedigree node.
static void __cilkrts_pedigree_leaf_destructor(void* pedigree_tls_ptr)
{
    __cilkrts_pedigree* pedigree_tls
	= (__cilkrts_pedigree*)pedigree_tls_ptr;
    if (pedigree_tls) {
        // Assert that we have either one or two nodes
        // left in the pedigree chain.
        // If we have more, then something is going wrong...
        CILK_ASSERT(!pedigree_tls->parent || !pedigree_tls->parent->parent);
	__cilkrts_free(pedigree_tls);
    }
}

void __cilkrts_init_tls_variables(void)
{
    int status;
    /* This will be called once in serial execution before any
       Cilk parallelism so we do not need to worry about races
       on cilk_keys_defined. */
    if (cilk_keys_defined)
        return;
    status = pthread_key_create(&worker_key, NULL);
    CILK_ASSERT (status == 0);
    status = pthread_key_create(&pedigree_leaf_key,
				__cilkrts_pedigree_leaf_destructor);
    CILK_ASSERT (status == 0);
    status = pthread_key_create(&tbb_interop_key, NULL);
    CILK_ASSERT (status == 0);

#if SUPPORT_GET_CURRENT_FIBER > 0    
    status = pthread_key_create(&fiber_key, NULL);
    CILK_ASSERT (status == 0);
#endif
    cilk_keys_defined = 1;
    return;
}

COMMON_SYSDEP
void* cilkos_get_current_thread_id(void)
{
    return (void*)pthread_self();
}


CILK_ABI_WORKER_PTR __cilkrts_get_tls_worker()
{
    if (__builtin_expect(cilk_keys_defined, 1))
        return (__cilkrts_worker *)pthread_getspecific(worker_key);
    else 
        return serial_worker;
    
}

CILK_ABI_WORKER_PTR __cilkrts_get_tls_worker_fast()
{
  return (__cilkrts_worker *)pthread_getspecific(worker_key);
}

COMMON_SYSDEP
__cilk_tbb_stack_op_thunk *__cilkrts_get_tls_tbb_interop(void)
{
    if (__builtin_expect(cilk_keys_defined, 1))
        return (__cilk_tbb_stack_op_thunk *)
            pthread_getspecific(tbb_interop_key);
    else
        return 0;
}

// This counter should be updated atomically.
static int __cilkrts_global_pedigree_tls_counter = -1;

COMMON_SYSDEP
__cilkrts_pedigree *__cilkrts_get_tls_pedigree_leaf(int create_new)
{
    __cilkrts_pedigree *pedigree_tls;    
    if (__builtin_expect(cilk_keys_defined, 1)) {
        pedigree_tls =
            (struct __cilkrts_pedigree *)pthread_getspecific(pedigree_leaf_key);
    }
    else {
        return 0;
    }
    
    if (!pedigree_tls && create_new) {
        // This call creates two nodes, X and Y.
        // X == pedigree_tls[0] is the leaf node, which gets copied
        // in and out of a user worker w when w binds and unbinds.
        // Y == pedigree_tls[1] is the root node,
        // which is a constant node that represents the user worker
        // thread w.
	pedigree_tls = (__cilkrts_pedigree*)
	    __cilkrts_malloc(2 * sizeof(__cilkrts_pedigree));

        // This call sets the TLS pointer to the new node.
	__cilkrts_set_tls_pedigree_leaf(pedigree_tls);
        
        pedigree_tls[0].rank = 0;
        pedigree_tls[0].parent = &pedigree_tls[1];

        // Create Y, whose rank begins as the global counter value.
        pedigree_tls[1].rank =
            __sync_add_and_fetch(&__cilkrts_global_pedigree_tls_counter, 1);

        pedigree_tls[1].parent = NULL;
        CILK_ASSERT(pedigree_tls[1].rank != -1);
    }
    return pedigree_tls;
}

#if SUPPORT_GET_CURRENT_FIBER > 0
COMMON_SYSDEP
cilk_fiber_sysdep* cilkos_get_tls_cilk_fiber(void)
{
    if (__builtin_expect(cilk_keys_defined, 1))
        return (cilk_fiber_sysdep *)pthread_getspecific(fiber_key);
    else
        return NULL;
}
#endif

COMMON_SYSDEP
void __cilkrts_set_tls_worker(__cilkrts_worker *w)
{
    if (__builtin_expect(cilk_keys_defined, 1)) {
        int status;
        status = pthread_setspecific(worker_key, w);
        CILK_ASSERT (status == 0);
        return;
    }
    else
    {
        serial_worker = w;
    }
}

COMMON_SYSDEP
void __cilkrts_set_tls_tbb_interop(__cilk_tbb_stack_op_thunk *t)
{
    if (__builtin_expect(cilk_keys_defined, 1)) {
        int status;
        status = pthread_setspecific(tbb_interop_key, t);
        CILK_ASSERT (status == 0);
        return;
    }
    abort();
}

COMMON_SYSDEP
void __cilkrts_set_tls_pedigree_leaf(__cilkrts_pedigree* pedigree_leaf)
{
    if (__builtin_expect(cilk_keys_defined, 1)) {
        int status;
        status = pthread_setspecific(pedigree_leaf_key, pedigree_leaf);
        CILK_ASSERT (status == 0);
        return;
    }
    abort();
}

#if SUPPORT_GET_CURRENT_FIBER > 0
COMMON_SYSDEP
void cilkos_set_tls_cilk_fiber(cilk_fiber_sysdep* fiber)
{
    if (__builtin_expect(cilk_keys_defined, 1)) {
        int status;
        status = pthread_setspecific(fiber_key, fiber);
        CILK_ASSERT (status == 0);
        return;
    }
    abort();
}
#endif

#else
void __cilkrts_init_tls_variables(void)
{
}
#endif

#if defined (__linux__) && ! defined(ANDROID)
/*
 * Get the thread id, rather than the pid. In the case of MIC offload, it's
 * possible that we have multiple threads entering Cilk, and each has a
 * different affinity.
 */
static pid_t linux_gettid(void)
{
    return syscall(SYS_gettid);
}

/*
 * On Linux we look at the thread affinity mask and restrict ourself to one
 * thread for each of the hardware contexts to which we are bound.
 * Therefore if user does
 * % taskset 0-1 cilkProgram
 *       # restrict execution to hardware contexts zero and one
 * the Cilk program will only use two threads even if it is running on a
 * machine that has 32 hardware contexts.
 * This is the right thing to do, because the threads are restricted to two
 * hardware contexts by the affinity mask set by taskset, and if we were to
 * create extra threads they would simply oversubscribe the hardware resources
 * we can use.
 * This is particularly important on MIC in offload mode, where the affinity
 * mask is set by the offload library to force the offload code away from
 * cores that have offload support threads running on them.
 */
static int linux_get_affinity_count (int tid) 
{
#if !defined HAVE_PTHREAD_AFFINITY_NP
  return 0;
#else

    cpu_set_t process_mask;

    // Extract the thread affinity mask
    int err = sched_getaffinity (tid, sizeof(process_mask),&process_mask);

    if (0 != err)
    {
        return 0;
    }

    // We have extracted the mask OK, so now we can count the number of threads
    // in it.  This is linear in the maximum number of CPUs available, We
    // could do a logarithmic version, if we assume the format of the mask,
    // but it's not really worth it. We only call this at thread startup
    // anyway.
    int available_procs = 0;
    int i;
    for (i = 0; i < CPU_SETSIZE; i++)
    {
        if (CPU_ISSET(i, &process_mask))
        {
            available_procs++;
        }
    }

    return available_procs;
#endif
}
#endif

/*
 * __cilkrts_hardware_cpu_count
 *
 * Returns the number of available CPUs on this hardware.  This is architecture-
 * specific. 
 */

COMMON_SYSDEP int __cilkrts_hardware_cpu_count(void)
{
#if defined ANDROID || (defined(__sun__) && defined(__svr4__))
    return sysconf (_SC_NPROCESSORS_ONLN);
#elif defined __MIC__
    /// HACK: Usually, the 3rd and 4th hyperthreads are not beneficial
    /// on KNC.  Also, ignore the last core.
    int P = sysconf (_SC_NPROCESSORS_ONLN);
    return P/2 - 2;
#elif defined __linux__
    int affinity_count = linux_get_affinity_count(linux_gettid());

    return (0 != affinity_count) ? affinity_count : sysconf (_SC_NPROCESSORS_ONLN);
#elif defined __APPLE__
    int count = 0;
    int cmd[2] = { CTL_HW, HW_NCPU };
    size_t len = sizeof count;
    int status = sysctl(cmd, 2, &count, &len, 0, 0);
    assert(status >= 0);
    assert((unsigned)count == count);

    return count;
#elif defined  __FreeBSD__ || defined __CYGWIN__
    int ncores = sysconf(_SC_NPROCESSORS_ONLN);

    return ncores;
    // Just get the number of processors
//    return sysconf(_SC_NPROCESSORS_ONLN);
#elif defined  __VXWORKS__
    return __builtin_popcount( vxCpuEnabledGet() );
#else
#error "Unknown architecture"
#endif
}

COMMON_SYSDEP void __cilkrts_sleep(void)
{
#ifdef __VXWORKS__
	taskDelay(1);
#else			
    usleep(1);
#endif	
}

COMMON_SYSDEP void __cilkrts_yield(void)
{
#if __APPLE__ || __FreeBSD__ || __VXWORKS__
    // On MacOS, call sched_yield to yield quantum.  I'm not sure why we
    // don't do this on Linux also.
    sched_yield();
#elif defined(__MIC__)
    // On MIC, pthread_yield() really trashes things.  Arch's measurements
    // showed that calling _mm_delay_32() (or doing nothing) was a better
    // option.  Delaying 1024 clock cycles is a reasonable compromise between
    // giving up the processor and latency starting up when work becomes
    // available
    _mm_delay_32(1024);
#elif defined(ANDROID) || (defined(__sun__) && defined(__svr4__))
    // On Android and Solaris, call sched_yield to yield quantum.  I'm not
    // sure why we don't do this on Linux also.
    sched_yield();
#else
    // On Linux, call pthread_yield (which in turn will call sched_yield)
    // to yield quantum.
    pthread_yield();
#endif
}

COMMON_SYSDEP __STDNS size_t cilkos_getenv(char* value, __STDNS size_t vallen,
                                           const char* varname)
{
    CILK_ASSERT(value);
    CILK_ASSERT(varname);

    const char* envstr = getenv(varname);
    if (envstr)
    {
        size_t len = strlen(envstr);
        if (len > vallen - 1)
            return len + 1;

        strcpy(value, envstr);
        return len;
    }
    else
    {
        value[0] = '\0';
        return 0;
    }
}

/*
 * Unrecoverable error: Print an error message and abort execution.
 */
COMMON_SYSDEP void cilkos_error(const char *fmt, ...)
{
    va_list l;
    fflush(NULL);
    fprintf(stderr, "Cilk error: ");
    va_start(l, fmt);
    vfprintf(stderr, fmt, l);
    va_end(l);
    fprintf(stderr, "Exiting.\n");
    fflush(stderr);

    abort();
}

/*
 * Print a warning message and return.
 */
COMMON_SYSDEP void cilkos_warning(const char *fmt, ...)
{
    va_list l;
    fflush(NULL);
    fprintf(stderr, "Cilk warning: ");
    va_start(l, fmt);
    vfprintf(stderr, fmt, l);
    va_end(l);
    fflush(stderr);
}

static void __attribute__((constructor)) init_once()
{
    /*__cilkrts_debugger_notification_internal(CILK_DB_RUNTIME_LOADED);*/
    __cilkrts_init_tls_variables();
}


#define PAGE 4096
#define CILK_MIN_STACK_SIZE (4*PAGE)
// Default size for the stacks that we create in Cilk for Unix.
#define CILK_DEFAULT_STACK_SIZE 0x100000

/*
 * Convert the user's specified stack size into a "reasonable" value
 * for this OS.
 */
size_t cilkos_validate_stack_size(size_t specified_stack_size) {
    // Convert any negative value to the default.
    if (specified_stack_size == 0) {
        CILK_ASSERT((CILK_DEFAULT_STACK_SIZE % PAGE) == 0);
        return CILK_DEFAULT_STACK_SIZE;
    }
    // Round values in between 0 and CILK_MIN_STACK_SIZE up to
    // CILK_MIN_STACK_SIZE.
    if (specified_stack_size <= CILK_MIN_STACK_SIZE) {
        return CILK_MIN_STACK_SIZE;
    }
    if ((specified_stack_size % PAGE) > 0) {
        // Round the user's stack size value up to nearest page boundary.
        return (PAGE * (1 + specified_stack_size / PAGE));
    }
    return specified_stack_size;
}

long cilkos_atomic_add(volatile long* p, long x)
{
    return __sync_add_and_fetch(p, x);
}

/* End os-unix.c */
