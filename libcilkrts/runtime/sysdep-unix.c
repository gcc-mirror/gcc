/*
 * sysdep-unix.c
 *
 *************************************************************************
 *
 *  Copyright (C) 2010-2016, Intel Corporation
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
 **************************************************************************
 */

#include "sysdep.h"
#include "os.h"
#include "bug.h"
#include "local_state.h"
#include "signal_node.h"
#include "full_frame.h"
#include "jmpbuf.h"
#include "cilk_malloc.h"
#include "reducer_impl.h"
#include "metacall_impl.h"


// On x86 processors (but not MIC processors), the compiler generated code to
// save the FP state (rounding mode and the like) before calling setjmp.  We
// will need to restore that state when we resume.
#ifndef __MIC__
# if defined(__i386__) || defined(__x86_64)
#   define RESTORE_X86_FP_STATE
# endif // defined(__i386__) || defined(__x86_64)
#endif  // __MIC__

// contains notification macros for VTune.
#include "cilk-ittnotify.h"

#include <stddef.h>

#ifdef __CYGWIN__
// On Cygwin, string.h doesnt declare strcasecmp if __STRICT_ANSI__ is defined
#   undef __STRICT_ANSI__
#endif

#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include "declare-alloca.h"

#ifdef __linux__
#   include <sys/resource.h>
#   include <sys/sysinfo.h>
#endif

#ifdef __FreeBSD__
#   include <sys/resource.h>
// BSD does not define MAP_ANONYMOUS, but *does* define MAP_ANON. Aren't standards great!
#   define MAP_ANONYMOUS MAP_ANON
#endif

#ifdef  __VXWORKS__
#   include <vxWorks.h>   
#   include <vxCpuLib.h>  
#endif

struct global_sysdep_state
{
    pthread_t *threads;    ///< Array of pthreads for system workers
    size_t pthread_t_size; ///< for cilk_db
}; 

static void internal_enforce_global_visibility();


COMMON_SYSDEP
void __cilkrts_init_worker_sysdep(struct __cilkrts_worker *w)
{
    ITT_SYNC_CREATE(w, "Scheduler");
}

COMMON_SYSDEP
void __cilkrts_destroy_worker_sysdep(struct __cilkrts_worker *w)
{
}

COMMON_SYSDEP
void __cilkrts_init_global_sysdep(global_state_t *g)
{
    internal_enforce_global_visibility();

    __cilkrts_init_tls_variables();

    CILK_ASSERT(g->total_workers >= g->P - 1);
    g->sysdep = __cilkrts_malloc(sizeof (struct global_sysdep_state));
    CILK_ASSERT(g->sysdep);
    g->sysdep->pthread_t_size = sizeof (pthread_t);
    
    // TBD: Should this value be g->total_workers, or g->P?
    //      Need to check what we are using this field for.
    g->sysdep->threads = __cilkrts_malloc(sizeof(pthread_t) * g->total_workers);
    CILK_ASSERT(g->sysdep->threads);

    return;
}

COMMON_SYSDEP
void __cilkrts_destroy_global_sysdep(global_state_t *g)
{
    if (g->sysdep->threads)
        __cilkrts_free(g->sysdep->threads);
    __cilkrts_free(g->sysdep);
}

/*************************************************************
  Creation of worker threads:
*************************************************************/

static void internal_run_scheduler_with_exceptions(__cilkrts_worker *w)
{
    /* We assume the stack grows down. */
    char var;
    __cilkrts_cilkscreen_establish_c_stack(&var - 1000000, &var);

    __cilkrts_run_scheduler_with_exceptions(w);
}



/*
 * scheduler_thread_proc_for_system_worker
 *
 * Thread start function called when we start a new worker.
 *
 */
NON_COMMON void* scheduler_thread_proc_for_system_worker(void *arg)
{
    /*int status;*/
    __cilkrts_worker *w = (__cilkrts_worker *)arg;

#ifdef __INTEL_COMPILER
#ifdef USE_ITTNOTIFY
    // Name the threads for Advisor.  They don't want a worker number.
    __itt_thread_set_name("Cilk Worker");
#endif // defined USE_ITTNOTIFY
#endif // defined __INTEL_COMPILER

    /* Worker startup is serialized
    status = pthread_mutex_lock(&__cilkrts_global_mutex);
    CILK_ASSERT(status == 0);*/
    CILK_ASSERT(w->l->type == WORKER_SYSTEM);
    /*status = pthread_mutex_unlock(&__cilkrts_global_mutex);
    CILK_ASSERT(status == 0);*/
    
    __cilkrts_set_tls_worker(w);

    START_INTERVAL(w, INTERVAL_IN_SCHEDULER);
    START_INTERVAL(w, INTERVAL_IN_RUNTIME);
    START_INTERVAL(w, INTERVAL_INIT_WORKER);

    // Create a cilk fiber for this worker on this thread.
    START_INTERVAL(w, INTERVAL_FIBER_ALLOCATE_FROM_THREAD) {
        w->l->scheduling_fiber = cilk_fiber_allocate_from_thread();
        cilk_fiber_set_owner(w->l->scheduling_fiber, w);
    } STOP_INTERVAL(w, INTERVAL_FIBER_ALLOCATE_FROM_THREAD);

    STOP_INTERVAL(w, INTERVAL_INIT_WORKER);
    
    internal_run_scheduler_with_exceptions(w);

    START_INTERVAL(w, INTERVAL_FIBER_DEALLOCATE_FROM_THREAD) {
        // Deallocate the scheduling fiber.  This operation reverses the
        // effect cilk_fiber_allocate_from_thread() and must be done in this
        // thread before it exits.
        int ref_count = cilk_fiber_deallocate_from_thread(w->l->scheduling_fiber);
        // Scheduling fibers should never have extra references to them.
        // We only get extra references into fibers because of Windows
        // exceptions.
        CILK_ASSERT(0 == ref_count);
        w->l->scheduling_fiber = NULL;
    } STOP_INTERVAL(w, INTERVAL_FIBER_DEALLOCATE_FROM_THREAD);
    
    STOP_INTERVAL(w, INTERVAL_IN_RUNTIME);
    STOP_INTERVAL(w, INTERVAL_IN_SCHEDULER);
    return 0;
}

/**
 * We are exporting a function with this name to Inspector?
 * What a confusing name...
 *
 * This function is exported so Piersol's stack trace displays
 * reasonable information.
 */ 
void* __cilkrts_worker_stub(void* arg)
{
    return scheduler_thread_proc_for_system_worker(arg);
}

// /* Return the lesser of the argument and the operating system
//    limit on the number of workers (threads) that may or ought
//    to be created. */
// int sysdep_thread_limit(int n, int physical_cpus)
// {
//     /* On Linux thread creation fails somewhere short of the
//        number of available processes. */
//     struct rlimit lim;

//     if (n > 256 + 2 * physical_cpus)
//         n = 256 + 2 * physical_cpus;

//     if (getrlimit(RLIMIT_NPROC, &lim) == 0 && lim.rlim_cur != RLIM_INFINITY)
//     {
//         /* If the limit reads 0 or absurdly small, ignore it. */
//         unsigned int maxproc = (lim.rlim_cur * 3 + 3) / 4;
//         if (maxproc > 8 + 2 * physical_cpus && maxproc < n)
//             n = maxproc;
//     }
//     return n;
// }



static void write_version_file (global_state_t *, int);

/* Create n worker threads from base..top-1
 */
static void create_threads(global_state_t *g, int base, int top)
{
    // TBD(11/30/12): We want to insert code providing the option of
    // pinning system workers to cores.
    for (int i = base; i < top; i++) {
        int status = pthread_create(&g->sysdep->threads[i],
                                    NULL,
                                    scheduler_thread_proc_for_system_worker,
                                    g->workers[i]);
        if (status != 0)
            __cilkrts_bug("Cilk runtime error: thread creation (%d) failed: %d\n", i, status);
    }
}

#if PARALLEL_THREAD_CREATE
static int volatile threads_created = 0;

// Create approximately half of the worker threads, and then become a worker
// ourselves.
static void * create_threads_and_work (void * arg)
{
    global_state_t *g = ((__cilkrts_worker *)arg)->g;

    create_threads(g, g->P/2, g->P-1);
    // Let the initial thread know that we're done.
    threads_created = 1;

    // Ideally this turns into a tail call that wipes out this stack frame.
    return scheduler_thread_proc_for_system_worker(arg);
}
#endif
void __cilkrts_start_workers(global_state_t *g, int n)
{
    g->workers_running = 1;
    g->work_done = 0;

    if (!g->sysdep->threads)
        return;

    // Do we actually have any threads to create?
    if (n > 0)
    {
#if PARALLEL_THREAD_CREATE
            int status;
            // We create (a rounded up) half of the threads, thread one creates the rest
            int half_threads = (n+1)/2;
        
            // Create the first thread passing a different thread function, so that it creates threads itself
            status = pthread_create(&g->sysdep->threads[0], NULL, create_threads_and_work, g->workers[0]);

            if (status != 0)
                __cilkrts_bug("Cilk runtime error: thread creation (0) failed: %d\n", status);
            
            // Then the rest of the ones we have to create
            create_threads(g, 1, half_threads);

            // Now wait for the first created thread to tell us it's created all of its threads.
            // We could maybe drop this a bit lower and overlap with write_version_file.
            while (!threads_created)
                __cilkrts_yield();
#else
            // Simply create all the threads linearly here.
            create_threads(g, 0, n);
#endif
    }
    // write the version information to a file if the environment is configured
    // for it (the function makes the check).
    write_version_file(g, n);


    return;
}

void __cilkrts_stop_workers(global_state_t *g)
{
    int i;

    // Tell the workers to give up

    g->work_done = 1;

    if (g->workers_running == 0)
        return;

    if (!g->sysdep->threads)
        return;

    /* Make them all runnable. */
    if (g->P > 1) {
        CILK_ASSERT(g->workers[0]->l->signal_node);
        signal_node_msg(g->workers[0]->l->signal_node, 1);
    }

        for (i = 0; i < g->P - 1; ++i) {
            int sc_status;
            void *th_status;

            sc_status = pthread_join(g->sysdep->threads[i], &th_status);
            if (sc_status != 0)
                __cilkrts_bug("Cilk runtime error: thread join (%d) failed: %d\n", i, sc_status);
        }

    g->workers_running = 0;


    return;
}


/*
 * @brief Returns the stack address for resuming execution of sf.
 *
 * This method takes in the top of the stack to use, and then returns
 * a properly aligned address for resuming execution of sf.
 *
 *  @param sf           -   The stack frame we want to resume executing.
 *  @param stack_base   -   The top of the stack we want to execute sf on.
 *
 */
static char* get_sp_for_executing_sf(char* stack_base,
                                     full_frame *ff,
                                     __cilkrts_stack_frame *sf)
{
// The original calculation that had been done to correct the stack
// pointer when resuming execution.
//
// But this code was never getting called in the eng branch anyway...
// 
// TBD(11/30/12): This logic needs to be revisited to make sure that
// we are doing the proper calculation in reserving space for outgoing
// arguments on all platforms and architectures.
#if 0    
    /* Preserve outgoing argument space and stack alignment on steal.
       Outgoing argument space is bounded by the difference between
       stack and frame pointers.  Some user code is known to rely on
       16 byte alignment.  Maintain 32 byte alignment for future
       compatibility. */
#define SMASK 31 /* 32 byte alignment */
    if (sf) {
        char *fp = FP(sf), *sp = SP(sf);
        int fp_align = (int)(size_t)fp & SMASK;
        ptrdiff_t space = fp - sp;

        fprintf(stderr, "Here: fp = %p, sp = %p\n", fp, sp);
        char *top_aligned = (char *)((((size_t)stack_base - SMASK) & ~(size_t)SMASK) | fp_align);
        /* Don't allocate an unreasonable amount of stack space. */

        fprintf(stderr, "Here: stack_base = %p, top_aligned=%p, space=%ld\n",
                stack_base, top_aligned, space);
        if (space < 32)
            space = 32 + (space & SMASK);
        else if (space > 40 * 1024)
            space = 40 * 1024 + (space & SMASK);

        return top_aligned - space;
    }
#endif    

#define PERFORM_FRAME_SIZE_CALCULATION 0
    
    char* new_stack_base = stack_base - 256;

#if PERFORM_FRAME_SIZE_CALCULATION
    // If there is a frame size saved, then use that as the
    // correction instead of 256.
    if (ff->frame_size > 0) {
        if (ff->frame_size < 40*1024) {
            new_stack_base = stack_base - ff->frame_size;
        }
        else {
            // If for some reason, our frame size calculation is giving us
            // a number which is bigger than about 10 pages, then
            // there is likely something wrong here?  Don't allocate
            // an unreasonable amount of space.
            new_stack_base = stack_base - 40*1024;
        }
    }
#endif
    
    // Whatever correction we choose, align the final stack top.
    // This alignment seems to be necessary in particular on 32-bit
    // Linux, and possibly Mac. (Is 32-byte alignment is sufficient?)
    /* 256-byte alignment. Why not? */
    const uintptr_t align_mask = ~(256 -1);
    new_stack_base = (char*)((size_t)new_stack_base & align_mask);
    return new_stack_base;
}

char* sysdep_reset_jump_buffers_for_resume(cilk_fiber* fiber,
                                           full_frame *ff,
                                           __cilkrts_stack_frame *sf)
{
#if FIBER_DEBUG >= 4
    fprintf(stderr, "ThreadId=%p (fiber_proc_to_resume), Fiber %p.  sf = %p. ff=%p, ff->sync_sp=%p\n",
            cilkos_get_current_thread_id(),
            fiber,
            sf,
            ff, ff->sync_sp);
#endif

    CILK_ASSERT(fiber);
    void* sp = (void*)get_sp_for_executing_sf(cilk_fiber_get_stack_base(fiber), ff, sf);
    SP(sf) = CILK_ADJUST_SP(sp);

    /* Debugging: make sure stack is accessible. */
    ((volatile char *)sp)[-1];

    // Adjust the saved_sp to account for the SP we're about to run.  This will
    // allow us to track fluctations in the stack
#if FIBER_DEBUG >= 4    
    fprintf(stderr, "ThreadId=%p, about to take stack ff=%p, sp=%p, sync_sp=%p\n",
            cilkos_get_current_thread_id(),
            ff,
            sp,
            ff->sync_sp);
#endif
    __cilkrts_take_stack(ff, sp);
    return sp;
}


NORETURN sysdep_longjmp_to_sf(char* new_sp,
                              __cilkrts_stack_frame *sf,
                              full_frame *ff_for_exceptions /* UNUSED on Unix */)
{
#if FIBER_DEBUG >= 3
    fprintf(stderr,
            "ThreadId=%p. resume user code, sf=%p, new_sp = %p, original SP(sf) = %p, FP(sf) = %p\n",
            cilkos_get_current_thread_id(), sf, new_sp, SP(sf), FP(sf));
#endif

    // Set the stack pointer.
    SP(sf) = CILK_ADJUST_SP(new_sp);

#ifdef RESTORE_X86_FP_STATE
    if (CILK_FRAME_VERSION_VALUE(sf->flags) >= 1) {
        // Restore the floating point state that was set in this frame at the
        // last spawn.
        //
        // This feature is only available in ABI 1 or later frames, and only
        // needed on IA64 or Intel64 processors.
        restore_x86_fp_state(sf);
    }
#endif

    CILK_LONGJMP(sf->ctx);
}


#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <errno.h>


void __cilkrts_make_unrunnable_sysdep(__cilkrts_worker *w,
                                      full_frame *ff,
                                      __cilkrts_stack_frame *sf,
                                      int is_loot,
                                      const char *why)
{
    (void)w; /* unused */
    sf->except_data = 0;

    if (is_loot)
    {
        if (ff->frame_size == 0)
        ff->frame_size = __cilkrts_get_frame_size(sf);

        // Null loot's sp for debugging purposes (so we'll know it's not valid)
    SP(sf) = 0;
    }
}

/*************************************************************
  Version information:
*************************************************************/

#ifndef _WRS_KERNEL
#include <dlfcn.h>
#endif
#include "internal/cilk_version.h"
#include <stdio.h>
#ifndef _WRS_KERNEL
#include <sys/utsname.h>
#endif

#ifdef __VXWORKS__
#include <version.h>
#endif

/* (Non-static) dummy function is used by get_runtime_path() to find the path
 * to the .so containing the Cilk runtime.
 */
void dummy_function() { }

/*
 * Return a string with the path to the Cilk runtime, or "unknown" if the path
 * cannot be determined.
 */
static const char *get_runtime_path ()
{
    // dladdr is a glibc extension. If it's available, use it to find the path
    // for libcilkrts.so
#if HAVE_DLADDR
    Dl_info info;
    if (0 != dladdr(dummy_function, &info))
        return info.dli_fname;
#endif

    // If dladdr isn't available, or dladdr failed, we can't know the path for
    // the shared object
    return "unknown";
}

#ifdef _WRS_KERNEL
#include <version.h>
#include <sysLib.h>
#endif
/* if the environment variable, CILK_VERSION, is defined, writes the version
 * information to the specified file.
 * g is the global state that was just created, and n is the number of workers
 * that were made (or requested from RML) for it.
 */
static void write_version_file (global_state_t *g, int n)
{
    const char *env;      // environment variable.
    char buf[256];        // print buffer.
    time_t t;
    FILE *fp;
#ifndef _WRS_KERNEL
    struct utsname sys_info;
#endif
    int err;              // error code from system calls.

    // if CILK_VERSION is not set, or if the file cannot be opened, fail
    // silently.  Otherwise open the file for writing (or use stderr or stdout
    // if the user specifies).
    if (NULL == (env = getenv("CILK_VERSION"))) return;
    if (0 == strcasecmp(env, "stderr"))         fp = stderr;
    else if (0 == strcasecmp(env, "stdout"))    fp = stdout;
    else if (NULL == (fp = fopen(env, "w")))    return;

    // get a string for the current time.  E.g.,
    // Cilk runtime initialized: Thu Jun 10 13:28:00 2010
    t = time(NULL);
    strftime(buf, 256, "%a %b %d %H:%M:%S %Y", localtime(&t));
    fprintf(fp, "Cilk runtime initialized: %s\n", buf);

    // Print runtime info.  E.g.,
    // Cilk runtime information
    // ========================
    // Cilk version: 2.0.0 Build 9184
    // Built by willtor on host willtor-desktop
    // Compilation date: Thu Jun 10 13:27:42 2010
    // Compiled with ICC V99.9.9, ICC build date: 20100610

    fprintf(fp, "\nCilk runtime information\n");
    fprintf(fp, "========================\n");
    fprintf(fp, "Cilk version: %d.%d.%d Build %d\n",
            VERSION_MAJOR,
            VERSION_MINOR,
            VERSION_REV,
            VERSION_BUILD);
#ifdef __VXWORKS__    
    char * vxWorksVer = VXWORKS_VERSION; 
    fprintf(fp, "Cross compiled for %s\n",vxWorksVer);
    // user and host not avalible if VxWorks cross compiled on windows build host 
#else

    // User and host are not available for GCC builds
#ifdef BUILD_USER
    fprintf(fp, "Built by "BUILD_USER" on host "BUILD_HOST"\n");
#endif // BUILD_USER
#endif // __VXWORKS__

    // GCC has requested that this be removed for GCC builds
#ifdef BUILD_USER    
    fprintf(fp, "Compilation date: "__DATE__" "__TIME__"\n");
#endif // BUILD_USER

#ifdef __INTEL_COMPILER
    // Compiled by the Intel C/C++ compiler.
    fprintf(fp, "Compiled with ICC V%d.%d.%d, ICC build date: %d\n",
            __INTEL_COMPILER / 100,
            (__INTEL_COMPILER / 10) % 10,
            __INTEL_COMPILER % 10,
            __INTEL_COMPILER_BUILD_DATE);
#else
    // Compiled by GCC.
    fprintf(fp, "Compiled with GCC V%d.%d.%d\n",
            __GNUC__,
            __GNUC_MINOR__,
            __GNUC_PATCHLEVEL__);
#endif // defined __INTEL_COMPILER

    // Print system info.  E.g.,
    // System information
    // ==================
    // Cilk runtime path: /opt/icc/64/lib/libcilkrts.so.5
    // System OS: Linux, release 2.6.28-19-generic
    // System architecture: x86_64

    fprintf(fp, "\nSystem information\n");
    fprintf(fp, "==================\n");
    fprintf(fp, "Cilk runtime path: %s\n", get_runtime_path());
#ifndef _WRS_KERNEL
    err = uname(&sys_info);
    fprintf(fp, "System OS: %s, release %s\n",
            err < 0 ? "unknown" : sys_info.sysname,
            err < 0 ? "?" : sys_info.release);
    fprintf(fp, "System architecture: %s\n",
            err < 0 ? "unknown" : sys_info.machine);
#else
    fprintf(fp, "System OS: %s, release %s\n",
            "VxWorks", RUNTIME_NAME RUNTIME_VERSION);
    fprintf(fp, "System architecture: %s\n",
            sysModel());
#endif

    // Print thread info.  E.g.,
    // Thread information
    // ==================
    // System cores: 8
    // Cilk workers requested: 8

    fprintf(fp, "\nThread information\n");
    fprintf(fp, "==================\n");
#ifdef __VXWORKS__      
    fprintf(fp, "System cores: %d\n", (int)__builtin_popcount(vxCpuEnabledGet()));
#else    
    fprintf(fp, "System cores: %d\n", (int)sysconf(_SC_NPROCESSORS_ONLN));
#endif    
    fprintf(fp, "Cilk workers requested: %d\n", n);

    if (fp != stderr && fp != stdout) fclose(fp);
    else fflush(fp); // flush the handle buffer if it is stdout or stderr.
}


/*
 * __cilkrts_establish_c_stack
 *
 * Tell Cilkscreen about the user stack bounds.
 *
 * Note that the Cilk V1 runtime only included the portion of the stack from
 * the entry into Cilk, down.  We don't appear to be able to find that, but
 * I think this will be sufficient.
 */

void __cilkrts_establish_c_stack(void)
{
    /* FIXME: Not implemented. */

    /* TBD: Do we need this */
    /*
    void __cilkrts_cilkscreen_establish_c_stack(char *begin, char *end);

    size_t r;
    MEMORY_BASIC_INFORMATION mbi;

    r = VirtualQuery (&mbi,
                      &mbi,
                      sizeof(mbi));

    __cilkrts_cilkscreen_establish_c_stack((char *)mbi.BaseAddress,
                                   (char *)mbi.BaseAddress + mbi.RegionSize);
    */
}


/*
 * internal_enforce_global_visibility
 *
 * Ensure global visibility of public symbols, for proper Cilk-TBB interop.
 *
 * If Cilk runtime is loaded dynamically, its symbols might remain unavailable
 * for global search with dladdr; that might prevent TBB from finding Cilk
 * in the process address space and initiating the interop protocol.
 * The workaround is for the library to open itself with RTLD_GLOBAL flag.
 */

static __attribute__((noinline))
void internal_enforce_global_visibility()
{
#ifndef __VXWORKS__
    void* handle = dlopen( get_runtime_path(), RTLD_GLOBAL|RTLD_LAZY );

    /* For proper reference counting, close the handle immediately. */
    if( handle) dlclose(handle);
#endif
}

/*
  Local Variables: **
  c-file-style:"bsd" **
  c-basic-offset:4 **
  indent-tabs-mode:nil **
  End: **
*/
