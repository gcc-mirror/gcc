/* global_state.cpp                  -*-C++-*-
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

#include "global_state.h"
#include "os.h"
#include "bug.h"
#include "metacall_impl.h"
#include "stats.h"
#include "cilk/cilk_api.h"
#include "cilk_malloc.h"
#include "record-replay.h"

#include <algorithm>  // For max()
#include <cstring>
#include <cstdlib>
#include <climits>
#include <cerrno>

#ifdef _WIN32
#   include <wchar.h>
#endif

// TBD: There is a race when multiple threads try to initialize the
// user_settable_values??
//
// Set to true if the user settable values portion of the global state
// singleton is initialized, even if the rest of the singleton is not
// initialized.
int cilkg_user_settable_values_initialized = false;

namespace {

// Single copy of the global state.  Zero-filled until
// cilkg_get_user_settable_values() is called and partially-zero-filled until
// cilkg_init_global_state() is called.  The first field is filled in with
// the size of a void* for the debugger and must be valid before initialization
static global_state_t global_state_singleton =
{
    sizeof(void *),         // addr_size
    GLOBAL_STATE_VERSION,   // structure version
};


// Variables that need to export C-style names
extern "C"
{
    // Pointer to the global state singleton.  
    global_state_t *cilkg_singleton_ptr = NULL;

    // __cilkrts_global_state is exported and referenced by the debugger.
    // The debugger expects it to be valid when the module loads.
//    CILK_EXPORT_DATA
    global_state_t *__cilkrts_global_state = &global_state_singleton;
}

// Returns true if 'a' and 'b' are equal null-terminated strings
inline bool strmatch(const char* a, const char* b)
{
    return 0 == std::strcmp(a, b);
}

// Returns the integer value represented by the null-terminated, decimal string
// at 's'.

inline long to_long(const char* s)
{
    char *end;

    errno = 0;
    return std::strtol(s, &end, 10);
}

#ifdef _WIN32
// Returns true if 'a' and 'b' are equal null-terminated wide-char strings
inline bool strmatch(const wchar_t* a, const wchar_t* b)
{
    return 0 == wcscmp(a, b);
}

// Returns true if the multi-byte character string at 'a' represents the same
// character sequence as the wide-character string at 'b'.  The behavior is
// undefined if 'a' contains more than 30 multi-byte characters.
bool strmatch(const char* a, const wchar_t* b)
{
    // Convert 'a' to wide-characters, then compare.
    wchar_t wa[31];
    std::size_t count;
    errno_t err = mbstowcs_s(&count, wa, a, 30);
    CILK_ASSERT(0 == err);
    if (err) return false;
    return strmatch(wa, b);
}

// Returns true if the wide-character string at 'a' represents the same
// character sequence as the multi-byte character string at 'b'.  The behavior
// id undefined if 'b' contains more than 30 multi-byte characters.
inline
bool strmatch(const wchar_t* a, const char* b)
{
    return strmatch(b, a);
}


// Returns the integer value represented by the null-terminated wide-char
// string at 's'.
inline long to_long(const wchar_t* s)
{
    wchar_t *end;

    errno = 0;
    return wcstol(s, &end, 0);
}
#endif

// Check if Cilkscreen or other sequential ptool wants to force reducers.
bool always_force_reduce()
{
    // Metacall *looks* like a no-op.  volatile needed to keep compiler from
    // optimizing away variable.
    volatile char not_force_reduce = '\377';
    __cilkrts_metacall(METACALL_TOOL_SYSTEM, HYPER_ZERO_IF_FORCE_REDUCE,
                       const_cast<char*>(&not_force_reduce));
    return ! not_force_reduce;
}

// Stores the boolean value represented by the null-terminated string at 'val'
// into the integer object at 'out'.  Returns '__CILKRTS_SET_PARAM_SUCCESS' if
// 'val' is "true", "false", "0" or "1" and '__CILKRTS_SET_PARAM_INVALID'
// otherwise.
template <typename INT_T, typename CHAR_T>
int store_bool(INT_T *out, const CHAR_T *val)
{
    static const char* const s_zero  = "0";
    static const char* const s_one   = "1";
    static const char* const s_true  = "true";
    static const char* const s_false = "false";
    
    if (val == 0)
        return __CILKRTS_SET_PARAM_INVALID;

    if (strmatch(s_false, val) || strmatch(s_zero, val)) { 
        *out = 0;
        return __CILKRTS_SET_PARAM_SUCCESS;
    }

    if (strmatch(s_true, val) || strmatch(s_one, val)) { 
        *out = 1;
        return __CILKRTS_SET_PARAM_SUCCESS;
    }

    return __CILKRTS_SET_PARAM_INVALID;
}

// Stores the integer value represented by the null-terminated string at 'val'
// into the integer object at 'out', restricting the result to the range 'min'
// to 'max', inclusive.  Returns '__CILKRTS_SET_PARAM_SUCCESS' if the conversion
// succeeds and is in range, '__CILKRTS_SET_PARAM_XRANGE' if the conversion
// succeeds but is out of range, and '__CILKRTS_SET_PARAM_INVALID' otherwise.  In
// the case of any error, '*out' is unchanged.
template <typename INT_T, typename CHAR_T>
int store_int(INT_T *out, const CHAR_T *val, INT_T min, INT_T max)
{
    errno = 0;
    long val_as_long = to_long(val);
    if (val_as_long == 0 && errno != 0)
        return __CILKRTS_SET_PARAM_INVALID;
    if (val_as_long < min || val_as_long == LONG_MIN)
        return __CILKRTS_SET_PARAM_XRANGE;
    else if (val_as_long > max || val_as_long == LONG_MAX)
        return __CILKRTS_SET_PARAM_XRANGE;

    *out = val_as_long;
    return __CILKRTS_SET_PARAM_SUCCESS;
}

// Implementaton of cilkg_set_param templatized on character type.
// Windows will instantiate with both char and wchar_t.
// Note that g must have its user settable values set, but need not be fully
// initialized.
template <class CHAR_T>
int set_param_imp(global_state_t* g, const CHAR_T* param, const CHAR_T* value)
{
    static const char* const s_force_reduce     = "force reduce";
    static const char* const s_nworkers         = "nworkers";
    static const char* const s_max_user_workers = "max user workers";
    static const char* const s_local_stacks     = "local stacks";
    static const char* const s_shared_stacks    = "shared stacks";
    static const char* const s_nstacks          = "nstacks";
    static const char* const s_stack_size       = "stack size";

    // We must have a parameter and a value
    if (0 == param)
        return __CILKRTS_SET_PARAM_INVALID;
    if (0 == value)
        return __CILKRTS_SET_PARAM_INVALID;

    if (strmatch(param, s_force_reduce))
    {
        // Sets whether we force a reduce operation at every sync.  Useful for
        // debugging reducers.  Off by default.  Overridden by Cilkscreen
        //
        // Documented in cilk_api_<os>.h
        if (always_force_reduce())
            // Force reduce is set by cilkscreen.  User cannot change it.
            return __CILKRTS_SET_PARAM_LATE;

        return store_bool(&g->force_reduce, value);
    }
    else if (strmatch(param, s_nworkers))
    {
        // Set the total number of workers.  Overrides count of cores we get
        // from the OS and the setting of the CILK_NWORKERS environment
        // variable.  Setting to 0 indicates that the default worker count
        // should be used.
        //
        // Documented in cilk_api_<os>.h
        if (cilkg_singleton_ptr)
            return __CILKRTS_SET_PARAM_LATE;

        // Fetch the number of cores.  There must be at last 1, since we're
        // executing on *something*, aren't we!?
        int hardware_cpu_count = __cilkrts_hardware_cpu_count();
        CILK_ASSERT(hardware_cpu_count > 0);

        int max_cpu_count = 16 * hardware_cpu_count;
        if (__cilkrts_running_under_sequential_ptool())
        {
            hardware_cpu_count = 1;
            max_cpu_count = 1;
        }
        // Allow a value of 0, which means "set to hardware thread count".
        int ret = store_int(&g->P, value, 0, max_cpu_count);
        if (0 == g->P)
            g->P = hardware_cpu_count;
        return ret;
    }
    else if (strmatch(param, s_max_user_workers))
    {
        // ** UNDOCUMENTED **
        //
        // Sets the number of slots allocated for user worker threads
        int hardware_cpu_count = __cilkrts_hardware_cpu_count();
        CILK_ASSERT (hardware_cpu_count > 0);

        return store_int(&g->max_user_workers, value, 1,
                         16 * hardware_cpu_count);
    }
    else if (strmatch(param, s_local_stacks))
    {
        // ** UNDOCUMENTED **
        //
        // Number of stacks we'll hold in the per-worker stack cache.  Maximum
        // value is 42.  See __cilkrts_make_global_state for details.
        return store_int(&g->fiber_pool_size, value, 0, 42);
    }
    else if (strmatch(param, s_shared_stacks))
    {
        // ** UNDOCUMENTED **
        //
        // Maximum number of stacks we'll hold in the global stack
        // cache. Maximum value is 42.  See __cilkrts_make_global_state for
        // details.
        return store_int(&g->global_fiber_pool_size, value, 0, 42);
    }
    else if (strmatch(param, s_nstacks))
    {
        // Sets the maximum number of stacks permitted at one time.  If the
        // runtime reaches this maximum, it will cease to allocate stacks and
        // the app will lose parallelism.  0 means unlimited.  Default is
        // unlimited.  Minimum is twice the number of worker threads, though
        // that cannot be tested at this time.
        //
        // Undocumented at this time, though there are plans to expose it.
        // The current implentation is for Linux debugging only and is not
        // robust enough for users.
        if (cilkg_singleton_ptr)
            return __CILKRTS_SET_PARAM_LATE;
        return store_int<unsigned>(&g->max_stacks, value, 0, INT_MAX);
    }
    else if (strmatch(param, s_stack_size))
    {
        // ** UNDOCUMENTED **
        //
        // Sets the size (in bytes) of the stacks that Cilk creates.
        // Can only be set before the runtime starts.
        if (cilkg_singleton_ptr)
            return __CILKRTS_SET_PARAM_LATE;

        // Maximum value that can be parsed is MAX_INT (32-bit).
        int ret = store_int<size_t>(&g->stack_size, value, 0, INT_MAX);

        // Process the value the user set (or 0 if the user didn't set
        // anything) into something nice for the current OS.  This
        // processing is done immediately and stored into
        // g->stack_size so that a call to get stack size will return
        // the value that the runtime will actually use.
        g->stack_size = cilkos_validate_stack_size(g->stack_size);
        return ret;     
    }


    // If got here, then didn't match any of the strings
    return __CILKRTS_SET_PARAM_UNIMP;
}

inline
int calc_max_user_workers(global_state_t *g)
{
    // If it's been set by the user, give back what we got
    if (g->max_user_workers > 0)
        return g->max_user_workers;

    // Calculate it
    return std::max(3, g->P * 2);
}

} // end unnamed namespace

__CILKRTS_BEGIN_EXTERN_C

/**
 * @brief Returns the global state object.  If called for the first time,
 * initializes the user-settable values in the global state, but does not
 * initialize the rest of the structure.
 */
global_state_t* cilkg_get_user_settable_values()
{
    // Environment variable value.  More than big enough for a 64-bit signed
    // integer.
    char envstr[24];

    // Abbreviating &global_state_singleton as g is not only shorter, it also
    // facilitates grepping for the string "g->", which appears ubiquitously
    // in the runtime code.
    global_state_t* g = &global_state_singleton;

    // TBD: We need synchronization around this loop to prevent
    // multiple threads from initializing this data.
    if (! cilkg_user_settable_values_initialized)
    {
        size_t len;

        // Preserve stealing disabled since it may have been set by the
        // debugger
        int stealing_disabled = g->stealing_disabled;

        // All fields will be zero until set.  In particular
        std::memset(g, 0, sizeof(global_state_t));

        // Fetch the number of cores.  There must be at last 1, since we're
        // executing on *something*, aren't we!?
        int hardware_cpu_count = __cilkrts_hardware_cpu_count();
        CILK_ASSERT(hardware_cpu_count > 0);

        bool under_ptool = __cilkrts_running_under_sequential_ptool();
        if (under_ptool)
            hardware_cpu_count = 1;

        g->stealing_disabled        = stealing_disabled;
        g->under_ptool              = under_ptool;
        g->force_reduce             = 0;   // Default Off
        g->P                        = hardware_cpu_count;   // Defaults to hardware CPU count
        g->max_user_workers         = 0;   // 0 unless set by user
        g->fiber_pool_size          = 7;   // Arbitrary default
        
        g->global_fiber_pool_size   = 3 * 3* g->P;  // Arbitrary default
        // 3*P was the default size of the worker array (including
        // space for extra user workers).  This parameter was chosen
        // to match previous versions of the runtime.

        if (4 == sizeof(void *))
            g->max_stacks           = 1200; // Only 1GB on 32-bit machines
        else
            g->max_stacks           = 2400; // 2GB on 64-bit machines

        // If we have 2400 1MB stacks, that is 2 gb.  If we reach this
        // limit on a single-socket machine, we may have other
        // problems.  Is 2400 too small for large multicore machines?

        // TBD(jsukha, 11/27/2012): I set this limit on stacks to be a
        // value independent of P.  When running on a Xeon Phi with
        // small values of P, I recall seeing a few microbenchmarks
        // (e.g., fib) where a limit of 10*P seemed to be
        // unnecessarily slowing things down.
        // 
        // That being said, the code has changed sufficiently that
        // this observation may no longer be true.
        //
        // Note: in general, the worst-case number of stacks required
        // for a Cilk computation with spawn depth "d" on P workers is
        // O(Pd).  Code with unbalanced recursion may run into issues
        // with this stack usage.

        g->max_steal_failures       = 128; // TBD: depend on max_workers?
        g->stack_size               = 0;   // 0 unless set by the user

        // Assume no record or replay log for now
        g->record_replay_file_name  = NULL;
        g->record_or_replay         = RECORD_REPLAY_NONE;  // set by user

        if (always_force_reduce())
            g->force_reduce = true;
        else if (cilkos_getenv(envstr, sizeof(envstr), "CILK_FORCE_REDUCE"))
            store_bool(&g->force_reduce, envstr);

        if (under_ptool)
            g->P = 1;  // Ignore environment variable if under cilkscreen
        else if (cilkos_getenv(envstr, sizeof(envstr), "CILK_NWORKERS"))
            // Set P to environment variable, but limit to no less than 1
            // and no more than 16 times the number of hardware threads.
            store_int(&g->P, envstr, 1, 16 * hardware_cpu_count);

        if (cilkos_getenv(envstr, sizeof(envstr), "CILK_MAX_USER_WORKERS"))
            // Set max_user_workers to environment variable, but limit to no
            // less than 1 and no more 16 times the number of hardware
            // threads.  If not specified, defaults (somewhat arbitrarily) to
            // the larger of 3 and twice the number of hardware threads.
            store_int(&g->max_user_workers, envstr, 1, 16*hardware_cpu_count);

        if (cilkos_getenv(envstr, sizeof(envstr), "CILK_STEAL_FAILURES"))
            // Set the number of times a worker should fail to steal before
            // it looks to see whether it should suspend itself.
            store_int<unsigned>(&g->max_steal_failures, envstr, 1, INT_MAX);

        // Compute the total number of workers to allocate.  Subtract one from
        // nworkers and user workers so that the first user worker isn't
        // factored in twice.
        //
        // total_workers must be computed now to support __cilkrts_get_total_workers
        g->total_workers = g->P + calc_max_user_workers(g) - 1;

#ifdef CILK_RECORD_REPLAY
        // RecordReplay: See if we've been asked to replay a log
        len = cilkos_getenv(envstr, 0, "CILK_REPLAY_LOG");
        if (len > 0)
        {
            len += 1;    // Allow for trailing NUL
            g->record_or_replay = REPLAY_LOG;
            g->record_replay_file_name = (char *)__cilkrts_malloc(len);
            cilkos_getenv(g->record_replay_file_name, len, "CILK_REPLAY_LOG");
        }

        // RecordReplay: See if we've been asked to record a log
        len = cilkos_getenv(envstr, 0, "CILK_RECORD_LOG");
        if (len > 0)
        {
            if (RECORD_REPLAY_NONE != g->record_or_replay)
                cilkos_warning("CILK_RECORD_LOG ignored since CILK_REPLAY_LOG is defined.\n");
            else
            {
                len += 1;    // Allow for trailing NUL
                g->record_or_replay = RECORD_LOG;
                g->record_replay_file_name = (char *)__cilkrts_malloc(len);
                cilkos_getenv(g->record_replay_file_name, len, "CILK_RECORD_LOG");
            }
        }
#endif
        
        cilkg_user_settable_values_initialized = true;
    }

    return g;
}

int cilkg_calc_total_workers()
{
    global_state_t* g = cilkg_get_user_settable_values();

    // Compute the total number of workers to allocate.  Subtract one from
    // nworkers and user workers so that the first user worker isn't
    // factored in twice.
    return g->P + calc_max_user_workers(g) - 1;
}

// Should be called while holding the global lock.
global_state_t* cilkg_init_global_state()
{
    if (cilkg_singleton_ptr)
        return cilkg_singleton_ptr;

    // Get partially-initialized global state.
    global_state_t* g = cilkg_get_user_settable_values();

    if (g->max_stacks > 0) {

        // nstacks is currently honored on non-Windows systems only.

        // Set an upper bound on the number of stacks that are allocated.  If
        // nstacks is set, each worker gets up to one stack in its cache so that
        // no one worker can hog all of the free stacks and keep work from being
        // stolen by the other workers.

        // nstacks corresponds to the number of stacks that will be allocated by
        // the runtime apart from the initial stack created for each thread by
        // the system.  Therefore, if a user asks for n stacks, and there are
        // p workers created, the total number of stacks is actually n + p.

        // This feature is primarily for MIC which has flat memory
        // instead of virtual addresses and tends to run out really quickly.
        // It is not implemented for Windows and it's non-intuitive
        // interaction with the local stack cache is specifically to help out
        // MIC.

        // About max_stacks / P stacks, except we require at least 1
        // per pool.
        if (((int)g->max_stacks / g->P) < g->fiber_pool_size)
            g->fiber_pool_size = g->max_stacks / g->P;

        if (g->fiber_pool_size <= 0) {
            g->fiber_pool_size = 1;
        }
        
        if ((int)g->max_stacks < g->P)
            g->max_stacks = g->P;

        g->global_fiber_pool_size = g->P * (g->fiber_pool_size+1);
    }

    // Number of bytes/address - validation for debugger integration
    g->addr_size = sizeof(void *);

    __cilkrts_init_stats(&g->stats);

    __cilkrts_frame_malloc_global_init(g);

    g->Q = 0;
    g->total_workers = cilkg_calc_total_workers();
    g->system_workers = g->P - 1; // system_workers is here for the debugger.
    g->work_done = 0;
    g->workers_running = 0;
    g->ltqsize = 1024; /* FIXME */

    g->stack_size = cilkos_validate_stack_size(g->stack_size);
    g->failure_to_allocate_stack = 0;

    return g;
}

void cilkg_publish_global_state(global_state_t* g) 
{
    // TBD: which one of these needs to be executed first?  I say
    // cilkg_singleton_ptr needs to be set last, with a mfence in
    // between, since it is the flag that cilkg_is_published_is
    // checking for.
    __cilkrts_global_state = g;
    __cilkrts_fence();
    cilkg_singleton_ptr = g;
}

void cilkg_deinit_global_state()
{
    cilkg_singleton_ptr = NULL;

    // The pointer to the global state needs to remain valid for the
    // debugger.  Thus, we can't clear the following pointer.
    //    __cilkrts_global_state = NULL;


    // We also don't reset the global state, so that if we resume
    // execution after ending Cilk, user set variables (e.g., # of
    // workers) remains valid.
}

int cilkg_is_published(void)
{
    return NULL != cilkg_singleton_ptr;
}

int cilkg_set_param(const char* param, const char* value)
{
    return set_param_imp(cilkg_get_user_settable_values(), param, value);
}

#ifdef _WIN32
int cilkg_set_param_w(const wchar_t* param, const wchar_t* value)
{
    return set_param_imp(cilkg_get_user_settable_values(), param, value);
}
#endif

extern "C++" {
    // C++ scheduler function (that may throw exceptions)
    typedef void cpp_scheduler_t(__cilkrts_worker *w);
}

void __cilkrts_run_scheduler_with_exceptions(__cilkrts_worker *w)
{
    global_state_t* g = cilkg_get_global_state();
    CILK_ASSERT(g->scheduler);

    cpp_scheduler_t* scheduler = (cpp_scheduler_t*) g->scheduler;

    try {
        scheduler(w);
    } catch (...) {
        __cilkrts_bug("Exception escaped Cilk context");
    }
}

__CILKRTS_END_EXTERN_C

/* End global_state.cpp */
