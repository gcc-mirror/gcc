/* except-gcc.cpp                  -*-C++-*-
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

#include "except-gcc.h"
#include "except.h"
#include "sysdep.h"
#include "bug.h"
#include "local_state.h"
#include "full_frame.h"
#include "scheduler.h"
#include "frame_malloc.h"
#include "pedigrees.h"

#include <stdint.h>
#include <typeinfo>

#define DEBUG_EXCEPTIONS 0

struct pending_exception_info
{
    void make(__cxa_eh_globals *, _Unwind_Exception *, bool);
    void destruct();
    bool empty() const;
    void check() const;
    /* Active exception at time of suspend. */
    _Unwind_Exception *active;
    /* If true the most recently caught exception is to be rethrown
       on resume.  This handling is technically incorrect but allows
       running without compiler support; the proper standards-compliant
       method is to save the exception in the previous field. */
    bool rethrow;
    struct __cxa_eh_globals runtime_state;
};

void pending_exception_info::check() const
{
    if (active)
        CILK_ASSERT((int)runtime_state.uncaughtExceptions > 0);
}

void pending_exception_info::make(__cxa_eh_globals *state_in,
                                  _Unwind_Exception *exc_in, bool rethrow_in)
{
    active = exc_in;
    rethrow = rethrow_in;
    runtime_state = *state_in;
    /* Read and clear C++ runtime state.  */
    state_in->caughtExceptions = 0;
    state_in->uncaughtExceptions = 0;
#if CILK_LIB_DEBUG
    check();
#endif
}

bool
pending_exception_info::empty() const
{
    return !active && !rethrow && !runtime_state.caughtExceptions &&
        !runtime_state.uncaughtExceptions;
}

#if DEBUG_EXCEPTIONS
#include <stdio.h>
static void
decode_exceptions(char *out, size_t len, struct pending_exception_info *info)
{
    if (info->empty())
        snprintf(out, len, "[empty]");
    else if (info->rethrow)
        snprintf(out, len, "[rethrow %p]",
                 info->runtime_state.caughtExceptions);
    else
        snprintf(out, len, "[throw %p]", (void *)info->active);
}
#endif

static void
save_exception_info(__cilkrts_worker *w,
                    __cxa_eh_globals *state,
                    _Unwind_Exception *exc,
                    bool rethrow,
                    const char *why)
{
    struct pending_exception_info *info =
        (struct pending_exception_info *)__cilkrts_frame_malloc(w, sizeof (struct pending_exception_info));
    CILK_ASSERT(info);
    info->make(state, exc, rethrow);

#if DEBUG_EXCEPTIONS
    {
        char buf[40];
        decode_exceptions(buf, sizeof buf, info);
        fprintf(stderr, "make exception info W%u %p %s (%s)\n",
                w->self, info, buf, why);
    }
#endif

    CILK_ASSERT(w->l->pending_exception == 0);
    w->l->pending_exception = info;
}

#if DEBUG_EXCEPTIONS
#include <stdio.h> /* DEBUG */

static void decode_flags(int flags, char out[9])
{
  out[0] = (flags & CILK_FRAME_STOLEN) ? 'S' : '_';
  out[1] = (flags & CILK_FRAME_UNSYNCHED) ? 'U' : '_';
  out[2] = (flags & CILK_FRAME_DETACHED) ? 'D' : '_';
  out[3] = (flags & CILK_FRAME_EXCEPTING) ? 'X' : '_';
  out[4] = '\0';
}
#endif

/* __cilkrts_save_except is called from the runtime epilogue
   when a function is returning with an exception pending.

   If the function has a parent to which it could return normally,
   return and have the caller call _Unwind_Resume, the same as if
   an exception filter had not matched.

   Otherwise save the exception in the worker.

   If this is a return from a ordinary call that must go through
   the runtime, the assembly epilogue must have saved the call-saved
   register state in the parent frame. */

extern "C"
CILK_ABI_THROWS_VOID
__cilkrts_return_exception(__cilkrts_stack_frame *sf)
{
    __cilkrts_worker *w = sf->worker;
    _Unwind_Exception *exc = (_Unwind_Exception *)sf->except_data;

    CILK_ASSERT(sf->flags & CILK_FRAME_DETACHED);
    sf->flags &= ~CILK_FRAME_DETACHED;

   /*
    * If we are in replay mode, and a steal occurred during the recording
    * phase, stall till a steal actually occurs.
    */
    replay_wait_for_steal_if_parent_was_stolen(w);

    /* If this is to be an abnormal return, save the active exception. */
    if (!__cilkrts_pop_tail(w)) {
        /* Write a record to the replay log for an attempt to return to a
           stolen parent.  This must be done before the exception handler
           invokes __cilkrts_leave_frame which will bump the pedigree so
           the replay_wait_for_steal_if_parent_was_stolen() above will match on
           replay */
        replay_record_orphaned(w);

        /* Now that the record/replay stuff is done, update the pedigree */
        update_pedigree_on_leave_frame(w, sf);

        /* Inline pop_frame; this may not be needed. */
        w->current_stack_frame = sf->call_parent;
        sf->call_parent = 0;
        __cxa_eh_globals *state = __cxa_get_globals();

#if DEBUG_EXCEPTIONS
        fflush(stdout);
        char decoded[9];
        decode_flags(sf->flags, decoded);
        fprintf(stderr, "__cilkrts_save_except W%u sf %p/%s exc %p [%u %p] suspend\n",
                w->self, sf, decoded, exc,
                state->uncaughtExceptions,
                state->caughtExceptions);
#endif

        /* Like __cilkrts_save_exception_state except for setting the
           rethrow flag. */
        save_exception_info(w, state, exc, exc == NULL, "save_except");
        {
            full_frame *ff = w->l->frame_ff;
            CILK_ASSERT(NULL == ff->pending_exception);
            ff->pending_exception = w->l->pending_exception;
            w->l->pending_exception = NULL;
        }
        __cilkrts_exception_from_spawn(w, sf); /* does not return */
    }
    /* This code path is taken when the parent is attached.  It is on
       the same stack and part of the same full frame.  The caller is
       cleaning up the Cilk frame during unwind and will reraise the
       exception */

    /* Now that the record/replay stuff is done, update the pedigree */
    update_pedigree_on_leave_frame(w, sf);

#if DEBUG_EXCEPTIONS /* DEBUG ONLY */
    {
        __cxa_eh_globals *state = __cxa_get_globals();

        fflush(stdout);
        char decoded[9];
        decode_flags(sf->flags, decoded);
        fprintf(stderr, "__cilkrts_save_except W%d %p/%s %p->%p [%u %p] escape\n",
                w->self, sf, decoded, exc,
                exc ? to_cxx(exc)->nextException : 0,
                state->uncaughtExceptions,
                state->caughtExceptions);

        /* XXX This is triggering in the user thread which gets an exception
           from somewhere but does not get the corresponding runtime exception
           state.
           XXX There might be two or more uncaught exceptions.  Test could be
           (uncaught != 0) == (exc != 0).  First, design tests to see if that
           case is otherwise handled correctly.  And what if there's an uncaught
           exception that does not belong to this function?  I.e. this is a return
           from spawn in a destructor. */
        if (exc)
            CILK_ASSERT((int)state->uncaughtExceptions > 0);
        /*CILK_ASSERT(state->uncaughtExceptions == (exc != 0));*/
    }
#endif
    
    /* The parent is attached so this exception can be propagated normally. */
    return;
}

/* Save the exception state into the full frame, which is exiting
   or suspending. */
extern "C"
void __cilkrts_save_exception_state(__cilkrts_worker *w, full_frame *ff)
{
    save_exception_info(w, __cxa_get_globals(), 0, false, "undo-detach");
    CILK_ASSERT(NULL == ff->pending_exception);
    ff->pending_exception = w->l->pending_exception;
    w->l->pending_exception = NULL;    
}

/* __cilkrts_c_sync_except is like __cilkrts_c_sync except that it
   saves exception state.  __cilkrts_c_sync never returns here and
   always reinstalls the saved exception state.

   This function must be used because a parent of this function may
   be propagating an uncaught exception.  The uncaught exception
   count must be saved by the child and passed back to the parent. */

extern "C"
NORETURN __cilkrts_c_sync_except (__cilkrts_worker *w, __cilkrts_stack_frame *sf)
{
    __cxa_eh_globals *state = __cxa_get_globals();
    _Unwind_Exception *exc = (_Unwind_Exception *)sf->except_data;

    CILK_ASSERT((sf->flags & (CILK_FRAME_UNSYNCHED|CILK_FRAME_EXCEPTING)) ==
                (CILK_FRAME_UNSYNCHED|CILK_FRAME_EXCEPTING));
    sf->flags &= ~CILK_FRAME_EXCEPTING;

#if DEBUG_EXCEPTIONS
    fflush(stdout);
    char decoded[9];
    decode_flags(sf->flags, decoded);
    if (exc)
        fprintf(stderr, "__cilkrts_sync_except W%u %p/%s %p->%p [%u %p]\n",
                w->self, sf, decoded, exc,
                to_cxx(exc)->nextException,
                state->uncaughtExceptions,
                state->caughtExceptions);
    else
        fprintf(stderr, "__cilkrts_sync_except W%d %p/%s none [%u %p]\n",
                w->self, sf, decoded,
                state->uncaughtExceptions,
                state->caughtExceptions);
#endif

    /* Here the identity of an rethrown exception is always known.
       If exc is NULL this call is only to preserve parent state. */
    save_exception_info(w, state, exc, false, "sync_except");
#if 0
    {
        full_frame *ff = w->l->frame_ff;
        CILK_ASSERT(NULL == ff->pending_exception);
        ff->pending_exception = w->l->pending_exception;
        w->l->pending_exception = NULL;    
    }
#endif
    CILK_ASSERT(!std::uncaught_exception());
    __cilkrts_c_sync(w, sf);
}

void
pending_exception_info::destruct()
{
    if (active) {
#if DEBUG_EXCEPTIONS
        fprintf(stderr, "destroy exception info %p %p\n", this, active);
#endif
        _Unwind_DeleteException(active);
        active = 0;
    } else {
#if DEBUG_EXCEPTIONS
        fprintf(stderr, "destroy exception info %p\n", this);
#endif
    }
    while (runtime_state.caughtExceptions) {
        __cxa_exception *exc = runtime_state.caughtExceptions;
        runtime_state.caughtExceptions = exc->nextException;
#if DEBUG_EXCEPTIONS
        fprintf(stderr, "destroy caught exception %p\n", this);
#endif
        _Unwind_DeleteException(&exc->unwindHeader);
    }
}

/*
 * __cilkrts_merge_pending_exceptions
 *
 * Merge the right exception record into the left.  The left is logically
 * earlier.
 *
 * The active exception of E is
 * E->active if it is non-NULL (in which case E->rethrow is false)
 * unresolved if E->active is NULL and E->rethrow is true
 * nil if E->active is NULL and E->rethrow is false
 *
 * The merged active exception is left active exception if it is not
 * nil, otherwise the right.
 *
 * On entry the left state is synched and can not have an unresolved
 * exception.  The merge may result in an unresolved exception.
 *
 * Due to scoping rules at most one of the caught exception lists is
 * non-NULL.
 */

struct pending_exception_info *
__cilkrts_merge_pending_exceptions (
    __cilkrts_worker *w,
    struct pending_exception_info *left,
    struct pending_exception_info *right)
{
    /* If we've only got one exception, return it */

    if (NULL == left) {
#if DEBUG_EXCEPTIONS
        if (right) {
            char buf[40];
            decode_exceptions(buf, sizeof buf, right);
            fprintf(stderr, "__cilkrts merge W%u nil %p -> %p %s\n",
                    w->self, right, right, buf);
        }
#endif
        return right;
    }

    if (NULL == right) {
#if DEBUG_EXCEPTIONS
        if (left) {
            char buf[40];
            decode_exceptions(buf, sizeof buf, left);
            fprintf(stderr, "__cilkrts merge W%u %p nil -> %p %s\n",
                    w->self, left, left, buf);
        }
#endif
        return left;
    }

#if CILK_LIB_DEBUG
    /*volatile struct pending_exception_info left_in = *left, right_in = *right;*/
    left->check();
    right->check();
#endif

#if DEBUG_EXCEPTIONS
    {
        char buf1[40], buf2[40];
        decode_exceptions(buf1, sizeof buf1, left);
        decode_exceptions(buf2, sizeof buf2, right);
        fprintf(stderr, "__cilkrts merge W%u %p %s %p %s\n",
                w->self, left, buf1, right, buf2);
    }
#endif

    /* It should not be possible for both left and right to
       have accumulated catch blocks.

       The left exception record may always have a catch
       chain it kept when its parent was stolen.

       If they are siblings, the right sibling should not
       have accumulated any net catches.  (Catch is lexically
       scoped.)

       If the right frame is a parent, it should not have entered
       a catch block without syncing first.  If it spawned in a
       catch block, the child got its catch. */
    __cxa_exception *caught = left->runtime_state.caughtExceptions;
    if (caught)
        CILK_ASSERT(!right->runtime_state.caughtExceptions);
    else {
        CILK_ASSERT(!left->rethrow);
        left->rethrow = right->rethrow;
        left->runtime_state.caughtExceptions = caught = right->runtime_state.caughtExceptions;
        right->runtime_state.caughtExceptions = NULL;
    }

    /* Merge the uncaught exception and count of uncaught exceptions. */
    const unsigned int right_uncaught = right->runtime_state.uncaughtExceptions;
    if (!left->active){
        left->active = right->active; /* could be NULL */
        right->active = 0;
        left->runtime_state.uncaughtExceptions += right_uncaught;
        if (left->active)
            /* assert is C++ exception */
            /*CILK_ASSERT(__cxxabiv1::__is_gxx_exception_class(left->active->exception_class))*/;
    } else {
        /* Subtract 1 if the right exception is being destructed. */
        left->runtime_state.uncaughtExceptions += right_uncaught - (right->active != 0);
    }

    right->destruct();
    __cilkrts_frame_free(w, right, sizeof *right);

    /* If there is no state left, return NULL. */
    if (left->empty()) {
        left->destruct();
        __cilkrts_frame_free(w, left, sizeof *left);
        left = NULL;
    }

#if CILK_LIB_DEBUG
    if (left)
        left->check();
#endif

    return left;
}

#if 0
/* __cilkrts_c_resume_except is called from the assembly language
   restart code when a resumed frame has a pending exception.

   The handler count negation on rethrow was done when the throw was
   resolved.

   The assembly language runtime must make the throw unwind to
   the sync, spawn, or other location where the exception should
   be injected.  (This should not happen after a spawn but nothing
   here depends on there being no exception on steal.)

   This function is unused in the Intel stack based system. */
extern "C"
void __cilkrts_c_resume_except (_Unwind_Exception *exc)
{
#if DEBUG_EXCEPTIONS
    fprintf(stderr, "resume exception %p\n", exc);
#endif
    _Unwind_Reason_Code why = _Unwind_RaiseException(exc);
    __cilkrts_bug ("Cilk runtime error: failed to reinstate suspended exception %p (%d)\n", exc, why);
}
#endif

/* Restore the caught exception chain.  This assumes no C++ exception
   code will run before the frame is resumed.  If there is no exception
   to be resumed free the object. */

extern "C"
void __cilkrts_setup_for_execution_sysdep(__cilkrts_worker *w, full_frame *ff)
{
    // ASSERT: We own w->lock and ff->lock || P == 1

    __cxa_eh_globals *state = __cxa_get_globals ();
    struct pending_exception_info *info = w->l->pending_exception;

    if (info == NULL)
        return;

    w->l->pending_exception = 0;

#if DEBUG_EXCEPTIONS
    _Unwind_Exception *exc = info->active;
    if (exc) {
        fflush(stdout);
        fprintf(stderr, "__cilkrts_resume_except W%u %p->%p [%u %p]\n",
                w->self, exc,
                to_cxx(exc)->nextException,
                info->runtime_state.uncaughtExceptions,
                info->runtime_state.caughtExceptions);
        /*CILK_ASSERT(info->runtime_state.uncaughtExceptions > 0);*/
    }
#endif

    if (state->uncaughtExceptions || state->caughtExceptions)
        __cilkrts_bug("W%u: resuming with non-empty prior exception state %u %p\n", state->uncaughtExceptions, state->caughtExceptions);

    *state = info->runtime_state;
    info->runtime_state.caughtExceptions = 0;
    info->runtime_state.uncaughtExceptions = 0;

    if (info->rethrow) {
        info->rethrow = false;
        /* Resuming function will rethrow.  Runtime calls
           std::terminate if there is no caught exception. */
        ff->call_stack->flags |= CILK_FRAME_EXCEPTING;
    }
    if (info->active) {
        ff->call_stack->flags |= CILK_FRAME_EXCEPTING;
        ff->call_stack->except_data = info->active;
        info->active = 0;
    }

    if (info->empty()) {
        info->destruct();
        __cilkrts_frame_free(w, info, sizeof *info);
        w->l->pending_exception = NULL;
    }

#if CILK_LIB_DEBUG
    if (ff->call_stack->except_data)
        CILK_ASSERT(std::uncaught_exception());
#endif
}

#if 0
extern "C"
struct pending_exception_info *__cilkrts_get_exception(__cilkrts_worker *w,
                                                       __cilkrts_stack_frame *sf)
{
    struct pending_exception_info *info = w->l->pending_exception;

    if (info == NULL) {
        sf->flags &= ~CILK_FRAME_EXCEPTING;
        return 0;
    }

    w->l->pending_exception = NULL;

    /* This exception goes into the frame. */

    _Unwind_Exception *exc = info->active;
    info->active = NULL;
    info->destruct();
    __cilkrts_frame_free(w, info, sizeof *info);
    info = 0;
    sf->flags |= CILK_FRAME_EXCEPTING;
    sf->exception = exc;
    return 0;
}
#endif

extern "C"
void __attribute__((nonnull)) __cilkrts_gcc_rethrow(__cilkrts_stack_frame *sf)
{
#ifdef __CYGWIN__
    // Cygwin doesn't support exceptions, so _Unwind_Resume isn't available
    // Which means we can't support exceptions either
    __cilkrts_bug("The Cygwin implementation of the Intel Cilk Plus runtime doesn't support exceptions\n");
#else
    if (sf->except_data) {
#if CILK_LIB_DEBUG
        CILK_ASSERT(std::uncaught_exception());
#endif        
        _Unwind_Resume ((_Unwind_Exception *)sf->except_data);
    } else {
        throw;
    }
#endif  // __CYGWIN__
}

/* End except-gcc.cpp */

