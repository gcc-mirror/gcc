/* Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libitm_i.h"

using namespace GTM;

/* Exceptions can exist in three phases: (1) after having been allocated by
   __cxa_allocate_exception but before being handed off to __cxa_throw,
   (2) when they are in flight, so between __cxa_throw and __cxa_begin_catch,
   and (3) when they are being handled (between __cxa_begin_catch and
   __cxa_end_catch).  Note that when an exception is re-thrown in (3), it is
   not moving back to (2) but handled as a special case of (3) by the EH
   runtime.

   We can get aborts in all three phases, for example in (1) during
   construction of the exception object, or in (2) in destructors called
   while unwinding the stack.  The transaction that created an exception
   object can only commit in phase (3) by re-throwing the exception; it cannot
   commit in other phases because throw expressions and catch clauses are
   properly nested wrt transactions and because the compiler wraps
   transaction bodies in a try/catch-all construct.

   We handle phase (1) by dealing with exception objects similar to how we
   deal with other (de)allocations, which also ensures that we can have more
   than one exception object allocated at the same time (e.g., if the
   throw expression itself throws an exception and thus calls
   __cxa_allocate_exception).  However, on the call to __cxa_begin_catch
   we hand off the exception to the special handling of phase (3) and
   remove the undo log entry of the allocation.  Note that if the allocation
   happened outside of this transaction, we do not need to do anything.

   When an exception reaches phase (2) due to a call to __cxa_throw, the count
   of uncaught exceptions is incremented.  We roll back this effect by saving
   and restoring this number in the structure returned from __cxa_get_globals.
   This also takes care of increments of this count when re-throwing an
   exception.

   For phase (3), we keep track of the number of times __cxa_begin_catch
   has been called without a matching call to __cxa_end_catch.  This count
   is then used by __cxa_tm_cleanup to roll back the exception handling state
   by calling __cxa_end_catch for the exceptions that have not been finished
   yet (without running destructors though because we roll back the memory
   anyway).
   Once an exception that was allocated in this transaction enters phase (3),
   it does not need to be deallocated on abort anymore because the calls to
   __cxa_end_catch will take care of that.

   We require all code executed by the transaction to be transaction_safe (or
   transaction_pure, or to have wrappers) if the transaction is to be rolled
   back.  However, we take care to not require this for transactions that
   just commit; this way, transactions that enter serial mode and then call
   uninstrumented code continue to work.
   */

/* Everything from libstdc++ is weak, to avoid requiring that library
   to be linked into plain C applications using libitm.so.  */

#define WEAK  __attribute__((weak))

extern "C" {

struct __cxa_eh_globals
{
  void *	caughtExceptions;
  unsigned int	uncaughtExceptions;
};

extern void *__cxa_allocate_exception (size_t) _ITM_NOTHROW WEAK;
extern void __cxa_free_exception (void *) _ITM_NOTHROW WEAK;
extern void __cxa_throw (void *, void *, void (*) (void *)) WEAK;
extern void *__cxa_begin_catch (void *) _ITM_NOTHROW WEAK;
extern void __cxa_end_catch (void) WEAK;
extern void __cxa_tm_cleanup (void *, void *, unsigned int) throw () WEAK;
extern __cxa_eh_globals *__cxa_get_globals (void) _ITM_NOTHROW WEAK;

#if !defined (HAVE_ELF_STYLE_WEAKREF) 
void *__cxa_allocate_exception (size_t) _ITM_NOTHROW { return NULL; }
void __cxa_free_exception (void *) _ITM_NOTHROW { return; }
void __cxa_throw (void *, void *, void (*) (void *)) { return; }
void *__cxa_begin_catch (void *) _ITM_NOTHROW { return NULL; }
void __cxa_end_catch (void) { return; }
void __cxa_tm_cleanup (void *, void *, unsigned int) throw () { return; }
void _Unwind_DeleteException (_Unwind_Exception *) { return; }
__cxa_eh_globals *__cxa_get_globals (void) _ITM_NOTHROW { return NULL; }
#endif /* HAVE_ELF_STYLE_WEAKREF */

}

static void
free_any_exception (void *exc_ptr)
{
  // The exception could be in phase (2) and thus calling just
  // _cxa_free_exception might not be sufficient.
  __cxa_tm_cleanup (NULL, exc_ptr, 0);
}

void *
_ITM_cxa_allocate_exception (size_t size) _ITM_NOTHROW
{
  void *r = __cxa_allocate_exception (size);
  gtm_thr()->record_allocation (r, free_any_exception);
  return r;
}

void
_ITM_cxa_free_exception (void *exc_ptr) _ITM_NOTHROW
{
  // __cxa_free_exception can be called from user code directly if
  // construction of an exception object throws another exception, in which
  // case we need to roll back the initial exception.  We handle this similar
  // to dead allocations in that we deallocate the exception on both commit
  // and abort of an outermost transaction.
  gtm_thr()->forget_allocation (exc_ptr, free_any_exception);
}

void
_ITM_cxa_throw (void *obj, void *tinfo, void (*dest) (void *))
{
  // This used to be instrumented, but does not need to be anymore.
  __cxa_throw (obj, tinfo, dest);
}

void *
_ITM_cxa_begin_catch (void *exc_ptr) _ITM_NOTHROW
{
  // If this exception object has been allocated by this transaction, we
  // discard the undo log entry for the allocation; we are entering phase (3)
  // now and will handle this exception specially.
  // Note that this exception cannot have been allocated in a parent
  // transaction or enclosing nontransactional block because an atomic block
  // cannot contain just a catch clause but not the associated try clause.
  // The exception can have been allocated in a nested transaction, in which
  // case the commit of the nested transaction will have inserted the undo
  // log entry of the allocation in our undo log.
  // The exception can also have been allocated in a nested nontransactional
  // block, but then this transaction cannot abort anymore; functions that
  // are marked transaction_pure, for example, must not side-step the
  // transactional exception handling we implement here.
  gtm_thread *t = gtm_thr ();
  t->discard_allocation (exc_ptr);
  // Keep track of the number of unfinished catch handlers.
  t->cxa_catch_count++;
  return __cxa_begin_catch (exc_ptr);
}

void
_ITM_cxa_end_catch (void)
{
  // Keep track of the number of unfinished catch handlers.
  gtm_thr()->cxa_catch_count--;
  __cxa_end_catch ();
}

void
GTM::gtm_thread::init_cpp_exceptions ()
{
  // Only save and restore the number of uncaught exceptions if this is
  // actually used in the program.
  if (
#if HAVE_ELF_STYLE_WEAKREF
      __cxa_get_globals != NULL &&
#endif
      __cxa_get_globals () != 0)
    cxa_uncaught_count_ptr = &__cxa_get_globals ()->uncaughtExceptions;
  else
    cxa_uncaught_count_ptr = 0;
}

void
GTM::gtm_thread::revert_cpp_exceptions (gtm_transaction_cp *cp)
{
  if (cp)
    {
      // If rolling back a nested transaction, only clean up incompletely
      // caught exceptions since the last checkpoint.
      assert (cxa_catch_count >= cp->cxa_catch_count);
      uint32_t catch_count = cxa_catch_count - cp->cxa_catch_count;
      if (catch_count)
	{
	  __cxa_tm_cleanup (NULL, NULL, catch_count);
	  cxa_catch_count = cp->cxa_catch_count;
	}
    }
  else
    {
      // Both cxa_catch_count and cxa_unthrown are maximal because EH regions
      // and transactions are properly nested.
      if (cxa_catch_count)
	{
	  __cxa_tm_cleanup (NULL, NULL, cxa_catch_count);
	  cxa_catch_count = 0;
	}
    }
  // Reset the number of uncaught exceptions.  Any allocations for these
  // exceptions have been rolled back already, if necessary.
  if (cxa_uncaught_count_ptr != 0)
    *cxa_uncaught_count_ptr = cxa_uncaught_count;
  // Always reset eh_in_flight because it just contains the argument provided
  // to _ITM_commitTransactionEH.
  eh_in_flight = NULL;
}
