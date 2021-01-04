/* Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "libitm_i.h"

// The default TM method used when starting a new transaction.  Initialized
// in number_of_threads_changed() below.
// Access to this variable is always synchronized with help of the serial
// lock, except one read access that happens in decide_begin_dispatch() before
// a transaction has become active (by acquiring the serial lock in read or
// write mode).  The default_dispatch is only changed and initialized in
// serial mode.  Transactions stay active when they restart (see beginend.cc),
// thus decide_retry_strategy() can expect default_dispatch to be unmodified.
// See decide_begin_dispatch() for further comments.
static std::atomic<GTM::abi_dispatch*> default_dispatch;
// The default TM method as requested by the user, if any.
static GTM::abi_dispatch* default_dispatch_user = 0;

void
GTM::gtm_thread::decide_retry_strategy (gtm_restart_reason r)
{
  struct abi_dispatch *disp = abi_disp ();

  this->restart_reason[r]++;
  this->restart_total++;

  if (r == RESTART_INIT_METHOD_GROUP)
    {
      // A re-initializations of the method group has been requested. Switch
      // to serial mode, initialize, and resume normal operation.
      if ((state & STATE_SERIAL) == 0)
	{
	  // We have to eventually re-init the method group. Therefore,
	  // we cannot just upgrade to a write lock here because this could
	  // fail forever when other transactions execute in serial mode.
	  // However, giving up the read lock then means that a change of the
	  // method group could happen in-between, so check that we're not
	  // re-initializing without a need.
	  // ??? Note that we can still re-initialize too often, but avoiding
	  // that would increase code complexity, which seems unnecessary
	  // given that re-inits should be very infrequent.
	  serial_lock.read_unlock(this);
	  serial_lock.write_lock();
	  if (disp->get_method_group()
	      == default_dispatch.load(memory_order_relaxed)
	      ->get_method_group())
	    // Still the same method group.
	    disp->get_method_group()->reinit();
	  serial_lock.write_unlock();
	  // Also, we're making the transaction inactive, so when we become
	  // active again, some other thread might have changed the default
	  // dispatch, so we run the same code as for the first execution
	  // attempt.
	  disp = decide_begin_dispatch(prop);
	  set_abi_disp(disp);
	}
      else
	// We are a serial transaction already, which makes things simple.
	disp->get_method_group()->reinit();

      return;
    }

  bool retry_irr = (r == RESTART_SERIAL_IRR);
  bool retry_serial = (retry_irr || this->restart_total > 100);

  // We assume closed nesting to be infrequently required, so just use
  // dispatch_serial (with undo logging) if required.
  if (r == RESTART_CLOSED_NESTING)
    retry_serial = true;

  if (retry_serial)
    {
      // In serialirr_mode we can succeed with the upgrade to
      // write-lock but fail the trycommit.  In any case, if the
      // write lock is not yet held, grab it.  Don't do this with
      // an upgrade, since we've no need to preserve the state we
      // acquired with the read.
      // Note that we will be restarting with either dispatch_serial or
      // dispatch_serialirr, which are compatible with all TM methods; if
      // we would retry with a different method, we would have to first check
      // whether the default dispatch or the method group have changed. Also,
      // the caller must have rolled back the previous transaction, so we
      // don't have to worry about things such as privatization.
      if ((this->state & STATE_SERIAL) == 0)
	{
	  this->state |= STATE_SERIAL;
	  serial_lock.read_unlock (this);
	  serial_lock.write_lock ();
	}

      // We can retry with dispatch_serialirr if the transaction
      // doesn't contain an abort and if we don't need closed nesting.
      if ((this->prop & pr_hasNoAbort) && (r != RESTART_CLOSED_NESTING))
	retry_irr = true;
    }

  // Note that we can just use serial mode here without having to switch
  // TM method sets because serial mode is compatible with all of them.
  if (retry_irr)
    {
      this->state = (STATE_SERIAL | STATE_IRREVOCABLE);
      disp = dispatch_serialirr ();
      set_abi_disp (disp);
    }
  else if (retry_serial)
    {
      disp = dispatch_serial();
      set_abi_disp (disp);
    }
}


// Decides which TM method should be used on the first attempt to run this
// transaction.  Acquires the serial lock and sets transaction state
// according to the chosen TM method.
GTM::abi_dispatch*
GTM::gtm_thread::decide_begin_dispatch (uint32_t prop)
{
  abi_dispatch* dd;
  // TODO Pay more attention to prop flags (eg, *omitted) when selecting
  // dispatch.
  // ??? We go irrevocable eagerly here, which is not always good for
  // performance.  Don't do this?
  if ((prop & pr_doesGoIrrevocable) || !(prop & pr_instrumentedCode))
    dd = dispatch_serialirr();

  else
    {
      // Load the default dispatch.  We're not an active transaction and so it
      // can change concurrently but will still be some valid dispatch.
      // Relaxed memory order is okay because we expect each dispatch to be
      // constructed properly already (at least that its closed_nesting() and
      // closed_nesting_alternatives() will return sensible values).  It is
      // harmless if we incorrectly chose the serial or serialirr methods, and
      // for all other methods we will acquire the serial lock in read mode
      // and load the default dispatch again.
      abi_dispatch* dd_orig = default_dispatch.load(memory_order_relaxed);
      dd = dd_orig;

      // If we might need closed nesting and the default dispatch has an
      // alternative that supports closed nesting, use it.
      // ??? We could choose another TM method that we know supports closed
      // nesting but isn't the default (e.g., dispatch_serial()). However, we
      // assume that aborts that need closed nesting are infrequent, so don't
      // choose a non-default method until we have to actually restart the
      // transaction.
      if (!(prop & pr_hasNoAbort) && !dd->closed_nesting()
	  && dd->closed_nesting_alternative())
	dd = dd->closed_nesting_alternative();

      if (!(dd->requires_serial() & STATE_SERIAL))
	{
	  // The current dispatch is supposedly a non-serial one.  Become an
	  // active transaction and verify this.  Relaxed memory order is fine
	  // because the serial lock itself will have established
	  // happens-before for any change to the selected dispatch.
	  serial_lock.read_lock (this);
	  if (default_dispatch.load(memory_order_relaxed) == dd_orig)
	    return dd;

	  // If we raced with a concurrent modification of default_dispatch,
	  // just fall back to serialirr.  The dispatch choice might not be
	  // up-to-date anymore, but this is harmless.
	  serial_lock.read_unlock (this);
	  dd = dispatch_serialirr();
	}
    }

  // We are some kind of serial transaction.
  serial_lock.write_lock();
  state = dd->requires_serial();
  return dd;
}


void
GTM::gtm_thread::set_default_dispatch(GTM::abi_dispatch* disp)
{
  abi_dispatch* dd = default_dispatch.load(memory_order_relaxed);
  if (dd == disp)
    return;
  if (dd)
    {
      // If we are switching method groups, initialize and shut down properly.
      if (dd->get_method_group() != disp->get_method_group())
	{
	  dd->get_method_group()->fini();
	  disp->get_method_group()->init();
	}
    }
  else
    disp->get_method_group()->init();
  default_dispatch.store(disp, memory_order_relaxed);
}


static GTM::abi_dispatch*
parse_default_method()
{
  const char *env = getenv("ITM_DEFAULT_METHOD");
  GTM::abi_dispatch* disp = 0;
  if (env == NULL)
    return 0;

  while (isspace((unsigned char) *env))
    ++env;
  if (strncmp(env, "serialirr_onwrite", 17) == 0)
    {
      disp = GTM::dispatch_serialirr_onwrite();
      env += 17;
    }
  else if (strncmp(env, "serialirr", 9) == 0)
    {
      disp = GTM::dispatch_serialirr();
      env += 9;
    }
  else if (strncmp(env, "serial", 6) == 0)
    {
      disp = GTM::dispatch_serial();
      env += 6;
    }
  else if (strncmp(env, "gl_wt", 5) == 0)
    {
      disp = GTM::dispatch_gl_wt();
      env += 5;
    }
  else if (strncmp(env, "ml_wt", 5) == 0)
    {
      disp = GTM::dispatch_ml_wt();
      env += 5;
    }
  else if (strncmp(env, "htm", 3) == 0)
    {
      disp = GTM::dispatch_htm();
      env += 3;
    }
  else
    goto unknown;

  while (isspace((unsigned char) *env))
    ++env;
  if (*env == '\0')
    return disp;

 unknown:
  GTM::GTM_error("Unknown TM method in environment variable "
      "ITM_DEFAULT_METHOD\n");
  return 0;
}

// Gets notifications when the number of registered threads changes. This is
// used to initialize the method set choice and trigger straightforward choice
// adaption.
// This must be called only by serial threads.
void
GTM::gtm_thread::number_of_threads_changed(unsigned previous, unsigned now)
{
  if (previous == 0)
    {
      // No registered threads before, so initialize.
      static bool initialized = false;
      if (!initialized)
	{
	  initialized = true;
	  // Check for user preferences here.
	  default_dispatch = 0;
	  default_dispatch_user = parse_default_method();
	}
    }
  else if (now == 0)
    {
      // No registered threads anymore. The dispatch based on serial mode do
      // not have any global state, so this effectively shuts down properly.
      set_default_dispatch(dispatch_serialirr());
    }

  if (now == 1)
    {
      // Only one thread, so use a serializing method.
      // ??? If we don't have a fast serial mode implementation, it might be
      // better to use the global lock method set here.
      if (default_dispatch_user && default_dispatch_user->supports(now))
	set_default_dispatch(default_dispatch_user);
      else
	set_default_dispatch(dispatch_serialirr());
    }
  else if (now > 1 && previous <= 1)
    {
      // More than one thread, use the default method.
      if (default_dispatch_user && default_dispatch_user->supports(now))
	set_default_dispatch(default_dispatch_user);
      else
	{
	  // If HTM is available, use it by default with serial mode as
	  // fallback.  Otherwise, use ml_wt because it probably scales best.
	  abi_dispatch* a;
#ifdef USE_HTM_FASTPATH
	  if (htm_available())
	    a = dispatch_htm();
	  else
#endif
	    a = dispatch_ml_wt();
	  if (a->supports(now))
	    set_default_dispatch(a);
	  else
	    // Serial-irrevocable mode always works.
	    set_default_dispatch(dispatch_serialirr());
	}
    }
}
