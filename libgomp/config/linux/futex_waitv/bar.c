/* Copyright The GNU Toolchain Authors.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
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

/* This is an implementation of a barrier synchronization mechanism for libgomp
   written for Linux versions > 5.16 (that have the `futex_waitv` syscall).
   This type is private to the library.  This implementation uses atomic
   instructions and the futex and futex_waitv syscalls.  */

#include <limits.h>
#include "wait.h"
#include "futex_waitv.h"

#ifndef GOMP_USE_ALIGNED_WORK_SHARES
#error "assume GOMP_USE_ALIGNED_WORK_SHARES when allocating barrier in team.c"
#endif

static inline gomp_barrier_state_t
gomp_central_barrier_wait_start (gomp_barrier_t *bar)
{
  unsigned ret = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
  ret &= -BAR_INCR | BAR_CANCELLED;
  /* A memory barrier is needed before exiting from the various forms
     of gomp_barrier_wait, to satisfy OpenMP API version 3.1 section
     2.8.6 flush Construct, which says there is an implicit flush during
     a barrier region.  This is a convenient place to add the barrier,
     so we use MEMMODEL_ACQ_REL here rather than MEMMODEL_ACQUIRE.  */
  if (__atomic_add_fetch (&bar->awaited, -1, MEMMODEL_ACQ_REL) == 0)
    ret |= BAR_WAS_LAST;
  return ret;
}

static inline void
gomp_central_barrier_wait_end (gomp_barrier_t *bar, gomp_barrier_state_t state)
{
  gomp_assert (!(state & BAR_CANCELLED),
	       "BAR_CANCELLED set when using plain barrier: %u", state);
  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      /* Next time we'll be awaiting TOTAL threads again.  */
      bar->awaited = bar->total;
      /* No need to atomically load bar->generation here since we know we are
	 last and since there's no tasking no-one else is writing to this
	 location.  */
      __atomic_store_n (&bar->generation, bar->generation + BAR_INCR,
			MEMMODEL_RELEASE);
      futex_wake ((int *) &bar->generation, INT_MAX);
    }
  else
    {
      do
	do_wait ((int *) &bar->generation, state);
      while (__atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE) == state);
    }
}

void
gomp_central_barrier_wait (gomp_barrier_t *bar)
{
  gomp_barrier_state_t state = gomp_central_barrier_wait_start (bar);
  gomp_central_barrier_wait_end (bar, state);
}

/* Like gomp_central_barrier_wait, except that if the encountering thread is
   not the last one to hit the barrier, it returns immediately.  The intended
   usage is that a thread which intends to gomp_barrier_destroy this barrier
   calls gomp_central_barrier_wait, while all other threads call
   gomp_central_barrier_wait_last.  When gomp_central_barrier_wait returns, the
   barrier can be safely destroyed.  */

void
gomp_central_barrier_wait_last (gomp_barrier_t *bar)
{
  gomp_barrier_state_t state = gomp_central_barrier_wait_start (bar);
  if (state & BAR_WAS_LAST)
    gomp_central_barrier_wait_end (bar, state);
}

static inline void
gomp_assert_seenflags (gomp_barrier_t *bar, bool cancellable)
{
#if _LIBGOMP_CHECKING_
  unsigned gen = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
  struct thread_lock_data *arr = bar->threadgens;
  unsigned cancel_incr = cancellable ? BAR_CANCEL_INCR : 0;
  unsigned incr = cancellable ? 0 : BAR_INCR;
  /* Assert that all threads have been seen.  */
  for (unsigned i = 0; i < bar->total; i++)
    {
      unsigned g = arr[i].gen;
      gomp_assert ((g & BAR_GEN_MASK) == (gen & BAR_GEN_MASK) + incr,
		   "index %u generation is %u (global is %u)\n", i, g, gen);
      gomp_assert ((g & BAR_CANCEL_GEN_MASK)
		     == ((gen + cancel_incr) & BAR_CANCEL_GEN_MASK),
		   "index %u cancel generation out of sync.  Gen is %u (global "
		   "is %u)\n",
		   i, g, gen);
    }

  /* Assert that generation numbers not corresponding to any thread are
     cleared.  This helps us test code-paths.  */
  for (unsigned i = bar->total; i < bar->allocated; i++)
    {
      unsigned g = arr[i].gen;
      gomp_assert (g == 0,
		   "index %u gen should be 0.  Is %u (global gen is %u)\n", i,
		   g, gen);
    }
#endif
}

static inline void
gomp_assert_and_store_flag (unsigned *addr, unsigned val, unsigned orig,
			    unsigned gens, unsigned id)
{
  __atomic_store_n (addr, val, MEMMODEL_RELEASE);
  futex_wake ((int *) addr, INT_MAX);
  gomp_assert (orig == (gens & BAR_BOTH_GENS_MASK),
	       "id %u:  Original flag %u != generation of %u\n", id, orig,
	       gens);
  gomp_assert ((orig & BAR_FLAGS_MASK) == 0,
	       "id %u:  Original flag %u has flags set\n", id, orig);
}

static inline void
gomp_assert_and_increment_flag (gomp_barrier_t *bar, unsigned id, unsigned gens)
{
  /* Atomically update our thread-local generation for the primary thread to
     observe.  We are the only thread writing to this value, so we only need an
     atomic store for publishing and do not need __atomic_fetch_add.  */
  unsigned orig = bar->threadgens[id].gen;
  unsigned new = orig + BAR_INCR;
  gomp_assert_and_store_flag (&bar->threadgens[id].gen, new, orig, gens, id);
}

static inline gomp_barrier_state_t
gomp_flat_barrier_arrive (gomp_barrier_t *bar, unsigned id)
{
  /* TODO I don't believe this MEMMODEL_ACQUIRE is needed.
     Look into it later.  Point being that this should only ever read a value
     from last barrier or from tasks/cancellation/etc.  There was already an
     acquire-release ordering at exit of the last barrier, all setting of
     tasks/cancellation etc are done with RELAXED memory model => using ACQUIRE
     doesn't help.

     See corresponding comment in `gomp_team_barrier_cancel` when thinking
     about this.

     I was pretty confident that this wasn't necessary when we *only* had a
     flat barrier, not so sure now that we also have a central barrier.
     Hence the TODO above that we have to look into this.  */
  unsigned ret = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
  ret &= (BAR_BOTH_GENS_MASK | BAR_CANCELLED);
#if !_LIBGOMP_CHECKING_
  if (id != 0)
#endif
    /* Increment local flag.  For thread id 0 this doesn't communicate
       anything to *other* threads, but it is useful for debugging purposes.  */
    gomp_assert_and_increment_flag (bar, id, ret);

  if (id == 0)
    ret |= BAR_WAS_LAST;
  return ret;
}

static void
gomp_wait_on_flag (gomp_barrier_t *bar, unsigned child_tid, unsigned tstate)
{
  unsigned tmp;
  struct thread_lock_data *arr = bar->threadgens;
  do
    {
      do_wait ((int *) &arr[child_tid].gen, tstate);
      tmp = __atomic_load_n (&arr[child_tid].gen, MEMMODEL_ACQUIRE);
      gomp_assert (tmp == tstate || tmp == (tstate + BAR_INCR),
		   "thread-local state seen to be %u"
		   " when global gens are %u",
		   tmp, tstate);
    }
  while (tmp != (tstate + BAR_INCR));
}

gomp_barrier_state_t
gomp_flat_barrier_wait_start (gomp_barrier_t *bar, unsigned id)
{
  gomp_barrier_state_t state = gomp_flat_barrier_arrive (bar, id);
  gomp_assert (!(state & BAR_CANCELLED),
	       "BAR_CANCELLED set when using plain barrier: %u", state);
  if (state & BAR_WAS_LAST)
    {
      gomp_assert (id == 0, "secondary thread %u believes is last", id);
      unsigned tstate = state & BAR_BOTH_GENS_MASK;
      for (unsigned i = 1; i < bar->total; i++)
	gomp_wait_on_flag (bar, i, tstate);
      gomp_assert_seenflags (bar, false);
    }
  return state;
}

#define hyper_iteration(ID, BAR, FINISHED, WAIT_ON_CHILD)		      \
  do									      \
    {									      \
      uint32_t num_threads = (BAR)->total;				      \
      uint32_t offset;							      \
      uint32_t level;							      \
      uint32_t branch_bits = 2;						      \
      uint32_t branch_factor = 1 << branch_bits;			      \
      for (level = 0, offset = 1; offset < num_threads;			      \
	   level += branch_bits, offset <<= branch_bits)		      \
	{								      \
	  uint32_t child;						      \
	  uint32_t child_tid;						      \
	  if ((((ID) >> level) & (branch_factor - 1)) != 0)		      \
	    {								      \
	      gomp_assert ((ID) != 0,					      \
			   "primary thread incrementing thread local gen\n"); \
	      FINISHED;							      \
	      break;							      \
	    }								      \
	  for (child = 1, child_tid = (ID) + (1 << level);		      \
	       child < branch_factor && child_tid < num_threads;	      \
	       child++, child_tid += (1 << level))			      \
	    WAIT_ON_CHILD (child_tid);					      \
	}								      \
    }									      \
  while (0)

gomp_barrier_state_t
gomp_tree_barrier_wait_start (gomp_barrier_t *bar, unsigned id)
{
  unsigned gstate = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
  gomp_assert (!(gstate & BAR_CANCELLED),
	       "BAR_CANCELLED set when using plain barrier: %u", gstate);
  gstate &= BAR_BOTH_GENS_MASK;
  unsigned tstate = gstate & BAR_BOTH_GENS_MASK;
#define wait_on_child(child_tid) gomp_wait_on_flag (bar, child_tid, tstate)
  hyper_iteration (id, bar, gomp_assert_and_increment_flag (bar, id, gstate),
		   wait_on_child);
#undef wait_on_child
  /* At this point children threads have marked their arrival to their parent
     thread but don't know that their parent thread has seen that mark.
     Parent threads know that all their children have arrived here (and that
     we've seen it).  */
  if (id == 0)
    {
#if _LIBGOMP_CHECKING_
      gomp_assert_and_increment_flag (bar, id, gstate);
#endif
      gstate |= BAR_WAS_LAST;
      gomp_assert_seenflags (bar, false);
    }
  return gstate;
}

static inline unsigned
gomp_central_tg_barrier_fetchgen (gomp_barrier_t *bar)
{
  unsigned ret = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
  ret &= BAR_BOTH_GENS_MASK | BAR_CANCELLED;
  return ret;
}

static inline unsigned
gomp_central_tg_barrier_assertions (gomp_barrier_t *bar, unsigned ret,
				    unsigned id)
{
  unsigned orig = bar->threadgens[id].gen;
  gomp_assert (orig == (ret & BAR_BOTH_GENS_MASK),
	       "id %u:  Original flag %u != generation of %u\n", id, orig, ret);
  gomp_assert ((orig & BAR_FLAGS_MASK) == 0,
	       "id %u:  Original flag %u has flags set\n", id, orig);
  return orig;
}

static inline unsigned
gomp_central_tg_barrier_decrement (gomp_barrier_t *bar, unsigned *await,
				   unsigned ret, unsigned id)
{
  /* A memory barrier is needed before exiting from the various forms
     of gomp_barrier_wait, to satisfy OpenMP API version 3.1 section
     2.8.6 flush Construct, which says there is an implicit flush during
     a barrier region.  This is a convenient place to add the barrier,
     so we use MEMMODEL_ACQ_REL here rather than MEMMODEL_ACQUIRE.  */
  if (__atomic_add_fetch (await, -1, MEMMODEL_ACQ_REL) == 0)
    {
      ret |= BAR_WAS_LAST;
      *await = bar->total;
    }
  return ret;
}

gomp_barrier_state_t
gomp_central_tg_barrier_wait_start (gomp_barrier_t *bar, unsigned id)
{
  unsigned ret = gomp_central_tg_barrier_fetchgen (bar);
  gomp_assert (!(ret & BAR_CANCELLED),
	       "BAR_CANCELLED set when using plain barrier: %u", ret);
#if !_LIBGOMP_CHECKING_
  if (id != 0)
#endif
    {
      unsigned orig = gomp_central_tg_barrier_assertions (bar, ret, id);
      /* Does not need to be atomic as it stands because there are
	 happens-before relationships between barrier epochs outside of
	 cancellable barriers, and a cancelled barrier is directly followed by
	 a `final` barrier.  However it seems more robust to later changes to
	 use an atomic operation here.  */
      __atomic_store_n (&bar->threadgens[id].gen, orig + BAR_INCR,
			MEMMODEL_RELAXED);
    }
  ret = gomp_central_tg_barrier_decrement (bar, &bar->awaited, ret, id);
  return ret;
}

void
gomp_barrier_wait_end (gomp_barrier_t *bar, gomp_barrier_state_t state,
		       unsigned id)
{
  gomp_assert (!(state & BAR_CANCELLED),
	       "BAR_CANCELLED set when using plain barrier: %u", state);
  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      /* Shouldn't have anything modifying bar->generation at this point since
	 we have waited for all other threads to enter this function and
	 they're all in the `do_wait` loop below.  */
      gomp_assert ((bar->generation & ~BAR_BOTH_GENS_MASK) == 0,
		   "flags set in gomp_barrier_wait_end: %u",
		   bar->generation & ~BAR_BOTH_GENS_MASK);
      bar->spin_count = spin_count ();
      __atomic_store_n (&bar->generation, bar->generation + BAR_INCR,
			MEMMODEL_RELEASE);
      futex_wake ((int *) &bar->generation, INT_MAX);
    }
  else
    {
      do
	do_wait ((int *) &bar->generation, state);
      while (__atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE) == state);
    }
}

/* We need to have some "ensure we're last, but perform useful work
   in the meantime" behaviour.  This allows the primary thread to perform
   useful work while it is waiting on all secondary threads.
   - Related to the fundamental difference between this barrier and the
     "central" barrier where the thread doing the bookkeeping is pre-determined
     as some "primary" thread and that is not necessarily the last thread to
     enter the barrier.

   To do that we loop through checking each of the other threads flags.  If any
   are not set then we take that opportunity to check the global generation
   number, if there's some task to handle then do so before going back to
   checking the remaining thread-local flags.

   This approach means that the cancellable barriers work reasonably naturally.
   If we're checking the global generation flag then we can see when it is
   cancelled.  Hence `gomp_team_barrier_cancel_wait_on_two_flags` below does
   something very similar to this function here.

   When there is nothing for a thread to do we want to wait on one of two
   addresses being changed (either the secondary threads control flag or the
   barrier main generation).  The futex_waitv syscall provides exactly that
   functionality.  */
static inline void
gomp_team_barrier_wait_on_two_flags (gomp_barrier_t *bar, unsigned child_tid,
				     unsigned long long spincount,
				     unsigned gstate, unsigned tstate)
{
  /* Thought process here:
     - Until we are the last thread, we are "some thread waiting on the
       barrier".  Hence we should doing only a variant of what is done in
       `gomp_team_barrier_wait_end` for the other threads.
     - The loop done in that function is:
       1) Wait on `generation` having some flag.
       2) When it changes, enforce the acquire-release semantics and
	  re-load.
       3) If added `BAR_TASK_PENDING`, then handle some tasks.
       4) Ignore `BAR_WAITING_FOR_TASK` flag.
     - Hence the loop done in this function is:
       1) Wait on `child_tid` generation number.
	  - When this is not incremented, then:
	    1. wait on `generation` having some flag.
	    2. When it does have such a flag, enforce the acquire-release
	       semantics and load.
	    3. If added `BAR_TASK_PENDING` then handle some tasks.
	    4. Ignore `BAR_WAITING_FOR_TASK` not necessary because it's us that
	       would set that flag.
       Differences otherwise are that we put pausing in different positions.  */
  unsigned long long j;
  unsigned threadgen_orig;
  struct thread_lock_data *arr = bar->threadgens;
restart_loop:
  /* Use `j <= spincount` here in case `gomp_spin_count_var == 0` (which can
     happen with OMP_WAIT_POLICY=passive).  We need to go around this loop at
     least once to check and handle any changes.  */
  for (j = 0; j <= spincount; j++)
    {
      /* Thought about using MEMMODEL_ACQUIRE or MEMMODEL_RELAXED until we
	 see a difference and *then* MEMMODEL_ACQUIRE for the
	 acquire-release semantics.  Idea was that the more relaxed memory
	 model might provide a performance boost.  Did not see any
	 improvement on a micro-benchmark and decided not to do that.

	 TODO Question whether to run one loop checking both variables, or
	      two loops each checking one variable multiple times.  Suspect
	      that one loop checking both variables is going to be more
	      responsive and more understandable in terms of performance
	      characteristics when specifying GOMP_SPINCOUNT.
	      However there's the chance that checking each variable
	      something like 10000 times and going around this loop
	      count/10000 times could give better throughput?  */
      threadgen_orig = __atomic_load_n (&arr[child_tid].gen, MEMMODEL_ACQUIRE);
      unsigned threadgen = threadgen_orig & BAR_GEN_MASK;
      gomp_assert ((threadgen_orig & BAR_FLAGS_MASK) == 0,
		   "id %u: thread local generation has non-zero flags: "
		   "%u\n",
		   child_tid, threadgen_orig);
      if (__builtin_expect (threadgen != tstate, 0))
	{
	  gomp_assert (threadgen == (tstate + BAR_INCR),
		       "thread-local state seen to be %u"
		       " when global state was %u.\n",
		       threadgen, tstate);
	  /* This is the only place we return from this function.  */
	  return;
	}
      unsigned gen = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
      if (__builtin_expect (gen != gstate, 0))
	{
	  if (gen & BAR_TASK_PENDING)
	    gomp_barrier_handle_tasks (gstate, bar, false);
	  /* When running as a logical "final" barrier the `CANCELLED` flag
	     can get set on the barrier if we were in a cancellable region
	     whose only barrier was the implicit "final" barrier at the end
	     of the region.  Ensure we update `gstate` in this case to
	     avoid any unnecessary busy-waiting when nothing is actually
	     changing.  */
	  gomp_assert (!(gstate & BAR_CANCELLED) || (gen & BAR_CANCELLED),
		       "BAR_CANCELLED state cleared by non-primary thread"
		       " bar->generation: %u   state: %u",
		       gen, gstate);
	  gstate |= gen & BAR_CANCELLED;

	  gomp_assert (!(gen & BAR_WAITING_FOR_TASK),
		       "BAR_WAITING_FOR_TASK set by non-primary thread "
		       "gen=%d",
		       gen);
	  gomp_assert (!gomp_barrier_incremented (gen, gstate, BAR_INCR),
		       "global state incremented by non-primary thread "
		       "gen=%d",
		       gen);
	  goto restart_loop;
	}
    }
  futex_waitv (&arr[child_tid].gen, threadgen_orig, &bar->generation, gstate);
  /* One of the above values has changed, restart this loop and we can find out
     what it was and deal with it accordingly.  */
  goto restart_loop;
}

gomp_barrier_state_t
gomp_team_flat_barrier_wait_start (gomp_barrier_t *bar, unsigned id)
{
  gomp_barrier_state_t state = gomp_flat_barrier_arrive (bar, id);
  if (state & BAR_WAS_LAST)
    {
      gomp_assert (id == 0, "secondary thread %u believes is last", id);
      unsigned gstate = state & (BAR_BOTH_GENS_MASK | BAR_CANCELLED);
      unsigned tstate = state & BAR_GEN_MASK;
      unsigned long long spincount = spin_count ();
      for (unsigned i = 1; i < bar->total; i++)
	gomp_team_barrier_wait_on_two_flags (bar, i, spincount, gstate, tstate);
      gomp_assert_seenflags (bar, false);
    }
  return state;
}

gomp_barrier_state_t
gomp_team_tree_barrier_wait_start (gomp_barrier_t *bar, unsigned id)
{
  unsigned gstate = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
  gstate &= (BAR_BOTH_GENS_MASK | BAR_CANCELLED);
  unsigned tstate = gstate & BAR_GEN_MASK;
  unsigned long long spincount = spin_count ();
#define wait_on_child(child_tid)					       \
  gomp_team_barrier_wait_on_two_flags (bar, child_tid, spincount, gstate,      \
				       tstate)
  hyper_iteration (id, bar, gomp_assert_and_increment_flag (bar, id, gstate),
		   wait_on_child);
#undef wait_on_child
  if (id == 0)
    {
#if _LIBGOMP_CHECKING_
      gomp_assert_and_increment_flag (bar, id, gstate);
#endif
      gstate |= BAR_WAS_LAST;
      gomp_assert_seenflags (bar, false);
    }

  return gstate;
}

gomp_barrier_state_t
gomp_team_central_tg_barrier_wait_start (gomp_barrier_t *bar, unsigned id)
{
  return gomp_central_tg_barrier_wait_start (bar, id);
}

gomp_barrier_state_t
gomp_team_central_tg_barrier_wait_final_start (gomp_barrier_t *bar, unsigned id)
{
  unsigned ret = gomp_central_tg_barrier_fetchgen (bar);
#if !_LIBGOMP_CHECKING_
  if (id != 0)
#endif
    {
      unsigned orig = gomp_central_tg_barrier_assertions (bar, ret, id);
      /* If the previous barrier was a tree/flat barrier that was cancelled,
	 then it may not have introduced a happens-before relationship between
	 the local threadgen load and this write.  That is the only reason we
	 need this write to be atomic.  */
      __atomic_store_n (&bar->threadgens[id].gen, orig + BAR_INCR,
			MEMMODEL_RELAXED);
    }
  ret = gomp_central_tg_barrier_decrement (bar, &bar->awaited_final, ret, id);
  return ret;
}

static inline bool
gomp_team_barrier_primary_check_for_tasks (gomp_barrier_t *bar,
					   unsigned increment,
					   gomp_barrier_state_t state,
					   enum memmodel m)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  team->work_share_cancelled = 0;
  unsigned task_count = __atomic_load_n (&team->task_count, MEMMODEL_ACQUIRE);
  if (__builtin_expect (task_count, 0))
    {
      gomp_barrier_handle_tasks (state, bar, increment);
      return true;
    }
  else
    {
      unsigned gens = state & ~BAR_WAS_LAST;
      gens = gomp_increment_gen (gens, increment);
      bar->spin_count = spin_count ();
      __atomic_store_n (&bar->generation, gens, m);
      /* Secondary threads remain held until
	 gomp_team_barrier_done_final.  Avoid waking them merely to observe
	 BAR_HOLDING_SECONDARIES and go back to sleep.  At high counts of
	 threads that are actually waiting, futex_wake is high cost.  */
      if (increment != BAR_HOLDING_SECONDARIES)
	futex_wake ((int *) &bar->generation, INT_MAX);
      return false;
    }
}

void
gomp_team_barrier_wait_end (gomp_barrier_t *bar, gomp_barrier_state_t state,
			    unsigned id)
{
  unsigned generation, gen;
  gomp_assert (!(state & BAR_CANCELLED),
	       "generation number includes BAR_CANCELLED in "
	       "gomp_team_barrier_wait_end: %u , id: %u",
	       state, id);

  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      if (!gomp_team_barrier_primary_check_for_tasks (bar, BAR_INCR, state,
						      MEMMODEL_RELEASE))
	return;
      state &= ~BAR_WAS_LAST;
    }

  generation = state;
  do
    {
      do_wait ((int *) &bar->generation, generation);
      gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
      gomp_assert (!(gen & BAR_CANCELLED),
		   "BAR_CANCELLED set on generation in team barrier"
		   " gen: %u  generation: %u  id: %u",
		   gen, generation, id);
      if (__builtin_expect (gen & BAR_TASK_PENDING, 0))
	{
	  gomp_barrier_handle_tasks (state, bar, BAR_INCR);
	  gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
	}
      /* These flags will not change until this barrier is completed.
	 Going forward we don't want to be continually waking up checking for
	 whether this barrier has completed yet.  */
      generation |= gen & BAR_WAITING_FOR_TASK;
      /* There should be no other flags set in bar->generation outside of the
	 ones we explicitly handle here.  */
    }
  while ((gen & BAR_BOTH_GENS_MASK) != state + BAR_INCR);
}

void
gomp_team_barrier_wait_for_tasks (gomp_barrier_t *bar,
				  gomp_barrier_state_t state, unsigned id)
{
  unsigned generation, gen;
  bool have_cleared __attribute__ ((unused)) = false;

  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      /* Acquire-release ordering from primary thread to secondary threads
	 is formed in `gomp_team_barrier_done_final` rather than this function.
	 That is where all secondary threads are allowed to run.
	 However, we do still need to perform an atomic store as-per C "data
	 race" requirements because there are other threads waiting in
	 `do_spin` that are repeatedly watching this variable.  */
      if (!gomp_team_barrier_primary_check_for_tasks (bar,
						      BAR_HOLDING_SECONDARIES,
						      state, MEMMODEL_RELAXED))
	return;
      state &= ~BAR_WAS_LAST;
    }

  generation = state;
  state &= ~BAR_CANCELLED;
  do
    {
      /* If the primary thread ends up in this loop then the benefit of
	 avoiding the wake of all threads (as per
	 `gomp_team_barrier_primary_check_for_tasks` does above) is largely
	 lost in the cost of tasking etc.  There is a possible protocol around
	 having a `primary_wake`, using `futex_waitv`, and if a secondary
	 thread wakes us up from `gomp_barrier_handle_tasks` then only wake the
	 primary up via that variable.  EPCC microbenchmarks didn't show a
	 significant improvement and it's quite a complex protocol so we don't
	 do that.  */
      do_wait ((int *) &bar->generation, generation);
      gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
      gomp_assert ((gen & BAR_CANCELLED) == (generation & BAR_CANCELLED)
		     /* Cancellation of *next* barrier is fine -- we will
			exit.  */
		     || gomp_barrier_incremented (gen, generation,
						  BAR_HOLDING_SECONDARIES)
		     /* Removing cancellation flag once all threads reached
			"final" barrier is also fine.  */
		     || ((gen & BAR_HOLDING_SECONDARIES)
			 && !(generation & BAR_HOLDING_SECONDARIES))
		     /* Can cancel a barrier when already gotten into final
			implicit barrier at the end of a parallel loop.
			This happens in `cancel-parallel-3.c`.
			In this case the above assertion does not hold because
			We are waiting on the implicit barrier at the end of a
			parallel region while some other thread is performing
			work in that parallel region, hits a
			`#pragma omp cancel parallel`, and sets said flag.  */
		     || !(generation & BAR_CANCELLED),
		   "unnecessary looping due to BAR_CANCELLED diff"
		   " gen: %u  state: %u  generation: %u  id: %u",
		   gen, state, generation, id);
      gomp_assert (!gomp_barrier_incremented (gen, state,
					      BAR_HOLDING_SECONDARIES)
		   || !gomp_barrier_incremented (gen,
						 gomp_increment_gen (state,
								     BAR_INCR),
						 BAR_HOLDING_SECONDARIES),
		   "generation has gone ahead of us two times!"
		   " gen = %u, state = %u, generation = %u",
		   gen, state, generation);
      if (__builtin_expect (gen & BAR_TASK_PENDING, 0))
	{
	  gomp_barrier_handle_tasks (state, bar, BAR_HOLDING_SECONDARIES);
	  gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
	}
#if _LIBGOMP_CHECKING_
      if (have_cleared)
	gomp_assert ((!(gen & BAR_WAITING_FOR_TASK)
		      && !(generation & BAR_WAITING_FOR_TASK)),
		     "invalid state in "
		     "gomp_team_barrier_primary_wait_for_tasks");
      else if ((generation & BAR_WAITING_FOR_TASK)
	       && !(gen & BAR_WAITING_FOR_TASK))
	have_cleared = true;
#endif
      generation
	= (generation & ~BAR_WAITING_FOR_TASK) | (gen & BAR_WAITING_FOR_TASK);
      generation = (generation & ~BAR_CANCELLED) | (gen & BAR_CANCELLED);
      /* Situation where we know all tasks are finished, and we were called
	 with instructions to wait for all tasks to finish before letting the
	 primary thread exit and holding secondary threads ready to be woken
	 up.  */
      if (id == 0 && gen & BAR_HOLDING_SECONDARIES)
	return;
      generation |= gen & BAR_HOLDING_SECONDARIES;
    }
  while ((gen & BAR_BOTH_GENS_MASK) != state + BAR_INCR);
}

void
gomp_team_barrier_done_final (gomp_barrier_t *bar, unsigned id)
{
  if (gomp_forkjoin_barrier_request == GOMP_CENTRALIZED_BARRIER)
    return;
  gomp_assert (id == 0, "called with ID = %u", id);
  unsigned gen = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
  gomp_assert ((gen & BAR_FLAGS_MASK) == BAR_HOLDING_SECONDARIES,
	       "gomp_team_barrier_done_final called with generation: %u", gen);
  gen &= ~BAR_HOLDING_SECONDARIES;
  gen += BAR_INCR;
  bar->spin_count = spin_count ();
  __atomic_store_n (&bar->generation, gen, MEMMODEL_RELEASE);
  futex_wake ((int *) &bar->generation, INT_MAX);
}

void
gomp_team_central_tg_barrier_wait_final_end (gomp_barrier_t *bar,
					     gomp_barrier_state_t state,
					     unsigned id)
{
  /* Same as gomp_team_barrier_wait_end, except that we account for
     BAR_CANCELLED being set on this barrier.
     - Different set of assertions
     - Clear BAR_CANCELLED from `state` before iterating on tasks.
     - Ensure we don't spin unnecessarily in that case.  */
  unsigned generation, gen;
  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      if (!gomp_team_barrier_primary_check_for_tasks (bar, BAR_INCR, state,
						      MEMMODEL_RELEASE))
	return;
      state &= ~BAR_WAS_LAST;
    }
  generation = state;
  state &= ~BAR_CANCELLED;
  do
    {
      do_wait ((int *) &bar->generation, generation);
      gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
      gomp_assert ((gen & BAR_CANCELLED) == (generation & BAR_CANCELLED)
		     /* Removing BAR_CANCELLED via incrementing generation is
			OK.  */
		     || gomp_barrier_incremented (gen, generation, BAR_INCR)
		     || !(generation & BAR_CANCELLED),
		   "BAR_CANCELLED removed from central final barrier %u -> "
		   "%u, Id: %u",
		   generation, gen, id);
      if (__builtin_expect (gen & BAR_TASK_PENDING, 0))
	{
	  /* We are incrementing by BAR_INCR rather than BAR_HOLDING_SECONDARIES
	     since central barrier can not guarantee primary is last and hence
	     can't implemenet "holding" barrier without extra synchronisation
	     (which loses most of the performance benefits of the "holding"
	     approach anyway).  */
	  gomp_barrier_handle_tasks (state, bar, BAR_INCR);
	  gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
	}
      /* These flags will not change until this barrier is completed.
	 Going forward we don't want to be continually waking up checking for
	 whether this barrier has completed yet.  */
      generation |= gen & BAR_WAITING_FOR_TASK;
      generation |= gen & BAR_CANCELLED;
      /* There should be no other flags set in bar->generation outside of the
	 ones we explicitly handle here.  */
    }
  while ((gen & BAR_BOTH_GENS_MASK) != state + BAR_INCR);
}

void
gomp_assert_and_increment_cancel_flag (gomp_barrier_t *bar, unsigned id,
				       unsigned gens)
{
  unsigned orig = bar->threadgens[id].gen;
  unsigned new = BAR_INCREMENT_CANCEL (orig);
  gomp_assert_and_store_flag (&bar->threadgens[id].gen, new, orig, gens, id);
}

static inline bool
gomp_team_barrier_cancel_wait_on_two_flags (gomp_barrier_t *bar,
					    unsigned child_tid,
					    unsigned long long spincount,
					    unsigned gstate, unsigned tstate)
{
  unsigned long long j;
  unsigned threadgen_orig;
  struct thread_lock_data *arr = bar->threadgens;
restart_loop:
  for (j = 0; j <= spincount; j++)
    {
      threadgen_orig = __atomic_load_n (&arr[child_tid].gen, MEMMODEL_ACQUIRE);
      unsigned threadgen = threadgen_orig & BAR_CANCEL_GEN_MASK;
      gomp_assert ((threadgen_orig & BAR_FLAGS_MASK) == 0,
		   "id %u: thread local generation has non-zero flags: "
		   "%u\n",
		   child_tid, threadgen_orig);
      if (__builtin_expect (threadgen != tstate, 0))
	{
	  gomp_assert (threadgen == BAR_INCREMENT_CANCEL (tstate),
		       "thread-local state seen to be %u"
		       " when global state was %u.\n",
		       threadgen, tstate);
	  return true;
	}
      unsigned gen = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
      if (__builtin_expect (gen != gstate, 0))
	{
	  gomp_assert (!(gen & BAR_WAITING_FOR_TASK),
		       "BAR_WAITING_FOR_TASK set in non-primary thread "
		       "gen=%d",
		       gen);
	  gomp_assert (!gomp_barrier_incremented (gen, gstate, BAR_CANCEL_INCR),
		       "global state incremented by non-primary thread "
		       "gstate=%d gen=%d",
		       gstate, gen);
	  if (gen & BAR_CANCELLED)
	    return false;

	  if (gen & BAR_TASK_PENDING)
	    gomp_barrier_handle_tasks (gstate, bar, false);
	  goto restart_loop;
	}
    }
  futex_waitv (&arr[child_tid].gen, threadgen_orig, &bar->generation, gstate);
  goto restart_loop;
}

gomp_barrier_state_t
gomp_team_flat_barrier_wait_cancel_start (gomp_barrier_t *bar, unsigned id)
{
  unsigned ret = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
  ret &= BAR_BOTH_GENS_MASK | BAR_CANCELLED;
  if (ret & BAR_CANCELLED)
    /* Do not return BAR_WAS_LAST because this thread is not last.
       The barrier is cancelled.  Some thread did not enter it.  */
    return ret;
  if (id == 0)
    {
      unsigned gstate = ret & BAR_BOTH_GENS_MASK;
      unsigned tstate = ret & BAR_CANCEL_GEN_MASK;
      unsigned long long spincount = spin_count ();
      for (unsigned i = 1; i < bar->total; i++)
	if (!gomp_team_barrier_cancel_wait_on_two_flags (bar, i, spincount,
							 gstate, tstate))
	  return (ret | BAR_CANCELLED);
      ret |= BAR_WAS_LAST;
#if _LIBGOMP_CHECKING_
      gomp_assert_and_increment_cancel_flag (bar, id, ret);
#endif
      gomp_assert_seenflags (bar, true);
    }
  else
    gomp_assert_and_increment_cancel_flag (bar, id, ret);
  return ret;
}

gomp_barrier_state_t
gomp_team_tree_barrier_wait_cancel_start (gomp_barrier_t *bar, unsigned id)
{
  unsigned gstate = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
  gstate &= (BAR_BOTH_GENS_MASK | BAR_CANCELLED);
  if (gstate & BAR_CANCELLED)
    /* Do not return BAR_WAS_LAST because this thread is not last.
       The barrier is cancelled.  Some thread did not enter it.  */
    return gstate;
  unsigned tstate = gstate & BAR_CANCEL_GEN_MASK;
  unsigned long long spincount = spin_count ();
#define wait_on_child(child_tid)					      \
  if (!gomp_team_barrier_cancel_wait_on_two_flags (bar, child_tid, spincount, \
						   gstate, tstate))	      \
    return (gstate | BAR_CANCELLED);
  hyper_iteration (id, bar,
		   gomp_assert_and_increment_cancel_flag (bar, id, gstate),
		   wait_on_child);
#undef wait_on_child
  if (id == 0)
    {
#if _LIBGOMP_CHECKING_
      gomp_assert_and_increment_cancel_flag (bar, id, gstate);
#endif
      gstate |= BAR_WAS_LAST;
      gomp_assert_seenflags (bar, true);
    }
  return gstate;
}
#undef hyper_iteration

gomp_barrier_state_t
gomp_team_central_tg_barrier_wait_cancel_start (gomp_barrier_t *bar,
						unsigned id)
{
  unsigned ret = gomp_central_tg_barrier_fetchgen (bar);
  if (ret & BAR_CANCELLED)
    return ret;
#if !_LIBGOMP_CHECKING_
  if (id != 0)
#endif
    {
      unsigned orig = gomp_central_tg_barrier_assertions (bar, ret, id);
      /* The increment of this thread-local generation number can race with a
	 read in the primary thread if that primary thread races ahead of us
	 without a sync point when the barrier gets cancelled and there is a
	 non-central _final barrier directly after us.  */
      __atomic_store_n (&bar->threadgens[id].gen, BAR_INCREMENT_CANCEL (orig),
			MEMMODEL_RELAXED);
    }
  ret = gomp_central_tg_barrier_decrement (bar, &bar->awaited, ret, id);
  return ret;
}

bool
gomp_team_barrier_wait_cancel_end_impl (gomp_barrier_t *bar,
					gomp_barrier_state_t state, unsigned id)
{
  unsigned generation, gen;
  gomp_assert (!(state & BAR_CANCELLED),
	       "gomp_team_barrier_wait_cancel_end_impl called when barrier "
	       "cancelled state: %u",
	       state);

  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      /* If primary thread got here, barrier was not cancelled (barrier can not
	 be cancelled from a task because not closely nested in parallel
	 region).  Hence if below function increments generation due to no
	 tasks remaining we don't have to worry about any other threads having
	 exited barrier due to cancellation beforehand.  */
      if (!gomp_team_barrier_primary_check_for_tasks (bar, BAR_CANCEL_INCR,
						      state, MEMMODEL_RELEASE))
	return false;
      state &= ~BAR_WAS_LAST;
    }

  generation = state;
  do
    {
      do_wait ((int *) &bar->generation, generation);
      gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
      if (__builtin_expect (gen & BAR_CANCELLED, 0))
	{
	  if (__builtin_expect ((gen & BAR_CANCEL_GEN_MASK)
				  != (state & BAR_CANCEL_GEN_MASK),
				0))
	    {
	      /* Have cancelled a barrier just after completing the current
		 one.  We must not reset our local state.  */
	      gomp_assert ((gen & BAR_CANCEL_GEN_MASK)
			     == (BAR_INCREMENT_CANCEL (state)
				 & BAR_CANCEL_GEN_MASK),
			   "incremented global generation (cancellable) more "
			   "than one gen: %u  original state: %u",
			   gen, state);
	      /* Other threads could have continued on in between the time that
		 the primary thread signalled all other threads should wake up
		 and the time that we actually read `bar->generation` above.
		 Any one of them could be performing tasks so we can't assert
		 that the corresponding flag is clear.

		 W.r.t. the task handling that could have been set (primary
		 thread increments generation telling us to go, then it
		 continues itself and finds an `omp task` directive and
		 schedules a task) we need to move on with it (PR122314).  */
	      gomp_assert (!(gen & BAR_WAITING_FOR_TASK),
			   "generation incremented while  main thread is "
			   "still waiting for tasks: gen: %u  original state:"
			   " %u",
			   gen, state);
	      /* *This* barrier wasn't cancelled -- the next barrier is
		 cancelled.  Returning `false` gives that information back up
		 to calling functions.  */
	      return false;
	    }
	  /* Need to reset our thread-local generation.  Don't want state to be
	     messed up the next time we hit a cancellable barrier.

	     We are helped by the invariant that when this barrier is cancelled,
	     the next barrier that will be entered is a non-cancellable barrier.
	     That means we don't have to worry about the primary thread getting
	     confused by this generation being incremented to say it's reached a
	     cancellable barrier.  Since we are the only thread that will modify
	     our own control word we don't need to worry about non-atomic RMW.
	   */
	  unsigned *cgen = &bar->threadgens[id].gen;
	  unsigned orig = *cgen;
	  unsigned new = ((orig - BAR_CANCEL_INCR) & BAR_CANCEL_GEN_MASK)
			 | (orig & ~BAR_CANCEL_GEN_MASK);
	  __atomic_store_n (cgen, new, MEMMODEL_RELAXED);
#if _LIBGOMP_CHECKING_
	  unsigned global_gen = gen & BAR_BOTH_GENS_MASK;
	  gomp_assert (new == global_gen,
		       "thread-local generation %u unknown modification:"
		       " expected %u seen %u",
		       id, BAR_INCREMENT_CANCEL (global_gen), orig);
#endif
	  return true;
	}
      if (__builtin_expect (gen & BAR_TASK_PENDING, 0))
	{
	  gomp_barrier_handle_tasks (state, bar, BAR_CANCEL_INCR);
	  gen = __atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE);
	}
      generation |= gen & BAR_WAITING_FOR_TASK;
    }
  while ((gen & BAR_CANCEL_GEN_MASK)
	 != (BAR_INCREMENT_CANCEL (state) & BAR_CANCEL_GEN_MASK));

  return false;
}

void
gomp_team_barrier_wake (gomp_barrier_t *bar, int count)
{
  futex_wake ((int *) &bar->generation, count == 0 ? INT_MAX : count);
}

void
gomp_team_barrier_cancel (struct gomp_team *team)
{
  /* Always set CANCEL on the barrier.  This needs to be atomic because
     barrier wait paths use `generation` to communicate without holding
     team->task_lock.  Don't need any memory-ordering here since the only thing
     that BAR_CANCELLED indicates is that the barrier is cancelled (no thread
     infers anything else about data being available based on this flag).  */
  unsigned orig = __atomic_fetch_or (&team->barrier.generation, BAR_CANCELLED,
				     MEMMODEL_RELAXED);
  if (!(orig & BAR_CANCELLED))
    futex_wake ((int *) &team->barrier.generation, INT_MAX);
}
