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

/* This is a Linux > 5.16 specific implementation of a barrier synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions, and the futex and futex_waitv
   syscalls.  */

#ifndef GOMP_BARRIER_H
#define GOMP_BARRIER_H 1

#include "mutex.h"

struct __attribute__ ((aligned (64))) thread_lock_data
{
  unsigned gen;
};

typedef struct
{
  /* Make sure total/allocated and generation are in mostly read cachelines,
     awaited is in a separate cacheline and each generation structure is in a
     separate cache line.  */
  unsigned total __attribute__ ((aligned (64)));
  unsigned allocated;
  unsigned generation;
  /* spin_count to use when deciding which barrier to use for the current
     generation.  Updated once per generation by whichever thread publishes the
     generation increment to others just before that is published -- update
     determines what will be used for *next* generation.  Since only used once
     per generation know can adjust non-atomically.  It's important that this
     is the same according to every thread in a given generation so all threads
     perform the same barrier algorithm for any given generation.
     N.b. our ability to adjust non-atomically is somewhat fragile.  Most
     notably we are relying on the fact that
     `gomp_team_barrier_wait_cancel_end` and `gomp_team_barrier_wait_end` do
     not read `bar->spin_count` to avoid C data races.  */
  unsigned long long spin_count;
  /* `awaited` used for "simple" or "central" barrier.  */
  unsigned awaited __attribute__ ((aligned (64)));
  unsigned awaited_final;
  /* An *only read* cache line.  */
  struct thread_lock_data *threadgens __attribute__ ((aligned (64)));
} gomp_barrier_t;

typedef unsigned int gomp_barrier_state_t;

/* The generation field contains a counter in the high bits, with a few
   low bits dedicated to flags.  Note that TASK_PENDING and WAS_LAST can
   share space because WAS_LAST is never stored back to generation.  */
#define BAR_TASK_PENDING 1
/* In this particular target BAR_WAS_LAST indicates something more like
   "chosen by design to be last", but I like having the macro the same name as
   it is given in other targets.  */
#define BAR_WAS_LAST 1
#define BAR_WAITING_FOR_TASK 2
#define BAR_CANCELLED 4
/* Flag to indicate that primary should be released while others should be
   left.  */
#define BAR_HOLDING_SECONDARIES 8
/* Using bits 4 -> 11 for the generation number of cancellable barriers and
   remaining bits for the generation number of non-cancellable barriers.  */
#define BAR_CANCEL_INCR 16
#define BAR_INCR 4096
#define BAR_FLAGS_MASK (~(-BAR_CANCEL_INCR))
#define BAR_GEN_MASK (-BAR_INCR)
#define BAR_BOTH_GENS_MASK (-BAR_CANCEL_INCR)
#define BAR_CANCEL_GEN_MASK (-BAR_CANCEL_INCR & (~(-BAR_INCR)))
/* Increment BAR_CANCEL_INCR, with wrapping arithmetic within the bits assigned
   to this generation number.  I.e. Increment, then set bits above BAR_INCR to
   what they were before.  */
#define BAR_INCREMENT_CANCEL(X)						      \
  ({									      \
    __typeof__ (X) _X = (X);						      \
    (((_X + BAR_CANCEL_INCR) & BAR_CANCEL_GEN_MASK)			      \
     | (_X & ~BAR_CANCEL_GEN_MASK));					      \
  })

static inline void
gomp_barrier_init (gomp_barrier_t *bar, unsigned count, void *ptr)
{
  gomp_assert ((((uintptr_t) ptr) & (__alignof (struct thread_lock_data) - 1))
		 == 0,
	       "barrier extra allocation is misaligned: %p", ptr);
  bar->threadgens = (struct thread_lock_data *) ptr;
  bar->spin_count = spin_count ();
  for (unsigned i = 0; i < count; ++i)
    bar->threadgens[i].gen = 0;
  bar->total = count;
  bar->allocated = count;
  bar->awaited = count;
  bar->awaited_final = count;
  bar->generation = 0;
}

static inline struct gomp_barrier_extra_alloc_needs
gomp_barrier_extra_alloc (unsigned count)
{
  return (struct gomp_barrier_extra_alloc_needs){
      sizeof (struct thread_lock_data) * count,
      __alignof (struct thread_lock_data)};
}

/* When re-initialising a barrier we know that all threads are serialised on
   something else (because `gomp_barrier_can_hold` returns true.  However we
   still want to have memory synchronisation between each thread so still use
   atomic operations.  */
static inline void
gomp_central_barrier_reinit (gomp_barrier_t *bar, unsigned nthreads)
{
  __atomic_add_fetch (&bar->awaited, nthreads - bar->total, MEMMODEL_ACQ_REL);
  bar->total = nthreads;
}

static inline void
gomp_central_barrier_init (gomp_barrier_t *bar, unsigned count)
{
  bar->threadgens = NULL;
  bar->total = count;
  bar->allocated = 0;
  bar->awaited = count;
  bar->generation = 0;
}

static inline void
gomp_barrier_destroy (gomp_barrier_t *bar)
{}

extern void gomp_central_barrier_wait (gomp_barrier_t *);
extern void gomp_central_barrier_wait_last (gomp_barrier_t *);

extern gomp_barrier_state_t gomp_flat_barrier_wait_start (gomp_barrier_t *,
							  unsigned);
extern gomp_barrier_state_t gomp_tree_barrier_wait_start (gomp_barrier_t *,
							  unsigned);
extern gomp_barrier_state_t
gomp_central_tg_barrier_wait_start (gomp_barrier_t *, unsigned);
extern void gomp_barrier_wait_end (gomp_barrier_t *, gomp_barrier_state_t,
				   unsigned);

extern gomp_barrier_state_t gomp_team_flat_barrier_wait_start (gomp_barrier_t *,
							       unsigned);
extern gomp_barrier_state_t gomp_team_tree_barrier_wait_start (gomp_barrier_t *,
							       unsigned);
extern gomp_barrier_state_t
gomp_team_central_tg_barrier_wait_start (gomp_barrier_t *, unsigned);
extern gomp_barrier_state_t
gomp_team_central_tg_barrier_wait_final_start (gomp_barrier_t *, unsigned);
extern void gomp_team_barrier_wait_end (gomp_barrier_t *, gomp_barrier_state_t,
					unsigned);
extern void gomp_team_barrier_wait_for_tasks (gomp_barrier_t *,
					      gomp_barrier_state_t, unsigned);
extern void gomp_team_barrier_done_final (gomp_barrier_t *bar, unsigned id);
extern void gomp_team_central_tg_barrier_wait_final_end (gomp_barrier_t *bar,
							 gomp_barrier_state_t,
							 unsigned id);

extern gomp_barrier_state_t
gomp_team_flat_barrier_wait_cancel_start (gomp_barrier_t *, unsigned);
extern gomp_barrier_state_t
gomp_team_tree_barrier_wait_cancel_start (gomp_barrier_t *, unsigned);
extern gomp_barrier_state_t
gomp_team_central_tg_barrier_wait_cancel_start (gomp_barrier_t *, unsigned);

extern bool gomp_team_barrier_wait_cancel_end_impl (gomp_barrier_t *,
						    gomp_barrier_state_t,
						    unsigned);

void gomp_team_barrier_wake (gomp_barrier_t *bar, int count);
struct gomp_team;
extern void gomp_team_barrier_cancel (struct gomp_team *);

static inline enum gomp_barrier_type
gomp_barrier_kind_to_use (unsigned long long spin_count, bool is_final)
{
  enum gomp_barrier_type req
    = is_final ? gomp_forkjoin_barrier_request : gomp_barrier_request;
  /* It is important to remember that the defaults we choose here have some
     interaction with the code we use for implementation.
     As it stands, if we were to use "centralized" barrier based on spin_count
     for the final barrier then we would have to ensure that
     `gomp_barrier_can_hold` returns information describing what the
     spin_count would have determined when running the last final barrier
     (rather than the current one). That's a bit awkward.  As it stands,
     depending on workflow one could very well prefer to use GOMP_TREE_BARRIER
     as the "final" barrier in low spincount because it performs drastically
     better if there are not many tasks being performed on that final barrier.
     If there are tasks being performed then GOMP_CENTRALIZED_BARRIER performs
     best -- but that can be chosen by GOMP_BARRIER_TYPE=centralized or
     GOMP_FORKJOIN_BARRIER_TYPE=centralized anyway.  Hence this set of
     defaults both removes some complexity and allows all optimal choices of
     barrier.  */
  if (req != GOMP_BARRIER_BY_SPINCOUNT)
    return req;
  /* 300000LL is not a number that we have solid performance numbers backing.
     Want to choose FLAT barrier when we don't enter futex_wait and
     centralized/tree combination otherwise.  That is determined based on the
     users workloads.
     Using "flat" on unspecified OMP_WAIT_POLICY since I've seen some library
     workloads that benefit from this.  */
  return (spin_count >= 300000LL ? GOMP_FLAT_BARRIER
	  : is_final ? GOMP_TREE_BARRIER : GOMP_CENTRALIZED_BARRIER);
}

static inline gomp_barrier_state_t
gomp_barrier_wait_start (gomp_barrier_t *bar, unsigned id)
{
  switch (gomp_barrier_kind_to_use (bar->spin_count, false))
    {
    case GOMP_FLAT_BARRIER:
      return gomp_flat_barrier_wait_start (bar, id);
    case GOMP_TREE_BARRIER:
      return gomp_tree_barrier_wait_start (bar, id);
    case GOMP_CENTRALIZED_BARRIER:
      return gomp_central_tg_barrier_wait_start (bar, id);
    default:
      __builtin_unreachable ();
    }
}

/* Semantics are supposed to be like gomp_barrier_wait, except that if the
   encountering thread is not the last one to hit the barrier, it returns
   immediately.  The intended usage is that a thread which intends to
   gomp_barrier_destroy this barrier calls gomp_barrier_wait, while all other
   threads call gomp_barrier_wait_last.  When gomp_barrier_wait returns, the
   barrier can be safely destroyed.

   This is pretty difficult to implement with the `tree` based barrier.
   If this thread should be the co-ordinator of a sub-group of threads then it
   can't immediately exit.  Instead we wait until all threads "above" it have
   arrived.  This fulfills the requirements of knowing that when
   `gomp_barrier_wait` returns the barrier can be safely destroyed.  */
static inline void
gomp_barrier_wait_last (gomp_barrier_t *bar, unsigned id)
{
  gomp_assert (id != 0, "thread with id %u called gomp_barrier_wait_last", id);
  /* After the call to `gomp_barrier_wait_start`:
     - For the "flat" barrier all secondary threads mark themselves as having
       arrived and will immediately exit.
     - For the "tree" barrier secondary threads wait for whatever children they
       are supposed to wait on and then exit.
     - For the "central_tg" barrier, if there is a secondary thread that
       happens to be last then it will have to signal to the primary thread
       (and others) that the barrier has been reached.
     For the intended use of this function we don't need to care about being
     called by the primary thread (see assertion above), if we did need to care
     about such things then it would require a different barrier structure.  */
  gomp_barrier_state_t state = gomp_barrier_wait_start (bar, id);
  if (state & BAR_WAS_LAST)
    gomp_barrier_wait_end (bar, state, id);
}

static inline void
gomp_barrier_wait (gomp_barrier_t *bar, unsigned id)
{
  gomp_barrier_state_t state = gomp_barrier_wait_start (bar, id);
  gomp_barrier_wait_end (bar, state, id);
}

static inline void
gomp_team_barrier_wait (gomp_barrier_t *bar, unsigned id)
{
  gomp_barrier_state_t state;
  switch (gomp_barrier_kind_to_use (bar->spin_count, false))
    {
    case GOMP_FLAT_BARRIER:
      state = gomp_team_flat_barrier_wait_start (bar, id);
      break;
    case GOMP_TREE_BARRIER:
      state = gomp_team_tree_barrier_wait_start (bar, id);
      break;
    case GOMP_CENTRALIZED_BARRIER:
      state = gomp_team_central_tg_barrier_wait_start (bar, id);
      break;
    default:
      __builtin_unreachable ();
    }
  gomp_team_barrier_wait_end (bar, state, id);
}

static inline void
gomp_team_barrier_wait_final (gomp_barrier_t *bar, unsigned id)
{
  gomp_barrier_state_t state;
  enum gomp_barrier_type type
    = gomp_barrier_kind_to_use (bar->spin_count, true);
  switch (type)
    {
    case GOMP_FLAT_BARRIER:
      state = gomp_team_flat_barrier_wait_start (bar, id);
      break;
    case GOMP_TREE_BARRIER:
      state = gomp_team_tree_barrier_wait_start (bar, id);
      break;
    case GOMP_CENTRALIZED_BARRIER:
      state = gomp_team_central_tg_barrier_wait_final_start (bar, id);
      break;
    default:
      __builtin_unreachable ();
    }
  /* Revert `awaited` to account for possibility of a cancelled central barrier
     before leaving `awaited` in unknown state.  */
  if (state & BAR_WAS_LAST)
    bar->awaited = bar->total;
  /* central barrier doesn't guarantee that primary thread is last out of
     wait_start.  This means it needs the BAR_HOLDING_SECONDARIES functionality
     doesn't give much performance boost (there's extra synchronisation
     required to get the primary last and that costs about the same as the
     second barrier that we get to avoid).  */
  if (type == GOMP_CENTRALIZED_BARRIER)
    gomp_team_central_tg_barrier_wait_final_end (bar, state, id);
  else
    gomp_team_barrier_wait_for_tasks (bar, state, id);
}

static inline gomp_barrier_state_t
gomp_barrier_wait_cancel_start (gomp_barrier_t *bar, unsigned id)
{
  switch (gomp_barrier_kind_to_use (bar->spin_count, false))
    {
    case GOMP_FLAT_BARRIER:
      return gomp_team_flat_barrier_wait_cancel_start (bar, id);
    case GOMP_TREE_BARRIER:
      return gomp_team_tree_barrier_wait_cancel_start (bar, id);
    case GOMP_CENTRALIZED_BARRIER:
      return gomp_team_central_tg_barrier_wait_cancel_start (bar, id);
    default:
      __builtin_unreachable ();
    }
}

static inline bool
gomp_team_barrier_wait_cancel (gomp_barrier_t *bar, unsigned id)
{
  gomp_barrier_state_t state = gomp_barrier_wait_cancel_start (bar, id);

  if (__builtin_expect (state & BAR_CANCELLED, 0))
    return true;

  return gomp_team_barrier_wait_cancel_end_impl (bar, state, id);
}

static inline bool
gomp_team_barrier_wait_cancel_end (gomp_barrier_t *bar,
				   gomp_barrier_state_t state, unsigned id)
{
  if (state & BAR_CANCELLED)
    return true;
  return gomp_team_barrier_wait_cancel_end_impl (bar, state, id);
}

static inline bool
gomp_barrier_last_thread (gomp_barrier_state_t state)
{
  return state & BAR_WAS_LAST;
}

static inline bool
gomp_barrier_can_hold (gomp_barrier_t *bar)
{
  /* As it stands never choose CENTRALIZED for final barrier by default.  */
  return gomp_forkjoin_barrier_request == GOMP_CENTRALIZED_BARRIER ? false
								   : true;
}

static inline bool
gomp_team_barrier_wake_on_increment (unsigned increment)
{
  return increment == BAR_HOLDING_SECONDARIES ? false : true;
}

static inline void
gomp_team_barrier_set_task_pending (gomp_barrier_t *bar)
{
  __atomic_fetch_or (&bar->generation, BAR_TASK_PENDING, MEMMODEL_RELAXED);
}

static inline void
gomp_team_barrier_clear_task_pending (gomp_barrier_t *bar)
{
  __atomic_fetch_and (&bar->generation, ~BAR_TASK_PENDING, MEMMODEL_RELAXED);
}

static inline void
gomp_team_barrier_set_waiting_for_tasks (gomp_barrier_t *bar)
{
  __atomic_fetch_or (&bar->generation, BAR_WAITING_FOR_TASK, MEMMODEL_RELAXED);
}

static inline bool
gomp_team_barrier_waiting_for_tasks (gomp_barrier_t *bar)
{
  unsigned gen = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
  return (gen & BAR_WAITING_FOR_TASK) != 0;
}

static inline bool
gomp_team_barrier_cancelled (gomp_barrier_t *bar)
{
  unsigned gen = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
  return __builtin_expect ((gen & BAR_CANCELLED) != 0, 0);
}

static inline unsigned
gomp_increment_gen (gomp_barrier_state_t gen, unsigned increment)
{
  unsigned gens = (gen & BAR_BOTH_GENS_MASK);
  switch (increment)
    {
    case BAR_CANCEL_INCR:
      return BAR_INCREMENT_CANCEL (gens);
    /* Increment of `false` used when increment of barrier not possible.
       Used when the primary thread is waiting for all other threads to arrive.
       Using `false` just as something to make it clear in the code that this
       function can't increment.  This function still has to handle such an
       argument.  */
    case false:
    case BAR_INCR:
      return gens + BAR_INCR;
    case BAR_HOLDING_SECONDARIES:
      return gens | BAR_HOLDING_SECONDARIES;
    default:
      gomp_fatal ("unknown increment in gomp_increment_gen: %u\n", increment);
    }
}

static inline void
gomp_team_barrier_done (gomp_barrier_t *bar, gomp_barrier_state_t state,
			unsigned increment)
{
  /* Using MEMMODEL_RELEASE because this will "publish" the user data written
     in tasks to other threads.  */
  bar->spin_count = spin_count ();
  __atomic_store_n (&bar->generation, gomp_increment_gen (state, increment),
		    MEMMODEL_RELEASE);
}

static inline bool
gomp_barrier_incremented (gomp_barrier_state_t gen, gomp_barrier_state_t state,
			  unsigned increment)
{
  /* Remove all flags from the "original state" field.
     Logic of the function is that if/when we spill over the generation will be
     less than the original generation.  However just checking that the current
     generation is less fails if it's less because flags have been cleared.
     Hence check against a `state` with cleared flags.
     `gomp_increment_gen` already clears flags before using `state` so this
     makes no difference there.  */
  state &= BAR_BOTH_GENS_MASK;
  /* In the case of BAR_HOLDING_SECONDARIES the actual point at which all
     threads are released is an increment of BAR_INCR after the primary thread
     has continued past that temporary hold.  */
  if (increment == BAR_HOLDING_SECONDARIES)
    increment = BAR_INCR;
  unsigned next = gomp_increment_gen (state, increment);
  return __builtin_expect (next > state, 1) ? gen >= next : gen < state;
}

static inline bool
gomp_barrier_has_completed (gomp_barrier_state_t state, gomp_barrier_t *bar,
			    unsigned increment)
{
  unsigned curgen = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
  if (curgen & BAR_HOLDING_SECONDARIES)
    {
      gomp_assert (increment == BAR_HOLDING_SECONDARIES,
		   "barrier generation %u which claims holding secondaries"
		   " but increment was %u",
		   curgen, increment);
      return true;
    }
  return gomp_barrier_incremented (curgen, state, increment);
}

#endif /* GOMP_BARRIER_H */
