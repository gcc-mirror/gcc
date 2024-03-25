/* Copyright (C) 2005-2024 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* This file handles the ORDERED construct.  */

#include "libgomp.h"
#include <stdarg.h>
#include <string.h>
#include "doacross.h"


/* This function is called when first allocating an iteration block.  That
   is, the thread is not currently on the queue.  The work-share lock must
   be held on entry.  */

void
gomp_ordered_first (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned index;

  /* Work share constructs can be orphaned.  */
  if (team == NULL || team->nthreads == 1)
    return;

  index = ws->ordered_cur + ws->ordered_num_used;
  if (index >= team->nthreads)
    index -= team->nthreads;
  ws->ordered_team_ids[index] = thr->ts.team_id;

  /* If this is the first and only thread in the queue, then there is
     no one to release us when we get to our ordered section.  Post to
     our own release queue now so that we won't block later.  */
  if (ws->ordered_num_used++ == 0)
    gomp_sem_post (team->ordered_release[thr->ts.team_id]);
}

/* This function is called when completing the last iteration block.  That
   is, there are no more iterations to perform and so the thread should be
   removed from the queue entirely.  Because of the way ORDERED blocks are
   managed, it follows that we currently own access to the ORDERED block,
   and should now pass it on to the next thread.  The work-share lock must
   be held on entry.  */

void
gomp_ordered_last (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned next_id;

  /* Work share constructs can be orphaned.  */
  if (team == NULL || team->nthreads == 1)
    return;

  /* We're no longer the owner.  */
  ws->ordered_owner = -1;

  /* If we're not the last thread in the queue, then wake the next.  */
  if (--ws->ordered_num_used > 0)
    {
      unsigned next = ws->ordered_cur + 1;
      if (next == team->nthreads)
	next = 0;
      ws->ordered_cur = next;

      next_id = ws->ordered_team_ids[next];
      gomp_sem_post (team->ordered_release[next_id]);
    }
}


/* This function is called when allocating a subsequent allocation block.
   That is, we're done with the current iteration block and we're allocating
   another.  This is the logical combination of a call to gomp_ordered_last
   followed by a call to gomp_ordered_first.  The work-share lock must be
   held on entry. */

void
gomp_ordered_next (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned index, next_id;

  /* Work share constructs can be orphaned.  */
  if (team == NULL || team->nthreads == 1)
    return;

  /* We're no longer the owner.  */
  ws->ordered_owner = -1;

  /* If there's only one thread in the queue, that must be us.  */
  if (ws->ordered_num_used == 1)
    {
      /* We have a similar situation as in gomp_ordered_first
	 where we need to post to our own release semaphore.  */
      gomp_sem_post (team->ordered_release[thr->ts.team_id]);
      return;
    }

  /* If the queue is entirely full, then we move ourself to the end of 
     the queue merely by incrementing ordered_cur.  Only if it's not 
     full do we have to write our id.  */
  if (ws->ordered_num_used < team->nthreads)
    {
      index = ws->ordered_cur + ws->ordered_num_used;
      if (index >= team->nthreads)
	index -= team->nthreads;
      ws->ordered_team_ids[index] = thr->ts.team_id;
    }

  index = ws->ordered_cur + 1;
  if (index == team->nthreads)
    index = 0;
  ws->ordered_cur = index;

  next_id = ws->ordered_team_ids[index];
  gomp_sem_post (team->ordered_release[next_id]);
}


/* This function is called when a statically scheduled loop is first
   being created.  */

void
gomp_ordered_static_init (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

  if (team == NULL || team->nthreads == 1)
    return;

  gomp_sem_post (team->ordered_release[0]);
}

/* This function is called when a statically scheduled loop is moving to
   the next allocation block.  Static schedules are not first come first
   served like the others, so we're to move to the numerically next thread,
   not the next thread on a list.  The work-share lock should *not* be held
   on entry.  */

void
gomp_ordered_static_next (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned id = thr->ts.team_id;

  if (team == NULL || team->nthreads == 1)
    return;

  ws->ordered_owner = -1;

  /* This thread currently owns the lock.  Increment the owner.  */
  if (++id == team->nthreads)
    id = 0;
  ws->ordered_team_ids[0] = id;
  gomp_sem_post (team->ordered_release[id]);
}

/* This function is called when we need to assert that the thread owns the
   ordered section.  Due to the problem of posted-but-not-waited semaphores,
   this needs to happen before completing a loop iteration.  */

void
gomp_ordered_sync (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;

  /* Work share constructs can be orphaned.  But this clearly means that
     we are the only thread, and so we automatically own the section.  */
  if (team == NULL || team->nthreads == 1)
    return;

  /* ??? I believe it to be safe to access this data without taking the
     ws->lock.  The only presumed race condition is with the previous
     thread on the queue incrementing ordered_cur such that it points
     to us, concurrently with our check below.  But our team_id is
     already present in the queue, and the other thread will always
     post to our release semaphore.  So the two cases are that we will
     either win the race an momentarily block on the semaphore, or lose
     the race and find the semaphore already unlocked and so not block.
     Either way we get correct results.
     However, there is an implicit flush on entry to an ordered region,
     so we do need to have a barrier here.  If we were taking a lock
     this could be MEMMODEL_RELEASE since the acquire would be covered
     by the lock.  */

  __atomic_thread_fence (MEMMODEL_ACQ_REL);
  if (ws->ordered_owner != thr->ts.team_id)
    {
      gomp_sem_wait (team->ordered_release[thr->ts.team_id]);
      ws->ordered_owner = thr->ts.team_id;
    }
}

/* This function is called by user code when encountering the start of an
   ORDERED block.  We must check to see if the current thread is at the
   head of the queue, and if not, block.  */

#ifdef HAVE_ATTRIBUTE_ALIAS
extern void GOMP_ordered_start (void)
	__attribute__((alias ("gomp_ordered_sync")));
#else
void
GOMP_ordered_start (void)
{
  gomp_ordered_sync ();
}
#endif

/* This function is called by user code when encountering the end of an
   ORDERED block.  With the current ORDERED implementation there's nothing
   for us to do.

   However, the current implementation has a flaw in that it does not allow
   the next thread into the ORDERED section immediately after the current
   thread exits the ORDERED section in its last iteration.  The existence
   of this function allows the implementation to change.  */

void
GOMP_ordered_end (void)
{
}

/* DOACROSS initialization.  */

#define MAX_COLLAPSED_BITS (__SIZEOF_LONG__ * __CHAR_BIT__)

void
gomp_doacross_init (unsigned ncounts, long *counts, long chunk_size,
		    size_t extra)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned int i, bits[MAX_COLLAPSED_BITS], num_bits = 0;
  unsigned long ent, num_ents, elt_sz, shift_sz;
  struct gomp_doacross_work_share *doacross;

  if (team == NULL || team->nthreads == 1)
    {
    empty:
      if (!extra)
	ws->doacross = NULL;
      else
	{
	  doacross = gomp_malloc_cleared (sizeof (*doacross) + extra);
	  doacross->extra = (void *) (doacross + 1);
	  ws->doacross = doacross;
	}
      return;
    }

  for (i = 0; i < ncounts; i++)
    {
      /* If any count is 0, GOMP_doacross_{post,wait} can't be called.  */
      if (counts[i] == 0)
	goto empty;

      if (num_bits <= MAX_COLLAPSED_BITS)
	{
	  unsigned int this_bits;
	  if (counts[i] == 1)
	    this_bits = 1;
	  else
	    this_bits = __SIZEOF_LONG__ * __CHAR_BIT__
			- __builtin_clzl (counts[i] - 1);
	  if (num_bits + this_bits <= MAX_COLLAPSED_BITS)
	    {
	      bits[i] = this_bits;
	      num_bits += this_bits;
	    }
	  else
	    num_bits = MAX_COLLAPSED_BITS + 1;
	}
    }

  if (ws->sched == GFS_STATIC)
    num_ents = team->nthreads;
  else if (ws->sched == GFS_GUIDED)
    num_ents = counts[0];
  else
    num_ents = (counts[0] - 1) / chunk_size + 1;
  if (num_bits <= MAX_COLLAPSED_BITS)
    {
      elt_sz = sizeof (unsigned long);
      shift_sz = ncounts * sizeof (unsigned int);
    }
  else
    {
      elt_sz = sizeof (unsigned long) * ncounts;
      shift_sz = 0;
    }
  elt_sz = (elt_sz + 63) & ~63UL;

  doacross = gomp_malloc (sizeof (*doacross) + 63 + num_ents * elt_sz
			  + shift_sz + extra);
  doacross->chunk_size = chunk_size;
  doacross->elt_sz = elt_sz;
  doacross->ncounts = ncounts;
  doacross->flattened = false;
  doacross->array = (unsigned char *)
		    ((((uintptr_t) (doacross + 1)) + 63 + shift_sz)
		     & ~(uintptr_t) 63);
  if (extra)
    {
      doacross->extra = doacross->array + num_ents * elt_sz;
      memset (doacross->extra, '\0', extra);
    }
  else
    doacross->extra = NULL;
  if (num_bits <= MAX_COLLAPSED_BITS)
    {
      unsigned int shift_count = 0;
      doacross->flattened = true;
      for (i = ncounts; i > 0; i--)
	{
	  doacross->shift_counts[i - 1] = shift_count;
	  shift_count += bits[i - 1];
	}
      for (ent = 0; ent < num_ents; ent++)
	*(unsigned long *) (doacross->array + ent * elt_sz) = 0;
    }
  else
    for (ent = 0; ent < num_ents; ent++)
      memset (doacross->array + ent * elt_sz, '\0',
	      sizeof (unsigned long) * ncounts);
  if (ws->sched == GFS_STATIC && chunk_size == 0)
    {
      unsigned long q = counts[0] / num_ents;
      unsigned long t = counts[0] % num_ents;
      doacross->boundary = t * (q + 1);
      doacross->q = q;
      doacross->t = t;
    }
  ws->doacross = doacross;
}

/* DOACROSS POST operation.  */

void
GOMP_doacross_post (long *counts)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  struct gomp_doacross_work_share *doacross = ws->doacross;
  unsigned long ent;
  unsigned int i;

  if (__builtin_expect (doacross == NULL, 0)
      || __builtin_expect (doacross->array == NULL, 0))
    {
      __sync_synchronize ();
      return;
    }

  if (__builtin_expect (ws->sched == GFS_STATIC, 1))
    ent = thr->ts.team_id;
  else if (ws->sched == GFS_GUIDED)
    ent = counts[0];
  else
    ent = counts[0] / doacross->chunk_size;
  unsigned long *array = (unsigned long *) (doacross->array
					    + ent * doacross->elt_sz);

  if (__builtin_expect (doacross->flattened, 1))
    {
      unsigned long flattened
	= (unsigned long) counts[0] << doacross->shift_counts[0];

      for (i = 1; i < doacross->ncounts; i++)
	flattened |= (unsigned long) counts[i]
		     << doacross->shift_counts[i];
      flattened++;
      if (flattened == __atomic_load_n (array, MEMMODEL_ACQUIRE))
	__atomic_thread_fence (MEMMODEL_RELEASE);
      else
	__atomic_store_n (array, flattened, MEMMODEL_RELEASE);
      return;
    }

  __atomic_thread_fence (MEMMODEL_ACQUIRE);
  for (i = doacross->ncounts; i-- > 0; )
    {
      if (counts[i] + 1UL != __atomic_load_n (&array[i], MEMMODEL_RELAXED))
	__atomic_store_n (&array[i], counts[i] + 1UL, MEMMODEL_RELEASE);
    }
}

/* DOACROSS WAIT operation.  */

void
GOMP_doacross_wait (long first, ...)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  struct gomp_doacross_work_share *doacross = ws->doacross;
  va_list ap;
  unsigned long ent;
  unsigned int i;

  if (__builtin_expect (doacross == NULL, 0)
      || __builtin_expect (doacross->array == NULL, 0))
    {
      __sync_synchronize ();
      return;
    }

  if (__builtin_expect (ws->sched == GFS_STATIC, 1))
    {
      if (ws->chunk_size == 0)
	{
	  if (first < doacross->boundary)
	    ent = first / (doacross->q + 1);
	  else
	    ent = (first - doacross->boundary) / doacross->q
		  + doacross->t;
	}
      else
	ent = first / ws->chunk_size % thr->ts.team->nthreads;
    }
  else if (ws->sched == GFS_GUIDED)
    ent = first;
  else
    ent = first / doacross->chunk_size;
  unsigned long *array = (unsigned long *) (doacross->array
					    + ent * doacross->elt_sz);

  if (__builtin_expect (doacross->flattened, 1))
    {
      unsigned long flattened
	= (unsigned long) first << doacross->shift_counts[0];
      unsigned long cur;

      va_start (ap, first);
      for (i = 1; i < doacross->ncounts; i++)
	flattened |= (unsigned long) va_arg (ap, long)
		     << doacross->shift_counts[i];
      cur = __atomic_load_n (array, MEMMODEL_ACQUIRE);
      if (flattened < cur)
	{
	  __atomic_thread_fence (MEMMODEL_RELEASE);
	  va_end (ap);
	  return;
	}
      doacross_spin (array, flattened, cur);
      __atomic_thread_fence (MEMMODEL_RELEASE);
      va_end (ap);
      return;
    }

  do
    {
      va_start (ap, first);
      for (i = 0; i < doacross->ncounts; i++)
	{
	  unsigned long thisv
	    = (unsigned long) (i ? va_arg (ap, long) : first) + 1;
	  unsigned long cur = __atomic_load_n (&array[i], MEMMODEL_RELAXED);
	  if (thisv < cur)
	    {
	      i = doacross->ncounts;
	      break;
	    }
	  if (thisv > cur)
	    break;
	}
      va_end (ap);
      if (i == doacross->ncounts)
	break;
      cpu_relax ();
    }
  while (1);
  __sync_synchronize ();
}

typedef unsigned long long gomp_ull;

void
gomp_doacross_ull_init (unsigned ncounts, gomp_ull *counts,
			gomp_ull chunk_size, size_t extra)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned int i, bits[MAX_COLLAPSED_BITS], num_bits = 0;
  unsigned long ent, num_ents, elt_sz, shift_sz;
  struct gomp_doacross_work_share *doacross;

  if (team == NULL || team->nthreads == 1)
    {
    empty:
      if (!extra)
	ws->doacross = NULL;
      else
	{
	  doacross = gomp_malloc_cleared (sizeof (*doacross) + extra);
	  doacross->extra = (void *) (doacross + 1);
	  ws->doacross = doacross;
	}
      return;
    }

  for (i = 0; i < ncounts; i++)
    {
      /* If any count is 0, GOMP_doacross_{post,wait} can't be called.  */
      if (counts[i] == 0)
	goto empty;

      if (num_bits <= MAX_COLLAPSED_BITS)
	{
	  unsigned int this_bits;
	  if (counts[i] == 1)
	    this_bits = 1;
	  else
	    this_bits = __SIZEOF_LONG_LONG__ * __CHAR_BIT__
			- __builtin_clzll (counts[i] - 1);
	  if (num_bits + this_bits <= MAX_COLLAPSED_BITS)
	    {
	      bits[i] = this_bits;
	      num_bits += this_bits;
	    }
	  else
	    num_bits = MAX_COLLAPSED_BITS + 1;
	}
    }

  if (ws->sched == GFS_STATIC)
    num_ents = team->nthreads;
  else if (ws->sched == GFS_GUIDED)
    num_ents = counts[0];
  else
    num_ents = (counts[0] - 1) / chunk_size + 1;
  if (num_bits <= MAX_COLLAPSED_BITS)
    {
      elt_sz = sizeof (unsigned long);
      shift_sz = ncounts * sizeof (unsigned int);
    }
  else
    {
      if (sizeof (gomp_ull) == sizeof (unsigned long))
	elt_sz = sizeof (gomp_ull) * ncounts;
      else if (sizeof (gomp_ull) == 2 * sizeof (unsigned long))
	elt_sz = sizeof (unsigned long) * 2 * ncounts;
      else
	abort ();
      shift_sz = 0;
    }
  elt_sz = (elt_sz + 63) & ~63UL;

  doacross = gomp_malloc (sizeof (*doacross) + 63 + num_ents * elt_sz
			  + shift_sz);
  doacross->chunk_size_ull = chunk_size;
  doacross->elt_sz = elt_sz;
  doacross->ncounts = ncounts;
  doacross->flattened = false;
  doacross->boundary = 0;
  doacross->array = (unsigned char *)
		    ((((uintptr_t) (doacross + 1)) + 63 + shift_sz)
		     & ~(uintptr_t) 63);
  if (extra)
    {
      doacross->extra = doacross->array + num_ents * elt_sz;
      memset (doacross->extra, '\0', extra);
    }
  else
    doacross->extra = NULL;
  if (num_bits <= MAX_COLLAPSED_BITS)
    {
      unsigned int shift_count = 0;
      doacross->flattened = true;
      for (i = ncounts; i > 0; i--)
	{
	  doacross->shift_counts[i - 1] = shift_count;
	  shift_count += bits[i - 1];
	}
      for (ent = 0; ent < num_ents; ent++)
	*(unsigned long *) (doacross->array + ent * elt_sz) = 0;
    }
  else
    for (ent = 0; ent < num_ents; ent++)
      memset (doacross->array + ent * elt_sz, '\0',
	      sizeof (unsigned long) * ncounts);
  if (ws->sched == GFS_STATIC && chunk_size == 0)
    {
      gomp_ull q = counts[0] / num_ents;
      gomp_ull t = counts[0] % num_ents;
      doacross->boundary_ull = t * (q + 1);
      doacross->q_ull = q;
      doacross->t = t;
    }
  ws->doacross = doacross;
}

/* DOACROSS POST operation.  */

void
GOMP_doacross_ull_post (gomp_ull *counts)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  struct gomp_doacross_work_share *doacross = ws->doacross;
  unsigned long ent;
  unsigned int i;

  if (__builtin_expect (doacross == NULL, 0)
      || __builtin_expect (doacross->array == NULL, 0))
    {
      __sync_synchronize ();
      return;
    }

  if (__builtin_expect (ws->sched == GFS_STATIC, 1))
    ent = thr->ts.team_id;
  else if (ws->sched == GFS_GUIDED)
    ent = counts[0];
  else
    ent = counts[0] / doacross->chunk_size_ull;

  if (__builtin_expect (doacross->flattened, 1))
    {
      unsigned long *array = (unsigned long *) (doacross->array
			      + ent * doacross->elt_sz);
      gomp_ull flattened
	= counts[0] << doacross->shift_counts[0];

      for (i = 1; i < doacross->ncounts; i++)
	flattened |= counts[i] << doacross->shift_counts[i];
      flattened++;
      if (flattened == __atomic_load_n (array, MEMMODEL_ACQUIRE))
	__atomic_thread_fence (MEMMODEL_RELEASE);
      else
	__atomic_store_n (array, flattened, MEMMODEL_RELEASE);
      return;
    }

  __atomic_thread_fence (MEMMODEL_ACQUIRE);
  if (sizeof (gomp_ull) == sizeof (unsigned long))
    {
      gomp_ull *array = (gomp_ull *) (doacross->array
				      + ent * doacross->elt_sz);

      for (i = doacross->ncounts; i-- > 0; )
	{
	  if (counts[i] + 1UL != __atomic_load_n (&array[i], MEMMODEL_RELAXED))
	    __atomic_store_n (&array[i], counts[i] + 1UL, MEMMODEL_RELEASE);
	}
    }
  else
    {
      unsigned long *array = (unsigned long *) (doacross->array
						+ ent * doacross->elt_sz);

      for (i = doacross->ncounts; i-- > 0; )
	{
	  gomp_ull cull = counts[i] + 1UL;
	  unsigned long c = (unsigned long) cull;
	  if (c != __atomic_load_n (&array[2 * i + 1], MEMMODEL_RELAXED))
	    __atomic_store_n (&array[2 * i + 1], c, MEMMODEL_RELEASE);
	  c = cull >> (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ / 2);
	  if (c != __atomic_load_n (&array[2 * i], MEMMODEL_RELAXED))
	    __atomic_store_n (&array[2 * i], c, MEMMODEL_RELEASE);
	}
    }
}

/* DOACROSS WAIT operation.  */

void
GOMP_doacross_ull_wait (gomp_ull first, ...)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  struct gomp_doacross_work_share *doacross = ws->doacross;
  va_list ap;
  unsigned long ent;
  unsigned int i;

  if (__builtin_expect (doacross == NULL, 0)
      || __builtin_expect (doacross->array == NULL, 0))
    {
      __sync_synchronize ();
      return;
    }

  if (__builtin_expect (ws->sched == GFS_STATIC, 1))
    {
      if (ws->chunk_size_ull == 0)
	{
	  if (first < doacross->boundary_ull)
	    ent = first / (doacross->q_ull + 1);
	  else
	    ent = (first - doacross->boundary_ull) / doacross->q_ull
		  + doacross->t;
	}
      else
	ent = first / ws->chunk_size_ull % thr->ts.team->nthreads;
    }
  else if (ws->sched == GFS_GUIDED)
    ent = first;
  else
    ent = first / doacross->chunk_size_ull;

  if (__builtin_expect (doacross->flattened, 1))
    {
      unsigned long *array = (unsigned long *) (doacross->array
						+ ent * doacross->elt_sz);
      gomp_ull flattened = first << doacross->shift_counts[0];
      unsigned long cur;

      va_start (ap, first);
      for (i = 1; i < doacross->ncounts; i++)
	flattened |= va_arg (ap, gomp_ull)
		     << doacross->shift_counts[i];
      cur = __atomic_load_n (array, MEMMODEL_ACQUIRE);
      if (flattened < cur)
	{
	  __atomic_thread_fence (MEMMODEL_RELEASE);
	  va_end (ap);
	  return;
	}
      doacross_spin (array, flattened, cur);
      __atomic_thread_fence (MEMMODEL_RELEASE);
      va_end (ap);
      return;
    }

  if (sizeof (gomp_ull) == sizeof (unsigned long))
    {
      gomp_ull *array = (gomp_ull *) (doacross->array
				      + ent * doacross->elt_sz);
      do
	{
	  va_start (ap, first);
	  for (i = 0; i < doacross->ncounts; i++)
	    {
	      gomp_ull thisv
		= (i ? va_arg (ap, gomp_ull) : first) + 1;
	      gomp_ull cur = __atomic_load_n (&array[i], MEMMODEL_RELAXED);
	      if (thisv < cur)
		{
		  i = doacross->ncounts;
		  break;
		}
	      if (thisv > cur)
		break;
	    }
	  va_end (ap);
	  if (i == doacross->ncounts)
	    break;
	  cpu_relax ();
	}
      while (1);
    }
  else
    {
      unsigned long *array = (unsigned long *) (doacross->array
						+ ent * doacross->elt_sz);
      do
	{
	  va_start (ap, first);
	  for (i = 0; i < doacross->ncounts; i++)
	    {
	      gomp_ull thisv
		= (i ? va_arg (ap, gomp_ull) : first) + 1;
	      unsigned long t
		= thisv >> (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ / 2);
	      unsigned long cur
		= __atomic_load_n (&array[2 * i], MEMMODEL_RELAXED);
	      if (t < cur)
		{
		  i = doacross->ncounts;
		  break;
		}
	      if (t > cur)
		break;
	      t = thisv;
	      cur = __atomic_load_n (&array[2 * i + 1], MEMMODEL_RELAXED);
	      if (t < cur)
		{
		  i = doacross->ncounts;
		  break;
		}
	      if (t > cur)
		break;
	    }
	  va_end (ap);
	  if (i == doacross->ncounts)
	    break;
	  cpu_relax ();
	}
      while (1);
    }
  __sync_synchronize ();
}
