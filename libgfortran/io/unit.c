/* Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <stdlib.h>
#include <string.h>
#include "libgfortran.h"
#include "io.h"


/* IO locking rules:
   UNIT_LOCK is a master lock, protecting UNIT_ROOT tree and UNIT_CACHE.
   Concurrent use of different units should be supported, so
   each unit has its own lock, LOCK.
   Open should be atomic with its reopening of units and list_read.c
   in several places needs find_unit another unit while holding stdin
   unit's lock, so it must be possible to acquire UNIT_LOCK while holding
   some unit's lock.  Therefore to avoid deadlocks, it is forbidden
   to acquire unit's private locks while holding UNIT_LOCK, except
   for freshly created units (where no other thread can get at their
   address yet) or when using just trylock rather than lock operation.
   In addition to unit's private lock each unit has a WAITERS counter
   and CLOSED flag.  WAITERS counter must be either only
   atomically incremented/decremented in all places (if atomic builtins
   are supported), or protected by UNIT_LOCK in all places (otherwise).
   CLOSED flag must be always protected by unit's LOCK.
   After finding a unit in UNIT_CACHE or UNIT_ROOT with UNIT_LOCK held,
   WAITERS must be incremented to avoid concurrent close from freeing
   the unit between unlocking UNIT_LOCK and acquiring unit's LOCK.
   Unit freeing is always done under UNIT_LOCK.  If close_unit sees any
   WAITERS, it doesn't free the unit but instead sets the CLOSED flag
   and the thread that decrements WAITERS to zero while CLOSED flag is
   set is responsible for freeing it (while holding UNIT_LOCK).
   flush_all_units operation is iterating over the unit tree with
   increasing UNIT_NUMBER while holding UNIT_LOCK and attempting to
   flush each unit (and therefore needs the unit's LOCK held as well).
   To avoid deadlocks, it just trylocks the LOCK and if unsuccessful,
   remembers the current unit's UNIT_NUMBER, unlocks UNIT_LOCK, acquires
   unit's LOCK and after flushing reacquires UNIT_LOCK and restarts with
   the smallest UNIT_NUMBER above the last one flushed.

   If find_unit/find_or_create_unit/find_file/get_unit routines return
   non-NULL, the returned unit has its private lock locked and when the
   caller is done with it, it must call either unlock_unit or close_unit
   on it.  unlock_unit or close_unit must be always called only with the
   private lock held.  */

/* Subroutines related to units */


#define CACHE_SIZE 3
static gfc_unit internal_unit, *unit_cache[CACHE_SIZE];
gfc_offset max_offset;
gfc_unit *unit_root;
#ifdef __GTHREAD_MUTEX_INIT
__gthread_mutex_t unit_lock = __GTHREAD_MUTEX_INIT;
#else
__gthread_mutex_t unit_lock;
#endif

/* This implementation is based on Stefan Nilsson's article in the
 * July 1997 Doctor Dobb's Journal, "Treaps in Java". */

/* pseudo_random()-- Simple linear congruential pseudorandom number
 * generator.  The period of this generator is 44071, which is plenty
 * for our purposes.  */

static int
pseudo_random (void)
{
  static int x0 = 5341;

  x0 = (22611 * x0 + 10) % 44071;
  return x0;
}


/* rotate_left()-- Rotate the treap left */

static gfc_unit *
rotate_left (gfc_unit * t)
{
  gfc_unit *temp;

  temp = t->right;
  t->right = t->right->left;
  temp->left = t;

  return temp;
}


/* rotate_right()-- Rotate the treap right */

static gfc_unit *
rotate_right (gfc_unit * t)
{
  gfc_unit *temp;

  temp = t->left;
  t->left = t->left->right;
  temp->right = t;

  return temp;
}



static int
compare (int a, int b)
{
  if (a < b)
    return -1;
  if (a > b)
    return 1;

  return 0;
}


/* insert()-- Recursive insertion function.  Returns the updated treap. */

static gfc_unit *
insert (gfc_unit *new, gfc_unit *t)
{
  int c;

  if (t == NULL)
    return new;

  c = compare (new->unit_number, t->unit_number);

  if (c < 0)
    {
      t->left = insert (new, t->left);
      if (t->priority < t->left->priority)
	t = rotate_right (t);
    }

  if (c > 0)
    {
      t->right = insert (new, t->right);
      if (t->priority < t->right->priority)
	t = rotate_left (t);
    }

  if (c == 0)
    internal_error (NULL, "insert(): Duplicate key found!");

  return t;
}


/* insert_unit()-- Create a new node, insert it into the treap.  */

static gfc_unit *
insert_unit (int n)
{
  gfc_unit *u = get_mem (sizeof (gfc_unit));
  memset (u, '\0', sizeof (gfc_unit));
  u->unit_number = n;
#ifdef __GTHREAD_MUTEX_INIT
  {
    __gthread_mutex_t tmp = __GTHREAD_MUTEX_INIT;
    u->lock = tmp;
  }
#else
  __GTHREAD_MUTEX_INIT_FUNCTION (&u->lock);
#endif
  __gthread_mutex_lock (&u->lock);
  u->priority = pseudo_random ();
  unit_root = insert (u, unit_root);
  return u;
}


static gfc_unit *
delete_root (gfc_unit * t)
{
  gfc_unit *temp;

  if (t->left == NULL)
    return t->right;
  if (t->right == NULL)
    return t->left;

  if (t->left->priority > t->right->priority)
    {
      temp = rotate_right (t);
      temp->right = delete_root (t);
    }
  else
    {
      temp = rotate_left (t);
      temp->left = delete_root (t);
    }

  return temp;
}


/* delete_treap()-- Delete an element from a tree.  The 'old' value
 * does not necessarily have to point to the element to be deleted, it
 * must just point to a treap structure with the key to be deleted.
 * Returns the new root node of the tree. */

static gfc_unit *
delete_treap (gfc_unit * old, gfc_unit * t)
{
  int c;

  if (t == NULL)
    return NULL;

  c = compare (old->unit_number, t->unit_number);

  if (c < 0)
    t->left = delete_treap (old, t->left);
  if (c > 0)
    t->right = delete_treap (old, t->right);
  if (c == 0)
    t = delete_root (t);

  return t;
}


/* delete_unit()-- Delete a unit from a tree */

static void
delete_unit (gfc_unit * old)
{
  unit_root = delete_treap (old, unit_root);
}


/* find_unit()-- Given an integer, return a pointer to the unit
 * structure.  Returns NULL if the unit does not exist,
 * otherwise returns a locked unit. */

static gfc_unit *
find_unit_1 (int n, int do_create)
{
  gfc_unit *p;
  int c, created = 0;

  __gthread_mutex_lock (&unit_lock);
retry:
  for (c = 0; c < CACHE_SIZE; c++)
    if (unit_cache[c] != NULL && unit_cache[c]->unit_number == n)
      {
	p = unit_cache[c];
	goto found;
      }

  p = unit_root;
  while (p != NULL)
    {
      c = compare (n, p->unit_number);
      if (c < 0)
	p = p->left;
      if (c > 0)
	p = p->right;
      if (c == 0)
	break;
    }

  if (p == NULL && do_create)
    {
      p = insert_unit (n);
      created = 1;
    }

  if (p != NULL)
    {
      for (c = 0; c < CACHE_SIZE - 1; c++)
	unit_cache[c] = unit_cache[c + 1];

      unit_cache[CACHE_SIZE - 1] = p;
    }

  if (created)
    {
      /* Newly created units have their lock held already
	 from insert_unit.  Just unlock UNIT_LOCK and return.  */
      __gthread_mutex_unlock (&unit_lock);
      return p;
    }

found:
  if (p != NULL)
    {
      /* Fast path.  */
      if (! __gthread_mutex_trylock (&p->lock))
	{
	  /* assert (p->closed == 0); */
	  __gthread_mutex_unlock (&unit_lock);
	  return p;
	}

      inc_waiting_locked (p);
    }

  __gthread_mutex_unlock (&unit_lock);

  if (p != NULL)
    {
      __gthread_mutex_lock (&p->lock);
      if (p->closed)
	{
	  __gthread_mutex_lock (&unit_lock);
	  __gthread_mutex_unlock (&p->lock);
	  if (predec_waiting_locked (p) == 0)
	    free_mem (p);
	  goto retry;
	}

      dec_waiting_unlocked (p);
    }
  return p;
}

gfc_unit *
find_unit (int n)
{
  return find_unit_1 (n, 0);
}

gfc_unit *
find_or_create_unit (int n)
{
  return find_unit_1 (n, 1);
}

/* get_unit()-- Returns the unit structure associated with the integer
 * unit or the internal file. */

gfc_unit *
get_unit (st_parameter_dt *dtp, int do_create)
{
  if ((dtp->common.flags & IOPARM_DT_HAS_INTERNAL_UNIT) != 0)
    {
      __gthread_mutex_lock (&internal_unit.lock);
      internal_unit.recl = dtp->internal_unit_len;
      if (is_array_io (dtp))
	{
	  internal_unit.rank = GFC_DESCRIPTOR_RANK (dtp->internal_unit_desc);
	  internal_unit.ls = (array_loop_spec *)
	    get_mem (internal_unit.rank * sizeof (array_loop_spec));
	  dtp->internal_unit_len *=
	    init_loop_spec (dtp->internal_unit_desc, internal_unit.ls);
	}

      internal_unit.s =
	open_internal (dtp->internal_unit, dtp->internal_unit_len);
      internal_unit.bytes_left = internal_unit.recl;
      internal_unit.last_record=0;
      internal_unit.maxrec=0;
      internal_unit.current_record=0;

      if (dtp->u.p.mode==WRITING && !is_array_io (dtp))
        empty_internal_buffer (internal_unit.s);

      /* Set flags for the internal unit */

      internal_unit.flags.access = ACCESS_SEQUENTIAL;
      internal_unit.flags.action = ACTION_READWRITE;
      internal_unit.flags.form = FORM_FORMATTED;
      internal_unit.flags.delim = DELIM_NONE;
      internal_unit.flags.pad = PAD_YES;

      return &internal_unit;
    }

  /* Has to be an external unit */

  return find_unit_1 (dtp->common.unit, do_create);
}


/* is_internal_unit()-- Determine if the current unit is internal or not */

int
is_internal_unit (st_parameter_dt *dtp)
{
  return dtp->u.p.current_unit == &internal_unit;
}


/* is_array_io ()-- Determine if the I/O is to/from an array */

int
is_array_io (st_parameter_dt *dtp)
{
  return dtp->internal_unit_desc != NULL;
}


/*************************/
/* Initialize everything */

void
init_units (void)
{
  gfc_unit *u;
  unsigned int i;

#ifndef __GTHREAD_MUTEX_INIT
  __GTHREAD_MUTEX_INIT_FUNCTION (&unit_lock);
#endif

#ifdef __GTHREAD_MUTEX_INIT
  {
    __gthread_mutex_t tmp = __GTHREAD_MUTEX_INIT;
    internal_unit.lock = tmp;
  }
#else
  __GTHREAD_MUTEX_INIT_FUNCTION (&internal_unit.lock);
#endif

  if (options.stdin_unit >= 0)
    {				/* STDIN */
      u = insert_unit (options.stdin_unit);
      u->s = input_stream ();

      u->flags.action = ACTION_READ;

      u->flags.access = ACCESS_SEQUENTIAL;
      u->flags.form = FORM_FORMATTED;
      u->flags.status = STATUS_OLD;
      u->flags.blank = BLANK_NULL;
      u->flags.pad = PAD_YES;
      u->flags.position = POSITION_ASIS;

      u->recl = options.default_recl;
      u->endfile = NO_ENDFILE;

      __gthread_mutex_unlock (&u->lock);
    }

  if (options.stdout_unit >= 0)
    {				/* STDOUT */
      u = insert_unit (options.stdout_unit);
      u->s = output_stream ();

      u->flags.action = ACTION_WRITE;

      u->flags.access = ACCESS_SEQUENTIAL;
      u->flags.form = FORM_FORMATTED;
      u->flags.status = STATUS_OLD;
      u->flags.blank = BLANK_NULL;
      u->flags.position = POSITION_ASIS;

      u->recl = options.default_recl;
      u->endfile = AT_ENDFILE;

      __gthread_mutex_unlock (&u->lock);
    }

  if (options.stderr_unit >= 0)
    {				/* STDERR */
      u = insert_unit (options.stderr_unit);
      u->s = error_stream ();

      u->flags.action = ACTION_WRITE;

      u->flags.access = ACCESS_SEQUENTIAL;
      u->flags.form = FORM_FORMATTED;
      u->flags.status = STATUS_OLD;
      u->flags.blank = BLANK_NULL;
      u->flags.position = POSITION_ASIS;

      u->recl = options.default_recl;
      u->endfile = AT_ENDFILE;

      __gthread_mutex_unlock (&u->lock);
    }

  /* Calculate the maximum file offset in a portable manner.
   * max will be the largest signed number for the type gfc_offset.
   *
   * set a 1 in the LSB and keep a running sum, stopping at MSB-1 bit. */

  max_offset = 0;
  for (i = 0; i < sizeof (max_offset) * 8 - 1; i++)
    max_offset = max_offset + ((gfc_offset) 1 << i);
}


static int
close_unit_1 (gfc_unit *u, int locked)
{
  int i, rc;

  rc = (u->s == NULL) ? 0 : sclose (u->s) == FAILURE;

  u->closed = 1;
  if (!locked)
    __gthread_mutex_lock (&unit_lock);

  for (i = 0; i < CACHE_SIZE; i++)
    if (unit_cache[i] == u)
      unit_cache[i] = NULL;

  delete_unit (u);

  if (u->file)
    free_mem (u->file);
  u->file = NULL;
  u->file_len = 0;

  if (!locked)
    __gthread_mutex_unlock (&u->lock);

  /* If there are any threads waiting in find_unit for this unit,
     avoid freeing the memory, the last such thread will free it
     instead.  */
  if (u->waiting == 0)
    free_mem (u);

  if (!locked)
    __gthread_mutex_unlock (&unit_lock);

  return rc;
}

void
unlock_unit (gfc_unit *u)
{
  __gthread_mutex_unlock (&u->lock);
}

/* close_unit()-- Close a unit.  The stream is closed, and any memory
 * associated with the stream is freed.  Returns nonzero on I/O error.
 * Should be called with the u->lock locked. */

int
close_unit (gfc_unit *u)
{
  return close_unit_1 (u, 0);
}


/* close_units()-- Delete units on completion.  We just keep deleting
 * the root of the treap until there is nothing left.
 * Not sure what to do with locking here.  Some other thread might be
 * holding some unit's lock and perhaps hold it indefinitely
 * (e.g. waiting for input from some pipe) and close_units shouldn't
 * delay the program too much.  */

void
close_units (void)
{
  __gthread_mutex_lock (&unit_lock);
  while (unit_root != NULL)
    close_unit_1 (unit_root, 1);
  __gthread_mutex_unlock (&unit_lock);
}
