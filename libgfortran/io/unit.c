/* Copyright (C) 2002, 2003, 2005, 2007, 2008, 2009, 2010 
   Free Software Foundation, Inc.
   Contributed by Andy Vaught
   F2003 I/O support contributed by Jerry DeLisle

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "io.h"
#include "fbuf.h"
#include "format.h"
#include "unix.h"
#include <stdlib.h>
#include <string.h>


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

GFC_INTEGER_4 next_available_newunit;
#define GFC_FIRST_NEWUNIT -10

#define CACHE_SIZE 3
static gfc_unit *unit_cache[CACHE_SIZE];
gfc_offset max_offset;
gfc_unit *unit_root;
#ifdef __GTHREAD_MUTEX_INIT
__gthread_mutex_t unit_lock = __GTHREAD_MUTEX_INIT;
#else
__gthread_mutex_t unit_lock;
#endif

/* We use these filenames for error reporting.  */

static char stdin_name[] = "stdin";
static char stdout_name[] = "stdout";
static char stderr_name[] = "stderr";

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


/* destroy_unit_mutex()-- Destroy the mutex and free memory of unit.  */

static void
destroy_unit_mutex (gfc_unit * u)
{
  __gthread_mutex_destroy (&u->lock);
  free (u);
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


/* get_external_unit()-- Given an integer, return a pointer to the unit
 * structure.  Returns NULL if the unit does not exist,
 * otherwise returns a locked unit. */

static gfc_unit *
get_external_unit (int n, int do_create)
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
	    destroy_unit_mutex (p);
	  goto retry;
	}

      dec_waiting_unlocked (p);
    }
  return p;
}


gfc_unit *
find_unit (int n)
{
  return get_external_unit (n, 0);
}


gfc_unit *
find_or_create_unit (int n)
{
  return get_external_unit (n, 1);
}


gfc_unit *
get_internal_unit (st_parameter_dt *dtp)
{
  gfc_unit * iunit;
  gfc_offset start_record = 0;

  /* Allocate memory for a unit structure.  */

  iunit = get_mem (sizeof (gfc_unit));
  if (iunit == NULL)
    {
      generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
      return NULL;
    }

  memset (iunit, '\0', sizeof (gfc_unit));
#ifdef __GTHREAD_MUTEX_INIT
  {
    __gthread_mutex_t tmp = __GTHREAD_MUTEX_INIT;
    iunit->lock = tmp;
  }
#else
  __GTHREAD_MUTEX_INIT_FUNCTION (&iunit->lock);
#endif
  __gthread_mutex_lock (&iunit->lock);

  iunit->recl = dtp->internal_unit_len;
  
  /* For internal units we set the unit number to -1.
     Otherwise internal units can be mistaken for a pre-connected unit or
     some other file I/O unit.  */
  iunit->unit_number = -1;

  /* Set up the looping specification from the array descriptor, if any.  */

  if (is_array_io (dtp))
    {
      iunit->rank = GFC_DESCRIPTOR_RANK (dtp->internal_unit_desc);
      iunit->ls = (array_loop_spec *)
	get_mem (iunit->rank * sizeof (array_loop_spec));
      dtp->internal_unit_len *=
	init_loop_spec (dtp->internal_unit_desc, iunit->ls, &start_record);

      start_record *= iunit->recl;
    }

  /* Set initial values for unit parameters.  */
  if (dtp->common.unit)
    {
      iunit->s = open_internal4 (dtp->internal_unit - start_record,
				 dtp->internal_unit_len, -start_record);
      fbuf_init (iunit, 256);
    }
  else
    iunit->s = open_internal (dtp->internal_unit - start_record,
			      dtp->internal_unit_len, -start_record);

  iunit->bytes_left = iunit->recl;
  iunit->last_record=0;
  iunit->maxrec=0;
  iunit->current_record=0;
  iunit->read_bad = 0;
  iunit->endfile = NO_ENDFILE;

  /* Set flags for the internal unit.  */

  iunit->flags.access = ACCESS_SEQUENTIAL;
  iunit->flags.action = ACTION_READWRITE;
  iunit->flags.blank = BLANK_NULL;
  iunit->flags.form = FORM_FORMATTED;
  iunit->flags.pad = PAD_YES;
  iunit->flags.status = STATUS_UNSPECIFIED;
  iunit->flags.sign = SIGN_SUPPRESS;
  iunit->flags.decimal = DECIMAL_POINT;
  iunit->flags.encoding = ENCODING_DEFAULT;
  iunit->flags.async = ASYNC_NO;
  iunit->flags.round = ROUND_COMPATIBLE;

  /* Initialize the data transfer parameters.  */

  dtp->u.p.advance_status = ADVANCE_YES;
  dtp->u.p.seen_dollar = 0;
  dtp->u.p.skips = 0;
  dtp->u.p.pending_spaces = 0;
  dtp->u.p.max_pos = 0;
  dtp->u.p.at_eof = 0;

  /* This flag tells us the unit is assigned to internal I/O.  */
  
  dtp->u.p.unit_is_internal = 1;

  return iunit;
}


/* free_internal_unit()-- Free memory allocated for internal units if any.  */
void
free_internal_unit (st_parameter_dt *dtp)
{
  if (!is_internal_unit (dtp))
    return;

  if (unlikely (is_char4_unit (dtp)))
    fbuf_destroy (dtp->u.p.current_unit);

  if (dtp->u.p.current_unit != NULL)
    {
      if (dtp->u.p.current_unit->ls != NULL)
	free (dtp->u.p.current_unit->ls);
  
      if (dtp->u.p.current_unit->s)
	free (dtp->u.p.current_unit->s);
  
      destroy_unit_mutex (dtp->u.p.current_unit);
    }
}
      


/* get_unit()-- Returns the unit structure associated with the integer
   unit or the internal file.  */

gfc_unit *
get_unit (st_parameter_dt *dtp, int do_create)
{

  if ((dtp->common.flags & IOPARM_DT_HAS_INTERNAL_UNIT) != 0)
    return get_internal_unit (dtp);

  /* Has to be an external unit.  */

  dtp->u.p.unit_is_internal = 0;
  dtp->internal_unit_desc = NULL;

  return get_external_unit (dtp->common.unit, do_create);
}


/*************************/
/* Initialize everything.  */

void
init_units (void)
{
  gfc_unit *u;
  unsigned int i;

#ifndef __GTHREAD_MUTEX_INIT
  __GTHREAD_MUTEX_INIT_FUNCTION (&unit_lock);
#endif

  next_available_newunit = GFC_FIRST_NEWUNIT;

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
      u->flags.sign = SIGN_SUPPRESS;
      u->flags.decimal = DECIMAL_POINT;
      u->flags.encoding = ENCODING_DEFAULT;
      u->flags.async = ASYNC_NO;
      u->flags.round = ROUND_COMPATIBLE;
     
      u->recl = options.default_recl;
      u->endfile = NO_ENDFILE;

      u->file_len = strlen (stdin_name);
      u->file = get_mem (u->file_len);
      memmove (u->file, stdin_name, u->file_len);

      fbuf_init (u, 0);
    
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
      u->flags.sign = SIGN_SUPPRESS;
      u->flags.decimal = DECIMAL_POINT;
      u->flags.encoding = ENCODING_DEFAULT;
      u->flags.async = ASYNC_NO;
      u->flags.round = ROUND_COMPATIBLE;

      u->recl = options.default_recl;
      u->endfile = AT_ENDFILE;
    
      u->file_len = strlen (stdout_name);
      u->file = get_mem (u->file_len);
      memmove (u->file, stdout_name, u->file_len);
      
      fbuf_init (u, 0);

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
      u->flags.sign = SIGN_SUPPRESS;
      u->flags.decimal = DECIMAL_POINT;
      u->flags.encoding = ENCODING_DEFAULT;
      u->flags.async = ASYNC_NO;
      u->flags.round = ROUND_COMPATIBLE;

      u->recl = options.default_recl;
      u->endfile = AT_ENDFILE;

      u->file_len = strlen (stderr_name);
      u->file = get_mem (u->file_len);
      memmove (u->file, stderr_name, u->file_len);
      
      fbuf_init (u, 256);  /* 256 bytes should be enough, probably not doing
                              any kind of exotic formatting to stderr.  */

      __gthread_mutex_unlock (&u->lock);
    }

  /* Calculate the maximum file offset in a portable manner.
     max will be the largest signed number for the type gfc_offset.
     set a 1 in the LSB and keep a running sum, stopping at MSB-1 bit.  */
  max_offset = 0;
  for (i = 0; i < sizeof (max_offset) * 8 - 1; i++)
    max_offset = max_offset + ((gfc_offset) 1 << i);
}


static int
close_unit_1 (gfc_unit *u, int locked)
{
  int i, rc;
  
  /* If there are previously written bytes from a write with ADVANCE="no"
     Reposition the buffer before closing.  */
  if (u->previous_nonadvancing_write)
    finish_last_advance_record (u);

  rc = (u->s == NULL) ? 0 : sclose (u->s) == -1;

  u->closed = 1;
  if (!locked)
    __gthread_mutex_lock (&unit_lock);

  for (i = 0; i < CACHE_SIZE; i++)
    if (unit_cache[i] == u)
      unit_cache[i] = NULL;

  delete_unit (u);

  if (u->file)
    free (u->file);
  u->file = NULL;
  u->file_len = 0;

  free_format_hash_table (u);  
  fbuf_destroy (u);

  if (!locked)
    __gthread_mutex_unlock (&u->lock);

  /* If there are any threads waiting in find_unit for this unit,
     avoid freeing the memory, the last such thread will free it
     instead.  */
  if (u->waiting == 0)
    destroy_unit_mutex (u);

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
   associated with the stream is freed.  Returns nonzero on I/O error.
   Should be called with the u->lock locked. */

int
close_unit (gfc_unit *u)
{
  return close_unit_1 (u, 0);
}


/* close_units()-- Delete units on completion.  We just keep deleting
   the root of the treap until there is nothing left.
   Not sure what to do with locking here.  Some other thread might be
   holding some unit's lock and perhaps hold it indefinitely
   (e.g. waiting for input from some pipe) and close_units shouldn't
   delay the program too much.  */

void
close_units (void)
{
  __gthread_mutex_lock (&unit_lock);
  while (unit_root != NULL)
    close_unit_1 (unit_root, 1);
  __gthread_mutex_unlock (&unit_lock);
}


/* update_position()-- Update the flags position for later use by inquire.  */

void
update_position (gfc_unit *u)
{
  /* If unit is not seekable, this makes no sense (and the standard is
     silent on this matter), and thus we don't change the position for
     a non-seekable file.  */
  if (is_seekable (u->s))
    {
      gfc_offset cur = stell (u->s);
      if (cur == 0)
	u->flags.position = POSITION_REWIND;
      else if (cur != -1 && (file_length (u->s) == cur))
	u->flags.position = POSITION_APPEND;
      else
	u->flags.position = POSITION_ASIS;
    }
}


/* High level interface to truncate a file safely, i.e. flush format
   buffers, check that it's a regular file, and generate error if that
   occurs.  Just like POSIX ftruncate, returns 0 on success, -1 on
   failure.  */

int
unit_truncate (gfc_unit * u, gfc_offset pos, st_parameter_common * common)
{
  int ret;

  /* Make sure format buffer is flushed.  */
  if (u->flags.form == FORM_FORMATTED)
    {
      if (u->mode == READING)
	pos += fbuf_reset (u);
      else
	fbuf_flush (u, u->mode);
    }
  
  /* Don't try to truncate a special file, just pretend that it
     succeeds.  */
  if (is_special (u->s) || !is_seekable (u->s))
    {
      sflush (u->s);
      return 0;
    }

  /* struncate() should flush the stream buffer if necessary, so don't
     bother calling sflush() here.  */
  ret = struncate (u->s, pos);

  if (ret != 0)
    {
      generate_error (common, LIBERROR_OS, NULL);
      u->endfile = NO_ENDFILE;
      u->flags.position = POSITION_ASIS;
    }
  else
    {
      u->endfile = AT_ENDFILE;
      u->flags.position = POSITION_APPEND;
    }

  return ret;
}


/* filename_from_unit()-- If the unit_number exists, return a pointer to the
   name of the associated file, otherwise return the empty string.  The caller
   must free memory allocated for the filename string.  */

char *
filename_from_unit (int n)
{
  char *filename;
  gfc_unit *u;
  int c;

  /* Find the unit.  */
  u = unit_root;
  while (u != NULL)
    {
      c = compare (n, u->unit_number);
      if (c < 0)
	u = u->left;
      if (c > 0)
	u = u->right;
      if (c == 0)
	break;
    }

  /* Get the filename.  */
  if (u != NULL)
    {
      filename = (char *) get_mem (u->file_len + 1);
      unpack_filename (filename, u->file, u->file_len);
      return filename;
    }
  else
    return (char *) NULL;
}

void
finish_last_advance_record (gfc_unit *u)
{
  
  if (u->saved_pos > 0)
    fbuf_seek (u, u->saved_pos, SEEK_CUR);

  if (!(u->unit_number == options.stdout_unit
	|| u->unit_number == options.stderr_unit))
    {
#ifdef HAVE_CRLF
      const int len = 2;
#else
      const int len = 1;
#endif
      char *p = fbuf_alloc (u, len);
      if (!p)
	os_error ("Completing record after ADVANCE_NO failed");
#ifdef HAVE_CRLF
      *(p++) = '\r';
#endif
      *p = '\n';
    }

  fbuf_flush (u, u->mode);
}

/* Assign a negative number for NEWUNIT in OPEN statements.  */
GFC_INTEGER_4
get_unique_unit_number (st_parameter_open *opp)
{
  GFC_INTEGER_4 num;

  __gthread_mutex_lock (&unit_lock);
  num = next_available_newunit--;

  /* Do not allow NEWUNIT numbers to wrap.  */
  if (next_available_newunit >=  GFC_FIRST_NEWUNIT )
    {
      __gthread_mutex_unlock (&unit_lock);
      generate_error (&opp->common, LIBERROR_INTERNAL, "NEWUNIT exhausted");
      return 0;
    }
  __gthread_mutex_unlock (&unit_lock);
  return num;
}
