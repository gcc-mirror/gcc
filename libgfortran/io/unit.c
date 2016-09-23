/* Copyright (C) 2002-2016 Free Software Foundation, Inc.
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

/* Unit number to be assigned when NEWUNIT is used in an OPEN statement.  */
#define GFC_FIRST_NEWUNIT -10
#define NEWUNIT_STACK_SIZE 16
static GFC_INTEGER_4 next_available_newunit = GFC_FIRST_NEWUNIT;

/* A stack to save previously used newunit-assigned unit numbers to
   allow them to be reused without reallocating the gfc_unit structure
   which is still in the treap.  */
static gfc_saved_unit newunit_stack[NEWUNIT_STACK_SIZE];
static int newunit_tos = 0; /* Index to Top of Stack.  */

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


#ifdef HAVE_NEWLOCALE
locale_t c_locale;
#else
/* If we don't have POSIX 2008 per-thread locales, we need to use the
   traditional setlocale().  To prevent multiple concurrent threads
   doing formatted I/O from messing up the locale, we need to store a
   global old_locale, and a counter keeping track of how many threads
   are currently doing formatted I/O.  The first thread saves the old
   locale, and the last one restores it.  */
char *old_locale;
int old_locale_ctr;
#ifdef __GTHREAD_MUTEX_INIT
__gthread_mutex_t old_locale_lock = __GTHREAD_MUTEX_INIT;
#else
__gthread_mutex_t old_locale_lock;
#endif
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
  gfc_unit *u = xcalloc (1, sizeof (gfc_unit));
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


/* get_gfc_unit()-- Given an integer, return a pointer to the unit
 * structure.  Returns NULL if the unit does not exist,
 * otherwise returns a locked unit. */

static gfc_unit *
get_gfc_unit (int n, int do_create)
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
  if (p != NULL && (p->child_dtio == 0))
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

  if (p != NULL && (p->child_dtio == 0))
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
  return get_gfc_unit (n, 0);
}


gfc_unit *
find_or_create_unit (int n)
{
  return get_gfc_unit (n, 1);
}


/* Helper function to check rank, stride, format string, and namelist.
   This is used for optimization. You can't trim out blanks or shorten
   the string if trailing spaces are significant.  */
static bool
is_trim_ok (st_parameter_dt *dtp)
{
  /* Check rank and stride.  */
  if (dtp->internal_unit_desc)
    return false;
  /* Format strings can not have 'BZ' or '/'.  */
  if (dtp->common.flags & IOPARM_DT_HAS_FORMAT)
    {
      char *p = dtp->format;
      off_t i;
      if (dtp->common.flags & IOPARM_DT_HAS_BLANK)
	return false;
      for (i = 0; i < dtp->format_len; i++)
	{
	  if (p[i] == '/') return false;
	  if (p[i] == 'b' || p[i] == 'B')
	    if (p[i+1] == 'z' || p[i+1] == 'Z')
	      return false;
	}
    }
  if (dtp->u.p.ionml) /* A namelist.  */
    return false;
  return true;
}


gfc_unit *
set_internal_unit (st_parameter_dt *dtp, gfc_unit *iunit, int kind)
{
  gfc_offset start_record = 0;

  iunit->recl = dtp->internal_unit_len;
  iunit->internal_unit = dtp->internal_unit;
  iunit->internal_unit_len = dtp->internal_unit_len;
  iunit->internal_unit_kind = kind;

  /* As an optimization, adjust the unit record length to not
     include trailing blanks. This will not work under certain conditions
     where trailing blanks have significance.  */
  if (dtp->u.p.mode == READING && is_trim_ok (dtp))
    {
      int len;
      if (kind == 1)
	  len = string_len_trim (iunit->internal_unit_len,
						   iunit->internal_unit);
      else
	  len = string_len_trim_char4 (iunit->internal_unit_len,
			      (const gfc_char4_t*) iunit->internal_unit);
      iunit->internal_unit_len = len;
      iunit->recl = iunit->internal_unit_len;
    }

  /* Set up the looping specification from the array descriptor, if any.  */

  if (is_array_io (dtp))
    {
      iunit->rank = GFC_DESCRIPTOR_RANK (dtp->internal_unit_desc);
      iunit->ls = (array_loop_spec *)
	xmallocarray (iunit->rank, sizeof (array_loop_spec));
      iunit->internal_unit_len *=
	init_loop_spec (dtp->internal_unit_desc, iunit->ls, &start_record);

      start_record *= iunit->recl;
    }

  /* Set initial values for unit parameters.  */
  if (kind == 4)
    iunit->s = open_internal4 (iunit->internal_unit - start_record,
				 iunit->internal_unit_len, -start_record);
  else
    iunit->s = open_internal (iunit->internal_unit - start_record,
			      iunit->internal_unit_len, -start_record);

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
  iunit->flags.sign = SIGN_UNSPECIFIED;
  iunit->flags.decimal = DECIMAL_POINT;
  iunit->flags.delim = DELIM_UNSPECIFIED;
  iunit->flags.encoding = ENCODING_DEFAULT;
  iunit->flags.async = ASYNC_NO;
  iunit->flags.round = ROUND_UNSPECIFIED;

  /* Initialize the data transfer parameters.  */

  dtp->u.p.advance_status = ADVANCE_YES;
  dtp->u.p.seen_dollar = 0;
  dtp->u.p.skips = 0;
  dtp->u.p.pending_spaces = 0;
  dtp->u.p.max_pos = 0;
  dtp->u.p.at_eof = 0;
  return iunit;
}


/* stash_internal_unit()-- Push the internal unit number onto the
   avaialble stack.  */
void
stash_internal_unit (st_parameter_dt *dtp)
{
  __gthread_mutex_lock (&unit_lock);
  newunit_tos++;
  if (newunit_tos >= NEWUNIT_STACK_SIZE)
    internal_error (&dtp->common, "stash_internal_unit(): Stack Size Exceeded");
  newunit_stack[newunit_tos].unit_number = dtp->common.unit;
  newunit_stack[newunit_tos].unit = dtp->u.p.current_unit;
  __gthread_mutex_unlock (&unit_lock);
}



/* get_unit()-- Returns the unit structure associated with the integer
   unit or the internal file.  */

gfc_unit *
get_unit (st_parameter_dt *dtp, int do_create)
{
  gfc_unit * unit;

  if ((dtp->common.flags & IOPARM_DT_HAS_INTERNAL_UNIT) != 0)
    {
      int kind;
      if (dtp->common.unit == GFC_INTERNAL_UNIT)
        kind = 1;
      else if (dtp->common.unit == GFC_INTERNAL_UNIT4)
        kind = 4;
      else
	internal_error (&dtp->common, "get_unit(): Bad internal unit KIND");

      if ((dtp->common.flags & IOPARM_DT_HAS_UDTIO) != 0)
	{
	  dtp->u.p.unit_is_internal = 1;
	  dtp->common.unit = get_unique_unit_number (&dtp->common);
	  unit = get_gfc_unit (dtp->common.unit, do_create);
	  set_internal_unit (dtp, unit, kind);
	  fbuf_init (unit, 128);
	  return unit;
	}
      else
	{
	  if (newunit_tos)
	    {
	      dtp->common.unit = newunit_stack[newunit_tos].unit_number;
	      unit = newunit_stack[newunit_tos--].unit;
	      unit->fbuf->act = unit->fbuf->pos = 0;
	    }
	  else
	    {
	      dtp->common.unit = get_unique_unit_number (&dtp->common);
	      unit = xcalloc (1, sizeof (gfc_unit));
	      fbuf_init (unit, 128);
	    }
	  set_internal_unit (dtp, unit, kind);
	  return unit;
	}
    }
  /* Has to be an external unit.  */
  dtp->u.p.unit_is_internal = 0;
  dtp->internal_unit = NULL;
  dtp->internal_unit_desc = NULL;
  unit = get_gfc_unit (dtp->common.unit, do_create);
  return unit;
}


/*************************/
/* Initialize everything.  */

void
init_units (void)
{
  gfc_unit *u;
  unsigned int i;

#ifdef HAVE_NEWLOCALE
  c_locale = newlocale (0, "C", 0);
#else
#ifndef __GTHREAD_MUTEX_INIT
  __GTHREAD_MUTEX_INIT_FUNCTION (&old_locale_lock);
#endif
#endif

#ifndef __GTHREAD_MUTEX_INIT
  __GTHREAD_MUTEX_INIT_FUNCTION (&unit_lock);
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
      u->flags.sign = SIGN_UNSPECIFIED;
      u->flags.decimal = DECIMAL_POINT;
      u->flags.delim = DELIM_UNSPECIFIED;
      u->flags.encoding = ENCODING_DEFAULT;
      u->flags.async = ASYNC_NO;
      u->flags.round = ROUND_UNSPECIFIED;

      u->recl = options.default_recl;
      u->endfile = NO_ENDFILE;

      u->filename = strdup (stdin_name);

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
      u->flags.sign = SIGN_UNSPECIFIED;
      u->flags.decimal = DECIMAL_POINT;
      u->flags.delim = DELIM_UNSPECIFIED;
      u->flags.encoding = ENCODING_DEFAULT;
      u->flags.async = ASYNC_NO;
      u->flags.round = ROUND_UNSPECIFIED;

      u->recl = options.default_recl;
      u->endfile = AT_ENDFILE;

      u->filename = strdup (stdout_name);

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
      u->flags.sign = SIGN_UNSPECIFIED;
      u->flags.decimal = DECIMAL_POINT;
      u->flags.encoding = ENCODING_DEFAULT;
      u->flags.async = ASYNC_NO;
      u->flags.round = ROUND_UNSPECIFIED;

      u->recl = options.default_recl;
      u->endfile = AT_ENDFILE;

      u->filename = strdup (stderr_name);

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

  /* Initialize the newunit stack.  */
  memset (newunit_stack, 0, NEWUNIT_STACK_SIZE * sizeof(gfc_saved_unit));
  newunit_tos = 0;
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

  free (u->filename);
  u->filename = NULL;

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

  while (newunit_tos != 0)
    if (newunit_stack[newunit_tos].unit)
      {
	fbuf_destroy (newunit_stack[newunit_tos].unit);
	free (newunit_stack[newunit_tos].unit->s);
	free (newunit_stack[newunit_tos--].unit);
      }
#ifdef HAVE_FREELOCALE
  freelocale (c_locale);
#endif
}


/* High level interface to truncate a file, i.e. flush format buffers,
   and generate an error or set some flags.  Just like POSIX
   ftruncate, returns 0 on success, -1 on failure.  */

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

  /* struncate() should flush the stream buffer if necessary, so don't
     bother calling sflush() here.  */
  ret = struncate (u->s, pos);

  if (ret != 0)
    generate_error (common, LIBERROR_OS, NULL);
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
  if (u != NULL && u->filename != NULL)
    return strdup (u->filename);
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

/* Assign a negative number for NEWUNIT in OPEN statements or for
   internal units.  */
GFC_INTEGER_4
get_unique_unit_number (st_parameter_common *common)
{
  GFC_INTEGER_4 num;

#ifdef HAVE_SYNC_FETCH_AND_ADD
  num = __sync_fetch_and_add (&next_available_newunit, -1);
#else
  __gthread_mutex_lock (&unit_lock);
  num = next_available_newunit--;
  __gthread_mutex_unlock (&unit_lock);
#endif
  /* Do not allow NEWUNIT numbers to wrap.  */
  if (num > GFC_FIRST_NEWUNIT)
    {
      generate_error (common, LIBERROR_INTERNAL, "NEWUNIT exhausted");
      return 0;
    }
  return num;
}
