/* Copyright (C) 2002-2003 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <stdlib.h>
#include <string.h>
#include "libgfortran.h"
#include "io.h"


/* Subroutines related to units */


#define CACHE_SIZE 3
static unit_t internal_unit, *unit_cache[CACHE_SIZE];


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

static unit_t *
rotate_left (unit_t * t)
{
  unit_t *temp;

  temp = t->right;
  t->right = t->right->left;
  temp->left = t;

  return temp;
}


/* rotate_right()-- Rotate the treap right */

static unit_t *
rotate_right (unit_t * t)
{
  unit_t *temp;

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

static unit_t *
insert (unit_t * new, unit_t * t)
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
    internal_error ("insert(): Duplicate key found!");

  return t;
}


/* insert_unit()-- Given a new node, insert it into the treap.  It is
 * an error to insert a key that already exists. */

void
insert_unit (unit_t * new)
{

  new->priority = pseudo_random ();
  g.unit_root = insert (new, g.unit_root);
}


static unit_t *
delete_root (unit_t * t)
{
  unit_t *temp;

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

static unit_t *
delete_treap (unit_t * old, unit_t * t)
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
delete_unit (unit_t * old)
{

  g.unit_root = delete_treap (old, g.unit_root);
}


/* find_unit()-- Given an integer, return a pointer to the unit
 * structure.  Returns NULL if the unit does not exist. */

unit_t *
find_unit (int n)
{
  unit_t *p;
  int c;

  for (c = 0; c < CACHE_SIZE; c++)
    if (unit_cache[c] != NULL && unit_cache[c]->unit_number == n)
      {
	p = unit_cache[c];
	return p;
      }

  p = g.unit_root;
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

  if (p != NULL)
    {
      for (c = 0; c < CACHE_SIZE - 1; c++)
	unit_cache[c] = unit_cache[c + 1];

      unit_cache[CACHE_SIZE - 1] = p;
    }

  return p;
}

/* get_unit()-- Returns the unit structure associated with the integer
 * unit or the internal file. */

unit_t *
get_unit (int read_flag)
{
  unit_t *u;

  if (ioparm.internal_unit != NULL)
    {
      internal_unit.s =
	open_internal (ioparm.internal_unit, ioparm.internal_unit_len);

      /* Set flags for the internal unit */

      internal_unit.flags.access = ACCESS_SEQUENTIAL;
      internal_unit.flags.action = ACTION_READWRITE;
      internal_unit.flags.form = FORM_FORMATTED;
      internal_unit.flags.delim = DELIM_NONE;

      return &internal_unit;
    }

  /* Has to be an external unit */

  u = find_unit (ioparm.unit);
  if (u != NULL)
    return u;

  return NULL;
}


/* is_internal_unit()-- Determine if the current unit is internal or
 * not */

int
is_internal_unit ()
{

  return current_unit == &internal_unit;
}



/*************************/
/* Initialize everything */

void
init_units (void)
{
  offset_t m, n;
  unit_t *u;
  int i;

  if (options.stdin_unit >= 0)
    {				/* STDIN */
      u = get_mem (sizeof (unit_t));

      u->unit_number = options.stdin_unit;
      u->s = input_stream ();

      u->flags.action = ACTION_READ;

      u->flags.access = ACCESS_SEQUENTIAL;
      u->flags.form = FORM_FORMATTED;
      u->flags.status = STATUS_OLD;
      u->flags.blank = BLANK_ZERO;
      u->flags.position = POSITION_ASIS;

      u->recl = options.default_recl;
      u->endfile = NO_ENDFILE;

      insert_unit (u);
    }

  if (options.stdout_unit >= 0)
    {				/* STDOUT */
      u = get_mem (sizeof (unit_t));

      u->unit_number = options.stdout_unit;
      u->s = output_stream ();

      u->flags.action = ACTION_WRITE;

      u->flags.access = ACCESS_SEQUENTIAL;
      u->flags.form = FORM_FORMATTED;
      u->flags.status = STATUS_OLD;
      u->flags.blank = BLANK_ZERO;
      u->flags.position = POSITION_ASIS;

      u->recl = options.default_recl;
      u->endfile = AT_ENDFILE;

      insert_unit (u);
    }

  /* Calculate the maximum file offset in a portable manner.
   * max will be the largest signed number for the type offset_t.
   *
   * set a 1 in the LSB and keep a running sum, stopping at MSB-1 bit. */

  g.max_offset = 0;
  for (i=0; i < sizeof(g.max_offset) * 8 - 1; i++)
    g.max_offset = g.max_offset + ((offset_t) 1 << i);

}


/* close_unit()-- Close a unit.  The stream is closed, and any memory
 * associated with the stream is freed.  Returns nonzero on I/O error. */

int
close_unit (unit_t * u)
{
  int i, rc;

  for (i = 0; i < CACHE_SIZE; i++)
    if (unit_cache[i] == u)
      unit_cache[i] = NULL;

  rc = (u->s == NULL) ? 0 : sclose (u->s) == FAILURE;

  delete_unit (u);
  free_mem (u);

  return rc;
}


/* close_units()-- Delete units on completion.  We just keep deleting
 * the root of the treap until there is nothing left. */

void
close_units (void)
{

  while (g.unit_root != NULL)
    close_unit (g.unit_root);
}
