/* An expandable hash tables datatype.  
   Copyright (C) 1999 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov (vmakarov@cygnus.com).

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This package implements basic hash table functionality.  It is possible
   to search for an entry, create an entry and destroy an entry.

   Elements in the table are generic pointers.

   The size of the table is not fixed; if the occupancy of the table
   grows too high the hash table will be expanded.

   The abstract data implementation is based on generalized Algorithm D
   from Knuth's book "The art of computer programming".  Hash table is
   expanded by creation of new hash table and transferring elements from
   the old table to the new table. */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#include "libiberty.h"
#include "hashtab.h"

/* This macro defines reserved value for empty table entry. */

#define EMPTY_ENTRY    ((void *) 0)

/* This macro defines reserved value for table entry which contained
   a deleted element. */

#define DELETED_ENTRY  ((void *) 1)

/* The following function returns the nearest prime number which is
   greater than given source number. */

static unsigned long
higher_prime_number (n)
     unsigned long n;
{
  unsigned long i;

  n |= 0x01;  /* Force N to be odd.  */
  if (n < 9)
    return n; /* All odd numbers < 9 are prime.  */

 next:
  n += 2;
  i = 3;
  do
    {
      if (n % i == 0)
	goto next;
      i += 2;
    }
  while ((i * i) <= n);

  return n;
}

/* This function creates table with length slightly longer than given
   source length.  Created hash table is initiated as empty (all the
   hash table entries are EMPTY_ENTRY).  The function returns the
   created hash table. */

htab_t
htab_create (size, hash_f, eq_f, del_f)
     size_t size;
     htab_hash hash_f;
     htab_eq eq_f;
     htab_del del_f;
{
  htab_t result;

  size = higher_prime_number (size);
  result = (htab_t) xcalloc (1, sizeof (struct htab));
  result->entries = (void **) xcalloc (size, sizeof (void *));
  result->size = size;
  result->hash_f = hash_f;
  result->eq_f = eq_f;
  result->del_f = del_f;
  return result;
}

/* This function frees all memory allocated for given hash table.
   Naturally the hash table must already exist. */

void
htab_delete (htab)
     htab_t htab;
{
  int i;
  if (htab->del_f)
    for (i = htab->size - 1; i >= 0; i--)
      {
	if (htab->entries[i] != EMPTY_ENTRY
	    && htab->entries[i] != DELETED_ENTRY)
	  (*htab->del_f) (htab->entries[i]);
      }

  free (htab->entries);
  free (htab);
}

/* This function clears all entries in the given hash table.  */

void
htab_empty (htab)
     htab_t htab;
{
  int i;
  if (htab->del_f)
    for (i = htab->size - 1; i >= 0; i--)
      {
	if (htab->entries[i] != EMPTY_ENTRY
	    && htab->entries[i] != DELETED_ENTRY)
	  (*htab->del_f) (htab->entries[i]);
      }

  memset (htab->entries, 0, htab->size * sizeof (void *));
}

/* Similar to htab_find_slot, but without several unwanted side effects:
    - Does not call htab->eq_f when it finds an existing entry.
    - Does not change the count of elements/searches/collisions in the
      hash table.
   This function also assumes there are no deleted entries in the table.
   HASH is the hash value for the element to be inserted.  */
static void **
find_empty_slot_for_expand (htab, hash)
     htab_t htab;
     unsigned int hash;
{
  size_t size = htab->size;
  unsigned int hash2 = 1 + hash % (size - 2);
  unsigned int index = hash % size;

  for (;;)
    {
      void **slot = htab->entries + index;
      if (*slot == EMPTY_ENTRY)
	return slot;

      if (*slot == DELETED_ENTRY)
	abort ();

      index += hash2;
      if (index >= size)
	index -= size;
    }
}

/* The following function changes size of memory allocated for the
   entries and repeatedly inserts the table elements.  The occupancy
   of the table after the call will be about 50%.  Naturally the hash
   table must already exist.  Remember also that the place of the
   table entries is changed. */

static void
htab_expand (htab)
     htab_t htab;
{
  void **oentries;
  void **olimit;
  void **p;

  oentries = htab->entries;
  olimit = oentries + htab->size;

  htab->size = higher_prime_number (htab->size * 2);
  htab->entries = xcalloc (htab->size, sizeof (void **));

  htab->n_elements -= htab->n_deleted;
  htab->n_deleted = 0;

  p = oentries;
  do
    {
      void *x = *p;
      if (x != EMPTY_ENTRY && x != DELETED_ENTRY)
	{
	  void **q = find_empty_slot_for_expand (htab, (*htab->hash_f) (x));
	  *q = x;
	}
      p++;
    }
  while (p < olimit);
  free (oentries);
}

/* This function searches for a hash table entry equal to the given
   element.  It cannot be used to insert or delete an element.  */

void *
htab_find_with_hash (htab, element, hash)
     htab_t htab;
     const void *element;
     unsigned int hash;
{
  unsigned int index, hash2;
  size_t size;

  htab->searches++;
  size = htab->size;
  hash2 = 1 + hash % (size - 2);
  index = hash % size;

  for (;;)
    {
      void *entry = htab->entries[index];
      if (entry == EMPTY_ENTRY)
	return NULL;
      else if (entry != DELETED_ENTRY && (*htab->eq_f) (entry, element))
	return entry;

      htab->collisions++;
      index += hash2;
      if (index >= size)
	index -= size;
    }
}

/* Like htab_find_slot_with_hash, but compute the hash value from the
   element.  */
void *
htab_find (htab, element)
     htab_t htab;
     const void *element;
{
  return htab_find_with_hash (htab, element, (*htab->hash_f) (element));
}

/* This function searches for a hash table slot containing an entry
   equal to the given element.  To delete an entry, call this with
   INSERT = 0, then call htab_clear_slot on the slot returned (possibly
   after doing some checks).  To insert an entry, call this with
   INSERT = 1, then write the value you want into the returned slot.  */

void **
htab_find_slot_with_hash (htab, element, hash, insert)
     htab_t htab;
     const void *element;
     unsigned int hash;
     int insert;
{
  void **first_deleted_slot;
  unsigned int index, hash2;
  size_t size;

  if (insert && htab->size * 3 <= htab->n_elements * 4)
    htab_expand (htab);

  size = htab->size;
  hash2 = 1 + hash % (size - 2);
  index = hash % size;

  htab->searches++;
  first_deleted_slot = NULL;

  for (;;)
    {
      void *entry = htab->entries[index];
      if (entry == EMPTY_ENTRY)
	{
	  if (!insert)
	    return NULL;

	  htab->n_elements++;

	  if (first_deleted_slot)
	    {
	      *first_deleted_slot = EMPTY_ENTRY;
	      return first_deleted_slot;
	    }

	  return &htab->entries[index];
	}

      if (entry == DELETED_ENTRY)
	{
	  if (!first_deleted_slot)
	    first_deleted_slot = &htab->entries[index];
	}
      else
	{
	  if ((*htab->eq_f) (entry, element))
	    return &htab->entries[index];
	}
      
      htab->collisions++;
      index += hash2;
      if (index >= size)
	index -= size;
    }
}

/* Like htab_find_slot_with_hash, but compute the hash value from the
   element.  */
void **
htab_find_slot (htab, element, insert)
     htab_t htab;
     const void *element;
     int insert;
{
  return htab_find_slot_with_hash (htab, element, (*htab->hash_f) (element),
				   insert);
}

/* This function deletes an element with the given value from hash
   table.  If there is no matching element in the hash table, this
   function does nothing.  */

void
htab_remove_elt (htab, element)
     htab_t htab;
     void *element;
{
  void **slot;

  slot = htab_find_slot (htab, element, 0);
  if (*slot == EMPTY_ENTRY)
    return;

  if (htab->del_f)
    (*htab->del_f) (*slot);

  *slot = DELETED_ENTRY;
  htab->n_deleted++;
}

/* This function clears a specified slot in a hash table.  It is
   useful when you've already done the lookup and don't want to do it
   again.  */

void
htab_clear_slot (htab, slot)
     htab_t htab;
     void **slot;
{
  if (slot < htab->entries || slot >= htab->entries + htab->size
      || *slot == EMPTY_ENTRY || *slot == DELETED_ENTRY)
    abort ();
  if (htab->del_f)
    (*htab->del_f) (*slot);
  *slot = DELETED_ENTRY;
  htab->n_deleted++;
}

/* This function scans over the entire hash table calling
   CALLBACK for each live entry.  If CALLBACK returns false,
   the iteration stops.  INFO is passed as CALLBACK's second
   argument.  */

void
htab_traverse (htab, callback, info)
     htab_t htab;
     htab_trav callback;
     void *info;
{
  void **slot, **limit;
  slot = htab->entries;
  limit = slot + htab->size;
  do
    {
      void *x = *slot;
      if (x != EMPTY_ENTRY && x != DELETED_ENTRY)
	if (!(*callback) (slot, info))
	  break;
    }
  while (++slot < limit);
}

/* The following function returns current size of given hash table. */

size_t
htab_size (htab)
     htab_t htab;
{
  return htab->size;
}

/* The following function returns current number of elements in given
   hash table. */

size_t
htab_elements (htab)
     htab_t htab;
{
  return htab->n_elements - htab->n_deleted;
}

/* The following function returns number of percents of fixed
   collisions during all work with given hash table. */

double
htab_collisions (htab)
     htab_t htab;
{
  int searches;

  searches = htab->searches;
  if (searches == 0)
    return 0.0;
  return (double)htab->collisions / (double)searches;
}
