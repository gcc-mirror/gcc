/* An expandable hash tables datatype.  
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <stdio.h>

#include "libiberty.h"
#include "hashtab.h"

/* This macro defines reserved value for empty table entry. */

#define EMPTY_ENTRY    ((PTR) 0)

/* This macro defines reserved value for table entry which contained
   a deleted element. */

#define DELETED_ENTRY  ((PTR) 1)

static unsigned long higher_prime_number PARAMS ((unsigned long));
static hashval_t hash_pointer PARAMS ((const void *));
static int eq_pointer PARAMS ((const void *, const void *));
static void htab_expand PARAMS ((htab_t));
static PTR *find_empty_slot_for_expand  PARAMS ((htab_t, hashval_t));

/* At some point, we could make these be NULL, and modify the
   hash-table routines to handle NULL specially; that would avoid
   function-call overhead for the common case of hashing pointers.  */
htab_hash htab_hash_pointer = hash_pointer;
htab_eq htab_eq_pointer = eq_pointer;

/* The following function returns the nearest prime number which is
   greater than a given source number, N. */

static unsigned long
higher_prime_number (n)
     unsigned long n;
{
  unsigned long i;

  /* Ensure we have a larger number and then force to odd.  */
  n++;  
  n |= 0x01; 

  /* All odd numbers < 9 are prime.  */
  if (n < 9)
    return n;

  /* Otherwise find the next prime using a sieve.  */

 next:

  for (i = 3; i * i <= n; i += 2)
    if (n % i == 0)
      {
	 n += 2;
	 goto next;
       }

  return n;
}

/* Returns a hash code for P.  */

static hashval_t
hash_pointer (p)
     const PTR p;
{
  return (hashval_t) ((long)p >> 3);
}

/* Returns non-zero if P1 and P2 are equal.  */

static int
eq_pointer (p1, p2)
     const PTR p1;
     const PTR p2;
{
  return p1 == p2;
}

/* This function creates table with length slightly longer than given
   source length.  Created hash table is initiated as empty (all the
   hash table entries are EMPTY_ENTRY).  The function returns the
   created hash table.  */

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
  result->entries = (PTR *) xcalloc (size, sizeof (PTR));
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
      if (htab->entries[i] != EMPTY_ENTRY
	  && htab->entries[i] != DELETED_ENTRY)
	(*htab->del_f) (htab->entries[i]);

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
      if (htab->entries[i] != EMPTY_ENTRY
	  && htab->entries[i] != DELETED_ENTRY)
	(*htab->del_f) (htab->entries[i]);

  memset (htab->entries, 0, htab->size * sizeof (PTR));
}

/* Similar to htab_find_slot, but without several unwanted side effects:
    - Does not call htab->eq_f when it finds an existing entry.
    - Does not change the count of elements/searches/collisions in the
      hash table.
   This function also assumes there are no deleted entries in the table.
   HASH is the hash value for the element to be inserted.  */

static PTR *
find_empty_slot_for_expand (htab, hash)
     htab_t htab;
     hashval_t hash;
{
  size_t size = htab->size;
  hashval_t hash2 = 1 + hash % (size - 2);
  unsigned int index = hash % size;

  for (;;)
    {
      PTR *slot = htab->entries + index;

      if (*slot == EMPTY_ENTRY)
	return slot;
      else if (*slot == DELETED_ENTRY)
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
   table entries is changed.  */

static void
htab_expand (htab)
     htab_t htab;
{
  PTR *oentries;
  PTR *olimit;
  PTR *p;

  oentries = htab->entries;
  olimit = oentries + htab->size;

  htab->size = higher_prime_number (htab->size * 2);
  htab->entries = (PTR *) xcalloc (htab->size, sizeof (PTR *));

  htab->n_elements -= htab->n_deleted;
  htab->n_deleted = 0;

  p = oentries;
  do
    {
      PTR x = *p;

      if (x != EMPTY_ENTRY && x != DELETED_ENTRY)
	{
	  PTR *q = find_empty_slot_for_expand (htab, (*htab->hash_f) (x));

	  *q = x;
	}

      p++;
    }
  while (p < olimit);

  free (oentries);
}

/* This function searches for a hash table entry equal to the given
   element.  It cannot be used to insert or delete an element.  */

PTR
htab_find_with_hash (htab, element, hash)
     htab_t htab;
     const PTR element;
     hashval_t hash;
{
  unsigned int index;
  hashval_t hash2;
  size_t size;
  PTR entry;

  htab->searches++;
  size = htab->size;
  index = hash % size;

  entry = htab->entries[index];
  if (entry == EMPTY_ENTRY
      || (entry != DELETED_ENTRY && (*htab->eq_f) (entry, element)))
    return entry;

  hash2 = 1 + hash % (size - 2);

  for (;;)
    {
      htab->collisions++;
      index += hash2;
      if (index >= size)
	index -= size;

      entry = htab->entries[index];
      if (entry == EMPTY_ENTRY
	  || (entry != DELETED_ENTRY && (*htab->eq_f) (entry, element)))
	return entry;
    }
}

/* Like htab_find_slot_with_hash, but compute the hash value from the
   element.  */

PTR
htab_find (htab, element)
     htab_t htab;
     const PTR element;
{
  return htab_find_with_hash (htab, element, (*htab->hash_f) (element));
}

/* This function searches for a hash table slot containing an entry
   equal to the given element.  To delete an entry, call this with
   INSERT = 0, then call htab_clear_slot on the slot returned (possibly
   after doing some checks).  To insert an entry, call this with
   INSERT = 1, then write the value you want into the returned slot.  */

PTR *
htab_find_slot_with_hash (htab, element, hash, insert)
     htab_t htab;
     const PTR element;
     hashval_t hash;
     enum insert_option insert;
{
  PTR *first_deleted_slot;
  unsigned int index;
  hashval_t hash2;
  size_t size;

  if (insert == INSERT && htab->size * 3 <= htab->n_elements * 4)
    htab_expand (htab);

  size = htab->size;
  hash2 = 1 + hash % (size - 2);
  index = hash % size;

  htab->searches++;
  first_deleted_slot = NULL;

  for (;;)
    {
      PTR entry = htab->entries[index];
      if (entry == EMPTY_ENTRY)
	{
	  if (insert == NO_INSERT)
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
      else  if ((*htab->eq_f) (entry, element))
	return &htab->entries[index];
      
      htab->collisions++;
      index += hash2;
      if (index >= size)
	index -= size;
    }
}

/* Like htab_find_slot_with_hash, but compute the hash value from the
   element.  */

PTR *
htab_find_slot (htab, element, insert)
     htab_t htab;
     const PTR element;
     enum insert_option insert;
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
     PTR element;
{
  PTR *slot;

  slot = htab_find_slot (htab, element, NO_INSERT);
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
     PTR *slot;
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
     PTR info;
{
  PTR *slot = htab->entries;
  PTR *limit = slot + htab->size;

  do
    {
      PTR x = *slot;

      if (x != EMPTY_ENTRY && x != DELETED_ENTRY)
	if (!(*callback) (slot, info))
	  break;
    }
  while (++slot < limit);
}

/* Return the current size of given hash table. */

size_t
htab_size (htab)
     htab_t htab;
{
  return htab->size;
}

/* Return the current number of elements in given hash table. */

size_t
htab_elements (htab)
     htab_t htab;
{
  return htab->n_elements - htab->n_deleted;
}

/* Return the fraction of fixed collisions during all work with given
   hash table. */

double
htab_collisions (htab)
     htab_t htab;
{
  if (htab->searches == 0)
    return 0.0;

  return (double) htab->collisions / (double) htab->searches;
}
