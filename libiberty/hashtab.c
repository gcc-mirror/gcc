/* An expandable hash tables datatype.  
   Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

#ifdef HAVE_MALLOC_H
#include <malloc.h>
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
static int htab_expand PARAMS ((htab_t));
static PTR *find_empty_slot_for_expand  PARAMS ((htab_t, hashval_t));

/* At some point, we could make these be NULL, and modify the
   hash-table routines to handle NULL specially; that would avoid
   function-call overhead for the common case of hashing pointers.  */
htab_hash htab_hash_pointer = hash_pointer;
htab_eq htab_eq_pointer = eq_pointer;

/* The following function returns a nearest prime number which is
   greater than N, and near a power of two. */

static unsigned long
higher_prime_number (n)
     unsigned long n;
{
  /* These are primes that are near, but slightly smaller than, a
     power of two.  */
  static const unsigned long primes[] = {
    (unsigned long) 7,
    (unsigned long) 13,
    (unsigned long) 31,
    (unsigned long) 61,
    (unsigned long) 127,
    (unsigned long) 251,
    (unsigned long) 509,
    (unsigned long) 1021,
    (unsigned long) 2039,
    (unsigned long) 4093,
    (unsigned long) 8191,
    (unsigned long) 16381,
    (unsigned long) 32749,
    (unsigned long) 65521,
    (unsigned long) 131071,
    (unsigned long) 262139,
    (unsigned long) 524287,
    (unsigned long) 1048573,
    (unsigned long) 2097143,
    (unsigned long) 4194301,
    (unsigned long) 8388593,
    (unsigned long) 16777213,
    (unsigned long) 33554393,
    (unsigned long) 67108859,
    (unsigned long) 134217689,
    (unsigned long) 268435399,
    (unsigned long) 536870909,
    (unsigned long) 1073741789,
    (unsigned long) 2147483647,
					/* 4294967291L */
    ((unsigned long) 2147483647) + ((unsigned long) 2147483644),
  };

  const unsigned long *low = &primes[0];
  const unsigned long *high = &primes[sizeof(primes) / sizeof(primes[0])];

  while (low != high)
    {
      const unsigned long *mid = low + (high - low) / 2;
      if (n > *mid)
	low = mid + 1;
      else
	high = mid;
    }

  /* If we've run out of primes, abort.  */
  if (n > *low)
    {
      fprintf (stderr, "Cannot find prime bigger than %lu\n", n);
      abort ();
    }

  return *low;
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
   created hash table, or NULL if memory allocation fails.  */

htab_t
htab_create_alloc (size, hash_f, eq_f, del_f, alloc_f, free_f)
     size_t size;
     htab_hash hash_f;
     htab_eq eq_f;
     htab_del del_f;
     htab_alloc alloc_f;
     htab_free free_f;
{
  htab_t result;

  size = higher_prime_number (size);
  result = (htab_t) (*alloc_f) (1, sizeof (struct htab));
  if (result == NULL)
    return NULL;
  result->entries = (PTR *) (*alloc_f) (size, sizeof (PTR));
  if (result->entries == NULL)
    {
      if (free_f != NULL)
	(*free_f) (result);
      return NULL;
    }
  result->size = size;
  result->hash_f = hash_f;
  result->eq_f = eq_f;
  result->del_f = del_f;
  result->alloc_f = alloc_f;
  result->free_f = free_f;
  return result;
}

/* As above, but use the variants of alloc_f and free_f which accept
   an extra argument.  */

htab_t
htab_create_alloc_ex (size, hash_f, eq_f, del_f, alloc_arg, alloc_f,
		      free_f)
     size_t size;
     htab_hash hash_f;
     htab_eq eq_f;
     htab_del del_f;
     PTR alloc_arg;
     htab_alloc_with_arg alloc_f;
     htab_free_with_arg free_f;
{
  htab_t result;

  size = higher_prime_number (size);
  result = (htab_t) (*alloc_f) (alloc_arg, 1, sizeof (struct htab));
  if (result == NULL)
    return NULL;
  result->entries = (PTR *) (*alloc_f) (alloc_arg, size, sizeof (PTR));
  if (result->entries == NULL)
    {
      if (free_f != NULL)
	(*free_f) (alloc_arg, result);
      return NULL;
    }
  result->size = size;
  result->hash_f = hash_f;
  result->eq_f = eq_f;
  result->del_f = del_f;
  result->alloc_arg = alloc_arg;
  result->alloc_with_arg_f = alloc_f;
  result->free_with_arg_f = free_f;
  return result;
}

/* Update the function pointers and allocation parameter in the htab_t.  */

void
htab_set_functions_ex (htab, hash_f, eq_f, del_f, alloc_arg, alloc_f, free_f)
     htab_t htab;
     htab_hash hash_f;
     htab_eq eq_f;
     htab_del del_f;
     PTR alloc_arg;
     htab_alloc_with_arg alloc_f;
     htab_free_with_arg free_f;
{
  htab->hash_f = hash_f;
  htab->eq_f = eq_f;
  htab->del_f = del_f;
  htab->alloc_arg = alloc_arg;
  htab->alloc_with_arg_f = alloc_f;
  htab->free_with_arg_f = free_f;
}

/* These functions exist solely for backward compatibility.  */

#undef htab_create
htab_t
htab_create (size, hash_f, eq_f, del_f)
     size_t size;
     htab_hash hash_f;
     htab_eq eq_f;
     htab_del del_f;
{
  return htab_create_alloc (size, hash_f, eq_f, del_f, xcalloc, free);
}

htab_t
htab_try_create (size, hash_f, eq_f, del_f)
     size_t size;
     htab_hash hash_f;
     htab_eq eq_f;
     htab_del del_f;
{
  return htab_create_alloc (size, hash_f, eq_f, del_f, calloc, free);
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

  if (htab->free_f != NULL)
    {
      (*htab->free_f) (htab->entries);
      (*htab->free_f) (htab);
    }
  else if (htab->free_with_arg_f != NULL)
    {
      (*htab->free_with_arg_f) (htab->alloc_arg, htab->entries);
      (*htab->free_with_arg_f) (htab->alloc_arg, htab);
    }
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
  unsigned int index = hash % size;
  PTR *slot = htab->entries + index;
  hashval_t hash2;

  if (*slot == EMPTY_ENTRY)
    return slot;
  else if (*slot == DELETED_ENTRY)
    abort ();

  hash2 = 1 + hash % (size - 2);
  for (;;)
    {
      index += hash2;
      if (index >= size)
	index -= size;

      slot = htab->entries + index;
      if (*slot == EMPTY_ENTRY)
	return slot;
      else if (*slot == DELETED_ENTRY)
	abort ();
    }
}

/* The following function changes size of memory allocated for the
   entries and repeatedly inserts the table elements.  The occupancy
   of the table after the call will be about 50%.  Naturally the hash
   table must already exist.  Remember also that the place of the
   table entries is changed.  If memory allocation failures are allowed,
   this function will return zero, indicating that the table could not be
   expanded.  If all goes well, it will return a non-zero value.  */

static int
htab_expand (htab)
     htab_t htab;
{
  PTR *oentries;
  PTR *olimit;
  PTR *p;
  PTR *nentries;
  size_t nsize;

  oentries = htab->entries;
  olimit = oentries + htab->size;

  /* Resize only when table after removal of unused elements is either
     too full or too empty.  */
  if ((htab->n_elements - htab->n_deleted) * 2 > htab->size
      || ((htab->n_elements - htab->n_deleted) * 8 < htab->size
	  && htab->size > 32))
    nsize = higher_prime_number ((htab->n_elements - htab->n_deleted) * 2);
  else
    nsize = htab->size;

  if (htab->alloc_with_arg_f != NULL)
    nentries = (PTR *) (*htab->alloc_with_arg_f) (htab->alloc_arg, nsize,
						  sizeof (PTR *));
  else
    nentries = (PTR *) (*htab->alloc_f) (nsize, sizeof (PTR *));
  if (nentries == NULL)
    return 0;
  htab->entries = nentries;
  htab->size = nsize;

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

  if (htab->free_f != NULL)
    (*htab->free_f) (oentries);
  else if (htab->free_with_arg_f != NULL)
    (*htab->free_with_arg_f) (htab->alloc_arg, oentries);
  return 1;
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
   INSERT = 1, then write the value you want into the returned slot.
   When inserting an entry, NULL may be returned if memory allocation
   fails.  */

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
  PTR entry;

  if (insert == INSERT && htab->size * 3 <= htab->n_elements * 4
      && htab_expand (htab) == 0)
    return NULL;

  size = htab->size;
  index = hash % size;

  htab->searches++;
  first_deleted_slot = NULL;

  entry = htab->entries[index];
  if (entry == EMPTY_ENTRY)
    goto empty_entry;
  else if (entry == DELETED_ENTRY)
    first_deleted_slot = &htab->entries[index];
  else if ((*htab->eq_f) (entry, element))
    return &htab->entries[index];
      
  hash2 = 1 + hash % (size - 2);
  for (;;)
    {
      htab->collisions++;
      index += hash2;
      if (index >= size)
	index -= size;
      
      entry = htab->entries[index];
      if (entry == EMPTY_ENTRY)
	goto empty_entry;
      else if (entry == DELETED_ENTRY)
	{
	  if (!first_deleted_slot)
	    first_deleted_slot = &htab->entries[index];
	}
      else if ((*htab->eq_f) (entry, element))
	return &htab->entries[index];
    }

 empty_entry:
  if (insert == NO_INSERT)
    return NULL;

  if (first_deleted_slot)
    {
      htab->n_deleted--;
      *first_deleted_slot = EMPTY_ENTRY;
      return first_deleted_slot;
    }

  htab->n_elements++;
  return &htab->entries[index];
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
htab_traverse_noresize (htab, callback, info)
     htab_t htab;
     htab_trav callback;
     PTR info;
{
  PTR *slot;
  PTR *limit;

  slot = htab->entries;
  limit = slot + htab->size;

  do
    {
      PTR x = *slot;

      if (x != EMPTY_ENTRY && x != DELETED_ENTRY)
	if (!(*callback) (slot, info))
	  break;
    }
  while (++slot < limit);
}

/* Like htab_traverse_noresize, but does resize the table when it is
   too empty to improve effectivity of subsequent calls.  */

void
htab_traverse (htab, callback, info)
     htab_t htab;
     htab_trav callback;
     PTR info;
{
  if ((htab->n_elements - htab->n_deleted) * 8 < htab->size)
    htab_expand (htab);

  htab_traverse_noresize (htab, callback, info);
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

/* Hash P as a null-terminated string.

   Copied from gcc/hashtable.c.  Zack had the following to say with respect
   to applicability, though note that unlike hashtable.c, this hash table
   implementation re-hashes rather than chain buckets.

   http://gcc.gnu.org/ml/gcc-patches/2001-08/msg01021.html
   From: Zack Weinberg <zackw@panix.com>
   Date: Fri, 17 Aug 2001 02:15:56 -0400

   I got it by extracting all the identifiers from all the source code
   I had lying around in mid-1999, and testing many recurrences of
   the form "H_n = H_{n-1} * K + c_n * L + M" where K, L, M were either
   prime numbers or the appropriate identity.  This was the best one.
   I don't remember exactly what constituted "best", except I was
   looking at bucket-length distributions mostly.
   
   So it should be very good at hashing identifiers, but might not be
   as good at arbitrary strings.
   
   I'll add that it thoroughly trounces the hash functions recommended
   for this use at http://burtleburtle.net/bob/hash/index.html, both
   on speed and bucket distribution.  I haven't tried it against the
   function they just started using for Perl's hashes.  */

hashval_t
htab_hash_string (p)
     const PTR p;
{
  const unsigned char *str = (const unsigned char *) p;
  hashval_t r = 0;
  unsigned char c;

  while ((c = *str++) != 0)
    r = r * 67 + c - 113;

  return r;
}

/* DERIVED FROM:
--------------------------------------------------------------------
lookup2.c, by Bob Jenkins, December 1996, Public Domain.
hash(), hash2(), hash3, and mix() are externally useful functions.
Routines to test the hash are included if SELF_TEST is defined.
You can use this free for any purpose.  It has no warranty.
--------------------------------------------------------------------
*/

/*
--------------------------------------------------------------------
mix -- mix 3 32-bit values reversibly.
For every delta with one or two bit set, and the deltas of all three
  high bits or all three low bits, whether the original value of a,b,c
  is almost all zero or is uniformly distributed,
* If mix() is run forward or backward, at least 32 bits in a,b,c
  have at least 1/4 probability of changing.
* If mix() is run forward, every bit of c will change between 1/3 and
  2/3 of the time.  (Well, 22/100 and 78/100 for some 2-bit deltas.)
mix() was built out of 36 single-cycle latency instructions in a 
  structure that could supported 2x parallelism, like so:
      a -= b; 
      a -= c; x = (c>>13);
      b -= c; a ^= x;
      b -= a; x = (a<<8);
      c -= a; b ^= x;
      c -= b; x = (b>>13);
      ...
  Unfortunately, superscalar Pentiums and Sparcs can't take advantage 
  of that parallelism.  They've also turned some of those single-cycle
  latency instructions into multi-cycle latency instructions.  Still,
  this is the fastest good hash I could find.  There were about 2^^68
  to choose from.  I only looked at a billion or so.
--------------------------------------------------------------------
*/
/* same, but slower, works on systems that might have 8 byte hashval_t's */
#define mix(a,b,c) \
{ \
  a -= b; a -= c; a ^= (c>>13); \
  b -= c; b -= a; b ^= (a<< 8); \
  c -= a; c -= b; c ^= ((b&0xffffffff)>>13); \
  a -= b; a -= c; a ^= ((c&0xffffffff)>>12); \
  b -= c; b -= a; b = (b ^ (a<<16)) & 0xffffffff; \
  c -= a; c -= b; c = (c ^ (b>> 5)) & 0xffffffff; \
  a -= b; a -= c; a = (a ^ (c>> 3)) & 0xffffffff; \
  b -= c; b -= a; b = (b ^ (a<<10)) & 0xffffffff; \
  c -= a; c -= b; c = (c ^ (b>>15)) & 0xffffffff; \
}

/*
--------------------------------------------------------------------
hash() -- hash a variable-length key into a 32-bit value
  k     : the key (the unaligned variable-length array of bytes)
  len   : the length of the key, counting by bytes
  level : can be any 4-byte value
Returns a 32-bit value.  Every bit of the key affects every bit of
the return value.  Every 1-bit and 2-bit delta achieves avalanche.
About 36+6len instructions.

The best hash table sizes are powers of 2.  There is no need to do
mod a prime (mod is sooo slow!).  If you need less than 32 bits,
use a bitmask.  For example, if you need only 10 bits, do
  h = (h & hashmask(10));
In which case, the hash table should have hashsize(10) elements.

If you are hashing n strings (ub1 **)k, do it like this:
  for (i=0, h=0; i<n; ++i) h = hash( k[i], len[i], h);

By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net.  You may use this
code any way you wish, private, educational, or commercial.  It's free.

See http://burtleburtle.net/bob/hash/evahash.html
Use for hash table lookup, or anything where one collision in 2^32 is
acceptable.  Do NOT use for cryptographic purposes.
--------------------------------------------------------------------
*/

hashval_t iterative_hash (k_in, length, initval)
     const PTR k_in;               /* the key */
     register size_t  length;      /* the length of the key */
     register hashval_t  initval;  /* the previous hash, or an arbitrary value */
{
  register const unsigned char *k = (const unsigned char *)k_in;
  register hashval_t a,b,c,len;

  /* Set up the internal state */
  len = length;
  a = b = 0x9e3779b9;  /* the golden ratio; an arbitrary value */
  c = initval;           /* the previous hash value */

  /*---------------------------------------- handle most of the key */
#ifndef WORDS_BIGENDIAN
  /* On a little-endian machine, if the data is 4-byte aligned we can hash
     by word for better speed.  This gives nondeterministic results on
     big-endian machines.  */
  if (sizeof (hashval_t) == 4 && (((size_t)k)&3) == 0)
    while (len >= 12)    /* aligned */
      {
	a += *(hashval_t *)(k+0);
	b += *(hashval_t *)(k+4);
	c += *(hashval_t *)(k+8);
	mix(a,b,c);
	k += 12; len -= 12;
      }
  else /* unaligned */
#endif
    while (len >= 12)
      {
	a += (k[0] +((hashval_t)k[1]<<8) +((hashval_t)k[2]<<16) +((hashval_t)k[3]<<24));
	b += (k[4] +((hashval_t)k[5]<<8) +((hashval_t)k[6]<<16) +((hashval_t)k[7]<<24));
	c += (k[8] +((hashval_t)k[9]<<8) +((hashval_t)k[10]<<16)+((hashval_t)k[11]<<24));
	mix(a,b,c);
	k += 12; len -= 12;
      }

  /*------------------------------------- handle the last 11 bytes */
  c += length;
  switch(len)              /* all the case statements fall through */
    {
    case 11: c+=((hashval_t)k[10]<<24);
    case 10: c+=((hashval_t)k[9]<<16);
    case 9 : c+=((hashval_t)k[8]<<8);
      /* the first byte of c is reserved for the length */
    case 8 : b+=((hashval_t)k[7]<<24);
    case 7 : b+=((hashval_t)k[6]<<16);
    case 6 : b+=((hashval_t)k[5]<<8);
    case 5 : b+=k[4];
    case 4 : a+=((hashval_t)k[3]<<24);
    case 3 : a+=((hashval_t)k[2]<<16);
    case 2 : a+=((hashval_t)k[1]<<8);
    case 1 : a+=k[0];
      /* case 0: nothing left to add */
    }
  mix(a,b,c);
  /*-------------------------------------------- report the result */
  return c;
}
