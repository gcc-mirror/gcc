/* An expandable hash tables datatype.
   Copyright (C) 1999-2015 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@cygnus.com>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

/* The hash table code copied from include/hashtab.[hc] and adjusted,
   so that the hash table entries are in the flexible array at the end
   of the control structure, no callbacks are used and the elements in the
   table are of the hash_entry_type type.
   Before including this file, define hash_entry_type type and
   htab_alloc and htab_free functions.  After including it, define
   htab_hash and htab_eq inline functions.   */

/* This package implements basic hash table functionality.  It is possible
   to search for an entry, create an entry and destroy an entry.

   Elements in the table are generic pointers.

   The size of the table is not fixed; if the occupancy of the table
   grows too high the hash table will be expanded.

   The abstract data implementation is based on generalized Algorithm D
   from Knuth's book "The art of computer programming".  Hash table is
   expanded by creation of new hash table and transferring elements from
   the old table to the new table.  */

/* The type for a hash code.  */
typedef unsigned int hashval_t;

static inline hashval_t htab_hash (hash_entry_type);
static inline bool htab_eq (hash_entry_type, hash_entry_type);

/* This macro defines reserved value for empty table entry.  */

#define HTAB_EMPTY_ENTRY    ((hash_entry_type) 0)

/* This macro defines reserved value for table entry which contained
   a deleted element. */

#define HTAB_DELETED_ENTRY  ((hash_entry_type) 1)

/* Hash tables are of the following type.  The structure
   (implementation) of this type is not needed for using the hash
   tables.  All work with hash table should be executed only through
   functions mentioned below.  The size of this structure is subject to
   change.  */

struct htab {
  /* Current size (in entries) of the hash table.  */
  size_t size;

  /* Current number of elements including also deleted elements.  */
  size_t n_elements;

  /* Current number of deleted elements in the table.  */
  size_t n_deleted;

  /* Current size (in entries) of the hash table, as an index into the
     table of primes.  */
  unsigned int size_prime_index;

  /* Table itself.  */
  hash_entry_type entries[];
};

typedef struct htab *htab_t;

/* An enum saying whether we insert into the hash table or not.  */
enum insert_option {NO_INSERT, INSERT};

/* Table of primes and multiplicative inverses.

   Note that these are not minimally reduced inverses.  Unlike when generating
   code to divide by a constant, we want to be able to use the same algorithm
   all the time.  All of these inverses (are implied to) have bit 32 set.

   For the record, the function that computed the table is in
   libiberty/hashtab.c.  */

struct prime_ent
{
  hashval_t prime;
  hashval_t inv;
  hashval_t inv_m2;	/* inverse of prime-2 */
  hashval_t shift;
};

static struct prime_ent const prime_tab[] = {
  {          7, 0x24924925, 0x9999999b, 2 },
  {         13, 0x3b13b13c, 0x745d1747, 3 },
  {         31, 0x08421085, 0x1a7b9612, 4 },
  {         61, 0x0c9714fc, 0x15b1e5f8, 5 },
  {        127, 0x02040811, 0x0624dd30, 6 },
  {        251, 0x05197f7e, 0x073260a5, 7 },
  {        509, 0x01824366, 0x02864fc8, 8 },
  {       1021, 0x00c0906d, 0x014191f7, 9 },
  {       2039, 0x0121456f, 0x0161e69e, 10 },
  {       4093, 0x00300902, 0x00501908, 11 },
  {       8191, 0x00080041, 0x00180241, 12 },
  {      16381, 0x000c0091, 0x00140191, 13 },
  {      32749, 0x002605a5, 0x002a06e6, 14 },
  {      65521, 0x000f00e2, 0x00110122, 15 },
  {     131071, 0x00008001, 0x00018003, 16 },
  {     262139, 0x00014002, 0x0001c004, 17 },
  {     524287, 0x00002001, 0x00006001, 18 },
  {    1048573, 0x00003001, 0x00005001, 19 },
  {    2097143, 0x00004801, 0x00005801, 20 },
  {    4194301, 0x00000c01, 0x00001401, 21 },
  {    8388593, 0x00001e01, 0x00002201, 22 },
  {   16777213, 0x00000301, 0x00000501, 23 },
  {   33554393, 0x00001381, 0x00001481, 24 },
  {   67108859, 0x00000141, 0x000001c1, 25 },
  {  134217689, 0x000004e1, 0x00000521, 26 },
  {  268435399, 0x00000391, 0x000003b1, 27 },
  {  536870909, 0x00000019, 0x00000029, 28 },
  { 1073741789, 0x0000008d, 0x00000095, 29 },
  { 2147483647, 0x00000003, 0x00000007, 30 },
  /* Avoid "decimal constant so large it is unsigned" for 4294967291.  */
  { 0xfffffffb, 0x00000006, 0x00000008, 31 }
};

/* The following function returns an index into the above table of the
   nearest prime number which is greater than N, and near a power of two. */

static unsigned int
higher_prime_index (unsigned long n)
{
  unsigned int low = 0;
  unsigned int high = sizeof(prime_tab) / sizeof(prime_tab[0]);

  while (low != high)
    {
      unsigned int mid = low + (high - low) / 2;
      if (n > prime_tab[mid].prime)
	low = mid + 1;
      else
	high = mid;
    }

  /* If we've run out of primes, abort.  */
  if (n > prime_tab[low].prime)
    abort ();

  return low;
}

/* Return the current size of given hash table.  */

static inline size_t
htab_size (htab_t htab)
{
  return htab->size;
}

/* Return the current number of elements in given hash table. */

static inline size_t
htab_elements (htab_t htab)
{
  return htab->n_elements - htab->n_deleted;
}

/* Return X % Y.  */

static inline hashval_t
htab_mod_1 (hashval_t x, hashval_t y, hashval_t inv, int shift)
{
  /* The multiplicative inverses computed above are for 32-bit types, and
     requires that we be able to compute a highpart multiply.  */
  if (sizeof (hashval_t) * __CHAR_BIT__ <= 32)
    {
      hashval_t t1, t2, t3, t4, q, r;

      t1 = ((unsigned long long)x * inv) >> 32;
      t2 = x - t1;
      t3 = t2 >> 1;
      t4 = t1 + t3;
      q  = t4 >> shift;
      r  = x - (q * y);

      return r;
    }

  /* Otherwise just use the native division routines.  */
  return x % y;
}

/* Compute the primary hash for HASH given HTAB's current size.  */

static inline hashval_t
htab_mod (hashval_t hash, htab_t htab)
{
  const struct prime_ent *p = &prime_tab[htab->size_prime_index];
  return htab_mod_1 (hash, p->prime, p->inv, p->shift);
}

/* Compute the secondary hash for HASH given HTAB's current size.  */

static inline hashval_t
htab_mod_m2 (hashval_t hash, htab_t htab)
{
  const struct prime_ent *p = &prime_tab[htab->size_prime_index];
  return 1 + htab_mod_1 (hash, p->prime - 2, p->inv_m2, p->shift);
}

/* Create hash table of size SIZE.  */

static htab_t
htab_create (size_t size)
{
  htab_t result;
  unsigned int size_prime_index;

  size_prime_index = higher_prime_index (size);
  size = prime_tab[size_prime_index].prime;

  result = (htab_t) htab_alloc (sizeof (struct htab)
				+ size * sizeof (hash_entry_type));
  result->size = size;
  result->n_elements = 0;
  result->n_deleted = 0;
  result->size_prime_index = size_prime_index;
  memset (result->entries, 0, size * sizeof (hash_entry_type));
  return result;
}

/* Similar to htab_find_slot, but without several unwanted side effects:
    - Does not call htab_eq when it finds an existing entry.
    - Does not change the count of elements in the hash table.
   This function also assumes there are no deleted entries in the table.
   HASH is the hash value for the element to be inserted.  */

static hash_entry_type *
find_empty_slot_for_expand (htab_t htab, hashval_t hash)
{
  hashval_t index = htab_mod (hash, htab);
  size_t size = htab_size (htab);
  hash_entry_type *slot = htab->entries + index;
  hashval_t hash2;

  if (*slot == HTAB_EMPTY_ENTRY)
    return slot;
  else if (*slot == HTAB_DELETED_ENTRY)
    abort ();

  hash2 = htab_mod_m2 (hash, htab);
  for (;;)
    {
      index += hash2;
      if (index >= size)
	index -= size;

      slot = htab->entries + index;
      if (*slot == HTAB_EMPTY_ENTRY)
	return slot;
      else if (*slot == HTAB_DELETED_ENTRY)
	abort ();
    }
}

/* The following function changes size of memory allocated for the
   entries and repeatedly inserts the table elements.  The occupancy
   of the table after the call will be about 50%.  Naturally the hash
   table must already exist.  Remember also that the place of the
   table entries is changed.  */

static htab_t
htab_expand (htab_t htab)
{
  htab_t nhtab;
  hash_entry_type *olimit;
  hash_entry_type *p;
  size_t osize, elts;

  osize = htab->size;
  olimit = htab->entries + osize;
  elts = htab_elements (htab);

  /* Resize only when table after removal of unused elements is either
     too full or too empty.  */
  if (elts * 2 > osize || (elts * 8 < osize && osize > 32))
    nhtab = htab_create (elts * 2);
  else
    nhtab = htab_create (osize - 1);
  nhtab->n_elements = htab->n_elements - htab->n_deleted;

  p = htab->entries;
  do
    {
      hash_entry_type x = *p;

      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
	*find_empty_slot_for_expand (nhtab, htab_hash (x)) = x;

      p++;
    }
  while (p < olimit);

  htab_free (htab);
  return nhtab;
}

/* This function searches for a hash table entry equal to the given
   element.  It cannot be used to insert or delete an element.  */

static hash_entry_type
htab_find (htab_t htab, const hash_entry_type element)
{
  hashval_t index, hash2, hash = htab_hash (element);
  size_t size;
  hash_entry_type entry;

  size = htab_size (htab);
  index = htab_mod (hash, htab);

  entry = htab->entries[index];
  if (entry == HTAB_EMPTY_ENTRY
      || (entry != HTAB_DELETED_ENTRY && htab_eq (entry, element)))
    return entry;

  hash2 = htab_mod_m2 (hash, htab);
  for (;;)
    {
      index += hash2;
      if (index >= size)
	index -= size;

      entry = htab->entries[index];
      if (entry == HTAB_EMPTY_ENTRY
	  || (entry != HTAB_DELETED_ENTRY && htab_eq (entry, element)))
	return entry;
    }
}

/* This function searches for a hash table slot containing an entry
   equal to the given element.  To delete an entry, call this with
   insert=NO_INSERT, then call htab_clear_slot on the slot returned
   (possibly after doing some checks).  To insert an entry, call this
   with insert=INSERT, then write the value you want into the returned
   slot.  */

static hash_entry_type *
htab_find_slot (htab_t *htabp, const hash_entry_type element,
		enum insert_option insert)
{
  hash_entry_type *first_deleted_slot;
  hashval_t index, hash2, hash = htab_hash (element);
  size_t size;
  hash_entry_type entry;
  htab_t htab = *htabp;

  size = htab_size (htab);
  if (insert == INSERT && size * 3 <= htab->n_elements * 4)
    {
      htab = *htabp = htab_expand (htab);
      size = htab_size (htab);
    }

  index = htab_mod (hash, htab);

  first_deleted_slot = NULL;

  entry = htab->entries[index];
  if (entry == HTAB_EMPTY_ENTRY)
    goto empty_entry;
  else if (entry == HTAB_DELETED_ENTRY)
    first_deleted_slot = &htab->entries[index];
  else if (htab_eq (entry, element))
    return &htab->entries[index];

  hash2 = htab_mod_m2 (hash, htab);
  for (;;)
    {
      index += hash2;
      if (index >= size)
	index -= size;

      entry = htab->entries[index];
      if (entry == HTAB_EMPTY_ENTRY)
	goto empty_entry;
      else if (entry == HTAB_DELETED_ENTRY)
	{
	  if (!first_deleted_slot)
	    first_deleted_slot = &htab->entries[index];
	}
      else if (htab_eq (entry, element))
	return &htab->entries[index];
    }

 empty_entry:
  if (insert == NO_INSERT)
    return NULL;

  if (first_deleted_slot)
    {
      htab->n_deleted--;
      *first_deleted_slot = HTAB_EMPTY_ENTRY;
      return first_deleted_slot;
    }

  htab->n_elements++;
  return &htab->entries[index];
}

/* This function clears a specified slot in a hash table.  It is
   useful when you've already done the lookup and don't want to do it
   again.  */

static inline void
htab_clear_slot (htab_t htab, hash_entry_type *slot)
{
  if (slot < htab->entries || slot >= htab->entries + htab_size (htab)
      || *slot == HTAB_EMPTY_ENTRY || *slot == HTAB_DELETED_ENTRY)
    abort ();

  *slot = HTAB_DELETED_ENTRY;
  htab->n_deleted++;
}

/* Returns a hash code for pointer P. Simplified version of evahash */

static inline hashval_t
hash_pointer (const void *p)
{
  uintptr_t v = (uintptr_t) p;
  if (sizeof (v) > sizeof (hashval_t))
    v ^= v >> (sizeof (uintptr_t) / 2 * __CHAR_BIT__);
  return v;
}
