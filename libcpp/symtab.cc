/* Hash tables.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "symtab.h"

/* The code below is a specialization of Vladimir Makarov's expandable
   hash tables (see libiberty/hashtab.c).  The abstraction penalty was
   too high to continue using the generic form.  This code knows
   intrinsically how to calculate a hash value, and how to compare an
   existing entry with a potential new one.  */

static unsigned int calc_hash (const unsigned char *, size_t);
static void ht_expand (cpp_hash_table *);
static double approx_sqrt (double);

/* A deleted entry.  */
#define DELETED ((hashnode) -1)

/* Calculate the hash of the string STR of length LEN.  */

static unsigned int
calc_hash (const unsigned char *str, size_t len)
{
  size_t n = len;
  unsigned int r = 0;

  while (n--)
    r = HT_HASHSTEP (r, *str++);

  return HT_HASHFINISH (r, len);
}

/* Initialize an identifier hashtable.  */

cpp_hash_table *
ht_create (unsigned int order)
{
  unsigned int nslots = 1 << order;
  cpp_hash_table *table;

  table = XCNEW (cpp_hash_table);

  /* Strings need no alignment.  */
  obstack_specify_allocation (&table->stack, 0, 0, xmalloc, free);

  obstack_alignment_mask (&table->stack) = 0;

  table->entries = XCNEWVEC (hashnode, nslots);
  table->entries_owned = true;
  table->nslots = nslots;
  return table;
}

/* Frees all memory associated with a hash table.  */

void
ht_destroy (cpp_hash_table *table)
{
  obstack_free (&table->stack, NULL);
  if (table->entries_owned)
    free (table->entries);
  free (table);
}

/* Returns the hash entry for the a STR of length LEN.  If that string
   already exists in the table, returns the existing entry.  If the
   identifier hasn't been seen before, and INSERT is CPP_NO_INSERT,
   returns NULL.  Otherwise insert and returns a new entry.  A new
   string is allocated.  */
hashnode
ht_lookup (cpp_hash_table *table, const unsigned char *str, size_t len,
	   enum ht_lookup_option insert)
{
  return ht_lookup_with_hash (table, str, len, calc_hash (str, len),
			      insert);
}

hashnode
ht_lookup_with_hash (cpp_hash_table *table, const unsigned char *str,
		     size_t len, unsigned int hash,
		     enum ht_lookup_option insert)
{
  unsigned int hash2;
  unsigned int index;
  unsigned int deleted_index = table->nslots;
  size_t sizemask;
  hashnode node;

  sizemask = table->nslots - 1;
  index = hash & sizemask;
  table->searches++;

  node = table->entries[index];

  if (node != NULL)
    {
      if (node == DELETED)
	deleted_index = index;
      else if (node->hash_value == hash
	       && HT_LEN (node) == (unsigned int) len
	       && !memcmp (HT_STR (node), str, len))
	return node;

      /* hash2 must be odd, so we're guaranteed to visit every possible
	 location in the table during rehashing.  */
      hash2 = ((hash * 17) & sizemask) | 1;

      for (;;)
	{
	  table->collisions++;
	  index = (index + hash2) & sizemask;
	  node = table->entries[index];
	  if (node == NULL)
	    break;

	  if (node == DELETED)
	    {
	      if (deleted_index != table->nslots)
		deleted_index = index;
	    }
	  else if (node->hash_value == hash
		   && HT_LEN (node) == (unsigned int) len
		   && !memcmp (HT_STR (node), str, len))
	    return node;
	}
    }

  if (insert == HT_NO_INSERT)
    return NULL;

  /* We prefer to overwrite the first deleted slot we saw.  */
  if (deleted_index != table->nslots)
    index = deleted_index;

  node = (*table->alloc_node) (table);
  table->entries[index] = node;

  HT_LEN (node) = (unsigned int) len;
  node->hash_value = hash;

  if (table->alloc_subobject)
    {
      char *chars = (char *) table->alloc_subobject (len + 1);
      memcpy (chars, str, len);
      chars[len] = '\0';
      HT_STR (node) = (const unsigned char *) chars;
    }
  else
    HT_STR (node) = (const unsigned char *) obstack_copy0 (&table->stack,
							   str, len);

  if (++table->nelements * 4 >= table->nslots * 3)
    /* Must expand the string table.  */
    ht_expand (table);

  return node;
}

/* Double the size of a hash table, re-hashing existing entries.  */

static void
ht_expand (cpp_hash_table *table)
{
  hashnode *nentries, *p, *limit;
  unsigned int size, sizemask;

  size = table->nslots * 2;
  nentries = XCNEWVEC (hashnode, size);
  sizemask = size - 1;

  p = table->entries;
  limit = p + table->nslots;
  do
    if (*p && *p != DELETED)
      {
	unsigned int index, hash, hash2;

	hash = (*p)->hash_value;
	index = hash & sizemask;

	if (nentries[index])
	  {
	    hash2 = ((hash * 17) & sizemask) | 1;
	    do
	      {
		index = (index + hash2) & sizemask;
	      }
	    while (nentries[index]);
	  }
	nentries[index] = *p;
      }
  while (++p < limit);

  if (table->entries_owned)
    free (table->entries);
  table->entries_owned = true;
  table->entries = nentries;
  table->nslots = size;
}

/* For all nodes in TABLE, callback CB with parameters TABLE->PFILE,
   the node, and V.  */
void
ht_forall (cpp_hash_table *table, ht_cb cb, const void *v)
{
  hashnode *p, *limit;

  p = table->entries;
  limit = p + table->nslots;
  do
    if (*p && *p != DELETED)
      {
	if ((*cb) (table->pfile, *p, v) == 0)
	  break;
      }
  while (++p < limit);
}

/* Like ht_forall, but a nonzero return from the callback means that
   the entry should be removed from the table.  */
void
ht_purge (cpp_hash_table *table, ht_cb cb, const void *v)
{
  hashnode *p, *limit;

  p = table->entries;
  limit = p + table->nslots;
  do
    if (*p && *p != DELETED)
      {
	if ((*cb) (table->pfile, *p, v))
	  *p = DELETED;
      }
  while (++p < limit);
}

/* Restore the hash table.  */
void
ht_load (cpp_hash_table *ht, hashnode *entries,
	 unsigned int nslots, unsigned int nelements,
	 bool own)
{
  if (ht->entries_owned)
    free (ht->entries);
  ht->entries = entries;
  ht->nslots = nslots;
  ht->nelements = nelements;
  ht->entries_owned = own;
}

/* Dump allocation statistics to stderr.  */

void
ht_dump_statistics (cpp_hash_table *table)
{
  size_t nelts, nids, overhead, headers;
  size_t total_bytes, longest, deleted = 0;
  double sum_of_squares, exp_len, exp_len2, exp2_len;
  hashnode *p, *limit;

#define SCALE(x) ((unsigned long) ((x) < 1024*10 \
		  ? (x) \
		  : ((x) < 1024*1024*10 \
		     ? (x) / 1024 \
		     : (x) / (1024*1024))))
#define LABEL(x) ((x) < 1024*10 ? ' ' : ((x) < 1024*1024*10 ? 'k' : 'M'))

  total_bytes = longest = sum_of_squares = nids = 0;
  p = table->entries;
  limit = p + table->nslots;
  do
    if (*p == DELETED)
      ++deleted;
    else if (*p)
      {
	size_t n = HT_LEN (*p);

	total_bytes += n;
	sum_of_squares += (double) n * n;
	if (n > longest)
	  longest = n;
	nids++;
      }
  while (++p < limit);

  nelts = table->nelements;
  headers = table->nslots * sizeof (hashnode);

  fprintf (stderr, "\nString pool\n%-32s%lu\n", "entries:",
	   (unsigned long) nelts);
  fprintf (stderr, "%-32s%lu (%.2f%%)\n", "identifiers:",
	   (unsigned long) nids, nids * 100.0 / nelts);
  fprintf (stderr, "%-32s%lu\n", "slots:",
	   (unsigned long) table->nslots);
  fprintf (stderr, "%-32s%lu\n", "deleted:",
	   (unsigned long) deleted);

  if (table->alloc_subobject)
    fprintf (stderr, "%-32s%lu%c\n", "GGC bytes:",
	     SCALE (total_bytes), LABEL (total_bytes));
  else
    {
      overhead = obstack_memory_used (&table->stack) - total_bytes;
      fprintf (stderr, "%-32s%lu%c (%lu%c overhead)\n",
	       "obstack bytes:",
	       SCALE (total_bytes), LABEL (total_bytes),
	       SCALE (overhead), LABEL (overhead));
    }
  fprintf (stderr, "%-32s%lu%c\n", "table size:",
	   SCALE (headers), LABEL (headers));

  exp_len = (double)total_bytes / (double)nelts;
  exp2_len = exp_len * exp_len;
  exp_len2 = (double) sum_of_squares / (double) nelts;

  fprintf (stderr, "%-32s%.4f\n", "coll/search:",
	   (double) table->collisions / (double) table->searches);
  fprintf (stderr, "%-32s%.4f\n", "ins/search:",
	   (double) nelts / (double) table->searches);
  fprintf (stderr, "%-32s%.2f bytes (+/- %.2f)\n",
	   "avg. entry:",
	   exp_len, approx_sqrt (exp_len2 - exp2_len));
  fprintf (stderr, "%-32s%lu\n", "longest entry:",
	   (unsigned long) longest);
#undef SCALE
#undef LABEL
}

/* Return the approximate positive square root of a number N.  This is for
   statistical reports, not code generation.  */
static double
approx_sqrt (double x)
{
  double s, d;

  if (x < 0)
    abort ();
  if (x == 0)
    return 0;

  s = x;
  do
    {
      d = (s * s - x) / (2 * s);
      s -= d;
    }
  while (d > .0001);
  return s;
}
