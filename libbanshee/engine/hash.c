/*
 * Copyright (c) 2000-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include <string.h>
#include "hash.h"
#include "util.h"

struct bucket
{
  hash_key key;
  hash_data data;
  struct bucket *next;
};

#define scan_bucket(b, var) for (var = b; var; var = var->next)

struct Hash_table
{
  region r;          /* Region for this table */
  hash_fn hash;      /* Function for hashing keys */
  keyeq_fn cmp;      /* Function for comparing keys */

  int size;          /* Number of buckets */
  int elts;          /* Number of elements */
  bool internal_rgn; /* TRUE if the ht uses an internal region */
  bucket *table;     /* Array of (size) buckets */
};

static void rehash(hash_table ht) deletes; 

/* Make a new hash table, with size buckets initially.  The actual
   table is allocated in a local region, which is discarded on rehashing. */
hash_table make_hash_table(region r, int size, hash_fn hash,
			   keyeq_fn cmp, bool internal_rgn)
{
  hash_table result;

  assert(size > 0);
  result = ralloc(r, struct Hash_table);
  
  if (internal_rgn)
    result->r = newregion();
  else
    result->r = r;

  result->internal_rgn = internal_rgn;
  result->hash = hash;
  result->cmp = cmp;
  result->size = size;
  result->elts = 0;
  result->table = rarrayalloc(result->r, size, bucket);

  return result;
}

/* Hash a string */
static int string_hash(char *str)
{
  char *c;
  int h;

  c = str;
  h = 0;
  if (!c)
    return 0;
  while (*c)
    h = 33*h + 720 + *c++; /* SML/NJ's string hash function */
  return h;
}

/* Return TRUE iff s1 == s2 */
static bool string_eq(char *s1, char *s2)
{
  return !strcmp(s1, s2);
}

/* Make a hash table for strings. */
hash_table make_string_hash_table(region rhash, int size, bool internal_rgn)
{
  return make_hash_table(rhash, size, (hash_fn) string_hash,
			 (keyeq_fn) string_eq,internal_rgn);
}

/* Zero out ht.  Doesn't reclaim bucket space. */
void hash_table_reset(hash_table ht) deletes
{
  int i;
  
  if (ht->internal_rgn)
    {
      deleteregion(ht->r);
      ht->r = newregion();  
    }

  ht->elts = 0;
  for (i = 0; i < ht->size; i++)
    ht->table[i] = NULL;
}

void hash_table_delete(hash_table ht) deletes
{
  if (ht->internal_rgn)
    deleteregion(ht->r);
}


/* Return the number of entries in ht */
int hash_table_size(hash_table ht)
{
  return ht->elts;
}

/* Return the bucket corresponding to k in ht */
static inline bucket *find_bucket(hash_table ht, hash_key k)
{
  int hash;

  hash = ht->hash(k);
  if (hash < 0)
    hash = -1*hash;
  return &ht->table[hash % ht->size];
}

/* Lookup k in ht.  Returns corresponding data in *d, and function
   result is TRUE if the k was in ht, false otherwise. */
bool hash_table_lookup(hash_table ht, hash_key k, hash_data *d)
{
  bucket cur;

  cur = *find_bucket(ht, k);
  while (cur)
    {
      if (ht->cmp(k, cur->key))
	{
	  if (d)
	    *d = cur->data;
	  return TRUE;
	}
      cur = cur->next;
    }
  return FALSE;
}


/* Add k:d to ht.  If k was already in ht, replace old entry by k:d.
   Rehash if necessary.  Returns TRUE if k was not already in ht. */
bool hash_table_insert(hash_table ht, hash_key k, hash_data d) deletes
{
  bucket *cur;

  if (ht->elts > ht->size*15)
    rehash(ht);
  cur = find_bucket(ht, k);
  while (*cur)
    {
      if (ht->cmp(k, (*cur)->key))
	{
	  (*cur)->data = d;
	  return FALSE; /* Replace */
	}
      cur = &(*cur)->next;
    }
  *cur = ralloc(ht->r, struct bucket);
  (*cur)->key = k;
  (*cur)->data = d;
  (*cur)->next = NULL;
  ht->elts++;
  return TRUE; /* New key */
}

/* Remove mapping for k in ht.  Returns TRUE if k was in ht. */
bool hash_table_remove(hash_table ht, hash_key k) 
{
  bucket *cur;
  bucket *prev = NULL;

  cur = find_bucket(ht, k);
  while (*cur)
    {
      if (ht->cmp(k, (*cur)->key))
	{
	  if (!*prev)
	    (*prev)->next = (*cur)->next;
	  else
	    *cur = NULL;
	  ht->elts--;
	  return TRUE;
	}
      prev = cur;
      cur = &(*cur)->next;
    }
  return FALSE;
}

/* Return a copy of ht */
hash_table hash_table_copy(region r, hash_table ht)
{
  int i;
  hash_table result;
  bucket cur, newbucket, *prev;

  result = make_hash_table(r, ht->size, ht->hash, ht->cmp,ht->internal_rgn);
  result->elts = ht->elts;
  
  for (i = 0; i < ht->size; i++)
    {
      prev = &result->table[i];
      scan_bucket(ht->table[i], cur)
	{
	  newbucket = ralloc(result->r, struct bucket);
	  newbucket->key = cur->key;
	  newbucket->data = cur->data;
	  newbucket->next = NULL;
	  assert(!*prev);
	  *prev = newbucket;
	  prev = &newbucket->next;
	}
    }
  return result;
  /*
  hash_table result;
  hash_table_scanner hts;
  hash_key k;
  hash_data d;
  
  result = make_hash_table(r, ht->size, ht->hash, ht->cmp);
  hash_table_scan(ht, &hts);
  while (hash_table_next(&hts, &k, &d))
    insist(hash_table_insert(result, k, d));
  
  return result;
  */
}

/* Increase size of ht (double it) and reinsert all the elements */
static void rehash(hash_table ht) deletes
{
  int old_table_size, i;
  bucket *old_table, cur;
  region old_region;

#ifdef DEBUG
  printf("Rehash table size=%d, elts=%d\n", ht->size, ht->elts);
#endif

  old_table_size = ht->size;
  old_table = ht->table;
  old_region = ht->r;

  if (ht->internal_rgn)
    ht->r = newregion();

  ht->size = ht->size*2;
  ht->elts = 0;
  ht->table = rarrayalloc(ht->r, ht->size, bucket);

  for (i = 0; i < old_table_size; i++)
    scan_bucket(old_table[i], cur)
      insist(hash_table_insert(ht, cur->key, cur->data));

  if (ht->internal_rgn)
    deleteregion(old_region);
}

/* Begin scanning ht */
void hash_table_scan(hash_table ht, hash_table_scanner *hts)
{
  hts->ht = ht;
  hts->i = 0;
  hts->cur = hts->ht->table[0];
}

/* Get next elt in table, storing the elt in *k and *d if k and d are
   non-NULL, respectively.  Returns TRUE if there is a next elt, FALSE
   otherwise. */
bool hash_table_next(hash_table_scanner *hts, hash_key *k, hash_data *d)
{
  while (hts->cur == NULL)
    {
      hts->i++;
      if (hts->i < hts->ht->size)
	hts->cur = hts->ht->table[hts->i];
      else
	break;
    }

  if (hts->i == hts->ht->size)
    {
      return FALSE;
    }
  else
    {
      if (k)
	*k = hts->cur->key;
      if (d)
	*d = hts->cur->data;
      hts->cur = hts->cur->next;
    }
  return TRUE;
}

/* Apply f to all elements of ht, in some arbitrary order */
void hash_table_apply(hash_table ht, hash_apply_fn f, void *arg)
{
  int i;
  bucket cur;

  for (i = 0; i < ht->size; i++)
    scan_bucket(ht->table[i], cur)
      f(cur->key, cur->data, arg);
}

/* Map f to all elements on ht, creating a new hash table */
hash_table hash_table_map(hash_table ht, hash_map_fn f, void *arg)
{
  int i;
  hash_table result;
  bucket cur, newbucket, *prev;

  result = make_hash_table(ht->r, ht->size, ht->hash, ht->cmp,ht->internal_rgn);
  result->elts = ht->elts;
  
  for (i = 0; i < ht->size; i++)
    {
      prev = &result->table[i];
      scan_bucket(ht->table[i], cur)
	{
	  newbucket = ralloc(ht->r, struct bucket);
	  newbucket->key = cur->key;
	  newbucket->data = f(cur->key, cur->data, arg);
	  newbucket->next = NULL;
	  assert(!*prev);
	  *prev = newbucket;
	  prev = &newbucket->next;
	}
    }
  return result;
  /*
  hash_table result;
  int i;
  bucket cur;

  result = make_hash_table(ht->r, ht->size, ht->hash, ht->cmp);
  for (i = 0; i < ht->size; i++)
    scan_bucket(ht->table[i], cur)
      insist(hash_table_insert(result, cur->key, f(cur->key, cur->data, arg)));
  return result;
  */
}

static keycmp_fn cur_cmp = NULL;

static int entry_cmp(const void *a, const void *b)
{
  struct sorted_entry *ae = (struct sorted_entry *) a;
  struct sorted_entry *be = (struct sorted_entry *) b;
  return cur_cmp(ae->k, be->k);
}

/* Begin scanning ht in sorted order according to f */
void hash_table_scan_sorted(hash_table ht, keycmp_fn f,
			    hash_table_scanner_sorted *htss)
{
  hash_table_scanner hts;
  int i;

  htss->r = newregion();
  htss->size = hash_table_size(ht);
  htss->entries = rarrayalloc(htss->r, htss->size, struct sorted_entry);
  htss->i = 0;

  hash_table_scan(ht, &hts);
  i = 0;
  while (hash_table_next(&hts, &htss->entries[i].k,
			 &htss->entries[i].d))
    i++;
  assert(i == htss->size);
  cur_cmp = f;
  qsort(htss->entries, htss->size, sizeof(struct sorted_entry), entry_cmp);
  cur_cmp = NULL;
}

/* Just like hash_table_next, but scans in sorted order */
bool hash_table_next_sorted(hash_table_scanner_sorted *htss, hash_key *k,
			    hash_data *d) deletes
{
  if (htss->i < htss->size)
    {
      *k = htss->entries[htss->i].k;
      *d = htss->entries[htss->i].d;
      htss->i++;
      return TRUE;
    }
  else
    {
      deleteregion(htss->r);
      htss->r = NULL;
      return FALSE;
    }
}
