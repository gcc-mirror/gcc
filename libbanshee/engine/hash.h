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

#ifndef HASH_H
#define HASH_H

#include <regions.h>
#include "bool.h"
/*#include "hash_info.h"*/ /* Includes hash_key, hash_data typedef */
#include "linkage.h"

EXTERN_C_BEGIN

typedef void *hash_key;
typedef void *hash_data;

/* Function to hash a key */
typedef int (*hash_fn)(hash_key k);

/* Function returning true iff k1 and k2 are equal */
typedef bool (*keyeq_fn)(hash_key k1, hash_key k2);

/* Function applied to elts in the hash table */
typedef void (*hash_apply_fn)(hash_key k, hash_data d, void *arg);

/* Function mapped to elts in the hash table */
typedef hash_data (*hash_map_fn)(hash_key k, hash_data d, void *arg);

typedef struct Hash_table *hash_table;

/* Make a new hash table, with size buckets initially. */
hash_table make_hash_table(region rhash, int size, hash_fn hash,
			   keyeq_fn cmp, bool internal_rgn);

/* Make a hash table for strings. */
hash_table make_string_hash_table(region rhash, int size, bool internal_rgn);

/* Zero out ht.  Doesn't reclaim bucket space. */
void hash_table_reset(hash_table ht) deletes;

/* Delete ht and internal memory associated with it. The top level pointer
 must still be deleted. */
void hash_table_delete(hash_table ht) deletes;

/* Return the number of entries in ht */
int hash_table_size(hash_table ht);


/* Lookup k in ht.  If d is not NULL, returns corresponding data in *d.
   Function result is TRUE if the k was in ht, false otherwise. */
bool hash_table_lookup(hash_table ht, hash_key k, hash_data *d);

/* Add k:d to ht.  If k was already in ht, replace old entry by k:d.
   Rehash if necessary.  Returns TRUE if k was not already in ht. */
bool hash_table_insert(hash_table ht, hash_key k, hash_data d) deletes;

/* Remove mapping for k in ht.  Returns TRUE if k was in ht. */
bool hash_table_remove(hash_table ht, hash_key k);

/* Return a copy of ht, allocated in rhash */
hash_table hash_table_copy(region rhash, hash_table ht);

/* Apply f to all elements of ht, in some arbitrary order */
void hash_table_apply(hash_table ht, hash_apply_fn f, void *arg);

/* Map f to all elements on ht, creating a new hash table */
hash_table hash_table_map(hash_table ht, hash_map_fn f, void *arg);

typedef struct bucket *bucket;
typedef struct
{
  hash_table ht;
  int i;
  bucket cur;
} hash_table_scanner; /* Opaque type!  Do not modify fields. */

/* Begin scanning ht */
void hash_table_scan(hash_table ht, hash_table_scanner *);

/* Get next elt in table, storing the elt in *k and *d if k and d are
   non-NULL, respectively.  Returns TRUE if there is a next elt, FALSE
   otherwise. */
bool hash_table_next(hash_table_scanner *, hash_key *k, hash_data *d);

/* Total order on hash table keys, only uesd for hash_table_scan_sorted */
typedef int (*keycmp_fn)(hash_key k1, hash_key k2);

struct sorted_entry
{
  hash_key k;
  hash_data d;
};

typedef struct
{
  region r;
  int i;
  int size;
  struct sorted_entry *entries;
} hash_table_scanner_sorted;

/* Begin scanning ht in sorted order according to f */
void hash_table_scan_sorted(hash_table ht, keycmp_fn f,
			    hash_table_scanner_sorted *htss);

/* Just like hash_table_next, but scans in sorted order */
bool hash_table_next_sorted(hash_table_scanner_sorted *htss, hash_key *k,
			    hash_data *d) deletes;


EXTERN_C_END

#endif
