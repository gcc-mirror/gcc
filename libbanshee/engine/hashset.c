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

#include <stdlib.h>
#include <assert.h>
#include <math.h>

#include "hashset.h"
#include "util.h"
#define INIT_TABLE_SIZE 2
#define EMPTY_KEY 0
#define UB(n) ((1<<n)-1) /* 2^n-1 */
#define CAP(n) (1<<n)   /* 2^n */
 
struct hash_set
{
  int *traditional table;
  unsigned int ub;
  unsigned int capacity;
  unsigned int inserts;
  unsigned int size;
};

static const int prime_1 = 83;
static const int prime_2 = 5189;
static const int init_table_size = INIT_TABLE_SIZE;
static const int empty_key = EMPTY_KEY;

hash_set hs_create(region r)
{
      
  hash_set hs = ralloc(r, struct hash_set);
    
  hs->ub = UB(init_table_size);
  hs->size = init_table_size;
  hs->capacity = CAP(init_table_size);
  hs->table = (int *)calloc(hs->capacity, sizeof(int));
  hs->inserts = 0;
  return hs;
}

int hs_num_items(hash_set hs)
{
  return hs->inserts;
}

int *hs_list_items(hash_set hs)
{
  return hs->table;
}

static bool member(int *table, int ub, int i, int value)
{
  while (table[i] != empty_key)
    {
      if (table[i] == value)
	return TRUE;
      
      else
	i = ub & (i + prime_2);
    }
  return FALSE;
}

static inline void reinsert(int *table, int ub, int value)
{
  int i;

  i = ub & (prime_1 * value);

  while (table[i] != empty_key)
    {
      /* possibly the value is already present */ 
      if (table[i] == value)
	return;
      
      else
	i = ub & (i + prime_2);
    }

  table[i] = value;
}

static bool member_or_insert(int *table, int ub, int i, int value)
{
  while (table[i] != empty_key)
    {
      if (table[i] == value)
	return TRUE;
      
      else
	i = ub & (i + prime_2);
    }
  table[i] = value;
  return FALSE;
}

static void rehash(hash_set hs)
{
  int *old_table;
  int old_capacity, i;

  old_table = hs->table;
  old_capacity = hs->capacity;
  hs->capacity *= 2;
  hs->ub = UB(++hs->size);
  hs->table = (int *)calloc(hs->capacity, sizeof(int));
  assert(hs->table);
  
  
  for (i = 0; i < old_capacity; i++)
    {
      reinsert(hs->table, hs->ub, old_table[i]);
    }

  free(old_table);
}
/*
static void post_insert(hash_set hs)
{
  float percent_full;

  int capacity = hs->capacity;
  int inserts = ++hs->inserts;

  printf("%d,%d->%f\n",inserts,capacity,percent_full);
  assert(capacity);
  percent_full = (float) inserts / capacity;

 
  if (percent_full != percent_full)
    {
      assert (0);
    }

  if (percent_full >= .85)
    rehash(hs);
}
*/

static void post_insert(hash_set hs)
{
  int capacity = hs->capacity;
  int inserts = ++hs->inserts;

  float percent_capacity = capacity * .85;
 
 /*
  printf("%d,%d->%f\n",inserts,capacity,percent_capacity);
 */

  if ( (float) inserts >= percent_capacity)
    {
      rehash(hs);
    }

}


bool hs_query(hash_set hs, int entry)
{
  int hash;
  int ub = hs->ub;

  hash = ub & (prime_1 * abs(entry));
  return member(hs->table, ub, hash, entry);
}

bool hs_member(hash_set hs, int entry)
{
  int hash;
  int ub = hs->ub;

  hash = ub & (prime_1 * abs(entry));
  if (member_or_insert(hs->table, ub, hash, entry))
    return TRUE;
  
  else
    {
      post_insert(hs);
      return FALSE;
    }
}

void hs_delete(hash_set hs)
{
  free(hs->table);
}


