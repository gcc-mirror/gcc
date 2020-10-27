/* Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

This file is part of the GNU Fortran Native Coarray Library (libnca).

Libnca is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libnca is distributed in the hope that it will be useful,
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

#include "libgfortran.h"
#include "shared_memory.h"
#include "allocator.h"
#include "hashmap.h"
#include <string.h>

#define INITIAL_BITNUM (5)
#define INITIAL_SIZE (1<<INITIAL_BITNUM)
#define CRITICAL_LOOKAHEAD (16)

static ssize_t n_ent;

typedef struct {
  memid id;
  shared_mem_ptr p; /* If p == SHMPTR_NULL, the entry is empty.  */
  size_t s;
  int max_lookahead; 
  int refcnt;
} hashmap_entry;

static ssize_t
num_entries (hashmap_entry *data, size_t size)
{
  size_t i;
  ssize_t ret = 0;
  for (i = 0; i < size; i++)
    {
      if (!SHMPTR_IS_NULL (data[i].p))
        ret ++;
    }
  return ret;
}

/* 64 bit to 64 bit hash function.  */

/*
static inline uint64_t 
hash (uint64_t x)
{
  return x * 11400714819323198485lu;
}
*/

#define ASSERT_HM(hm, cond) assert_hashmap(hm, cond, #cond)

static void 
assert_hashmap(hashmap *hm, bool asserted, const char *cond) 
{
  if (!asserted)
    {
      dprintf(2, cond);
      dump_hm(hm);
    }
  assert(asserted);
}

static inline uint64_t 
hash (uint64_t key)
{
  key ^= (key >> 30);
  key *= 0xbf58476d1ce4e5b9ul;
  key ^= (key >> 27);
  key *= 0x94d049bb133111ebul;
  key ^= (key >> 31);

  return key;
}


/* Gets a pointer to the current data in the hashmap.  */

static inline hashmap_entry *
get_data(hashmap *hm)
{
  return SHMPTR_AS (hashmap_entry *, hm->s->data, hm->sm); 
}


/* Generate mask from current number of bits.  */

static inline intptr_t
gen_mask (hashmap *hm)
{
  return (1 << hm->s->bitnum) - 1;
}


/* Add with wrap-around at hashmap size.  */

static inline size_t
hmiadd (hashmap *hm, size_t s, ssize_t o) {
  return (s + o) & gen_mask (hm);
}


/* Get the expected offset for entry id.  */

static inline ssize_t
get_expected_offset (hashmap *hm, memid id)
{
  return hash(id) >> (PTR_BITS - hm->s->bitnum);
}


/* Initialize the hashmap.  */

void 
hashmap_init (hashmap *hm, hashmap_shared *hs, allocator *a,
              shared_memory *mem)
{
  hashmap_entry *data;
  hm->s = hs;
  hm->sm = mem;
  hm->s->data = shared_malloc (a, INITIAL_SIZE * sizeof(hashmap_entry));
  data = get_data (hm);
  memset(data, '\0', INITIAL_SIZE*sizeof(hashmap_entry));

  for (int i = 0; i < INITIAL_SIZE; i++)
    data[i].p = SHMPTR_NULL;

  hm->s->size = INITIAL_SIZE;
  hm->s->bitnum = INITIAL_BITNUM;
  hm->a = a;
}


/* This checks if the entry id exists in that range the range between
   the expected position and the maximum lookahead.  */

static ssize_t 
scan_inside_lookahead (hashmap *hm, ssize_t expected_off, memid id)
{
  ssize_t lookahead;
  hashmap_entry *data;

  data = get_data (hm);
  lookahead = data[expected_off].max_lookahead;
  ASSERT_HM (hm, lookahead < CRITICAL_LOOKAHEAD);

  for (int i = 0; i <= lookahead; i++) /* For performance, this could
                                         iterate backwards.  */
    if (data[hmiadd (hm, expected_off, i)].id == id)
      return hmiadd (hm, expected_off, i);

  return -1;
}


/* Scan for the next empty slot we can use.  Returns offset relative
   to the expected position.  */

static ssize_t
scan_empty (hashmap *hm, ssize_t expected_off)
{
  hashmap_entry *data;

  data = get_data(hm);
  for (int i = 0; i < CRITICAL_LOOKAHEAD; i++) 
    if (SHMPTR_IS_NULL (data[hmiadd (hm, expected_off, i)].p))
      return i;

  return -1;
}


/* Search the hashmap for id.  */

hashmap_search_result 
hashmap_get (hashmap *hm, memid id)
{
  hashmap_search_result ret;
  hashmap_entry *data; 
  size_t expected_offset;
  ssize_t res;

  data = get_data (hm);
  expected_offset = get_expected_offset (hm, id);
  res = scan_inside_lookahead (hm, expected_offset, id);

  if (res != -1)
    ret = ((hashmap_search_result)
      { .p = data[res].p, .size=data[res].s, .res_offset = res });
  else
    ret.p = SHMPTR_NULL;

  return ret;
}


/* Return size of a hashmap search result.  */

size_t
hm_search_result_size (hashmap_search_result *res)
{
  return res->size;
}


/* Return pointer of a hashmap search result.  */

shared_mem_ptr 
hm_search_result_ptr (hashmap_search_result *res)
{
  return res->p;
}


/* Return pointer of a hashmap search result.  */

bool 
hm_search_result_contains (hashmap_search_result *res)
{
  return !SHMPTR_IS_NULL(res->p);
}


/* Enlarge hashmap memory.  */

static void
enlarge_hashmap_mem (hashmap *hm, hashmap_entry **data, bool f)
{
  shared_mem_ptr old_data_p;
  size_t old_size;

  old_data_p = hm->s->data;
  old_size = hm->s->size;

  hm->s->data = shared_malloc (hm->a, (hm->s->size *= 2)*sizeof(hashmap_entry));
  hm->s->bitnum++;

  *data = get_data(hm);
  for (size_t i = 0; i < hm->s->size; i++)
    (*data)[i] = ((hashmap_entry) { .id = 0, .p = SHMPTR_NULL, .s=0,
          .max_lookahead = 0, .refcnt=0 });

  if (f)
    shared_free(hm->a, old_data_p, old_size);
}


/* Resize hashmap.  */

static void
resize_hm (hashmap *hm, hashmap_entry **data)
{
  shared_mem_ptr old_data_p;
  hashmap_entry *old_data, *new_data;
  size_t old_size;
  ssize_t new_offset, inital_index, new_index;
  memid id;
  ssize_t max_lookahead;

  /* old_data points to the old block containing the hashmap.  We
     redistribute the data from there into the new block.  */
  
  old_data_p = hm->s->data;
  old_data = *data;
  old_size = hm->s->size;

  enlarge_hashmap_mem (hm, &new_data, false); 
 retry_resize:
  for (size_t i = 0; i < old_size; i++)
    {
      if (SHMPTR_IS_NULL (old_data[i].p))
        continue;

      id = old_data[i].id;
      inital_index = get_expected_offset (hm, id);
      new_offset = scan_empty (hm, inital_index);

      /* If we didn't find a free slot, just resize the hashmap
         again.  */
      if (new_offset == -1)
        {
          enlarge_hashmap_mem (hm, &new_data, true);
          goto retry_resize; /* Sue me.  */
        }

      ASSERT_HM (hm, new_offset < CRITICAL_LOOKAHEAD);
      new_index = hmiadd (hm, inital_index, new_offset);
      max_lookahead = new_data[inital_index].max_lookahead;
      new_data[inital_index].max_lookahead
        = new_offset > max_lookahead ? new_offset : max_lookahead;

      new_data[new_index] = ((hashmap_entry) {.id = id, .p = old_data[i].p,
            .s = old_data[i].s,
            .max_lookahead =  new_data[new_index].max_lookahead, 
            .refcnt = old_data[i].refcnt});
    }
  shared_free (hm->a, old_data_p, old_size);
  *data = new_data;
}


/* Set an entry in the hashmap.  */

void 
hashmap_set (hashmap *hm, memid id, hashmap_search_result *hsr,
             shared_mem_ptr p, size_t size) 
{
  hashmap_entry *data;
  ssize_t expected_offset, lookahead;
  ssize_t empty_offset;
  ssize_t delta;

  data = get_data(hm);

  if (hsr) {
    data[hsr->res_offset].s = size;
    data[hsr->res_offset].p = p;
    return;
  }

  expected_offset = get_expected_offset (hm, id);
  while ((delta = scan_empty (hm, expected_offset)) == -1)
    {
      resize_hm (hm, &data);
      expected_offset = get_expected_offset (hm, id);
    }

  empty_offset = hmiadd (hm, expected_offset, delta);
  lookahead = data[expected_offset].max_lookahead;
  data[expected_offset].max_lookahead = delta > lookahead ? delta : lookahead;
  data[empty_offset] = ((hashmap_entry) {.id = id, .p = p, .s = size, 
                            .max_lookahead = data[empty_offset].max_lookahead, 
                          .refcnt = 1});

  n_ent ++;
  /* TODO: Shouldn't reset refcnt, but this doesn't matter at the
     moment because of the way the function is used. */
}

/* Change the refcount of a hashmap entry.  */

static int 
hashmap_change_refcnt (hashmap *hm, memid id, hashmap_search_result *res,
                       int delta)
{
  hashmap_entry *data;
  hashmap_search_result r;
  hashmap_search_result *pr;
  int ret;
  hashmap_entry *entry;

  data = get_data (hm);

  if (res) 
    pr = res;
  else
    {
      r = hashmap_get (hm, id);
      pr = &r;
    }

  entry = &data[pr->res_offset];
  ret = (entry->refcnt += delta);
  if (ret == 0)
    {
      n_ent--;
      entry->id = 0;
      entry->p = SHMPTR_NULL;
      entry->s = 0;
    }

  return ret;
}


/* Increase hashmap entry refcount.  */

void 
hashmap_inc (hashmap *hm, memid id, hashmap_search_result * res)
{
  int ret;
  ret = hashmap_change_refcnt (hm, id, res, 1);
  ASSERT_HM (hm, ret > 0);
}


/* Decrease hashmap entry refcount.  */

int 
hashmap_dec (hashmap *hm, memid id, hashmap_search_result * res)
{
  int ret;
  ret = hashmap_change_refcnt (hm, id, res, -1);
  ASSERT_HM (hm, ret >= 0);
  return ret;
}

#define PE(str, ...) fprintf(stderr, INDENT str, ##__VA_ARGS__)
#define INDENT ""

void
dump_hm(hashmap *hm) {
  hashmap_entry *data;
  size_t exp;
  size_t occ_num = 0;
  PE("h %p (size: %lu, bitnum: %d)\n", hm, hm->s->size, hm->s->bitnum);
  data = get_data (hm);
  fprintf (stderr,"offset = %lx data = %p\n", (unsigned long) hm->s->data.offset, data);

#undef INDENT
#define INDENT "   "
  for (size_t i = 0; i < hm->s->size; i++) {
    exp =  get_expected_offset(hm, data[i].id);
    if (!SHMPTR_IS_NULL(data[i].p)) {
      PE("%2lu. (exp: %2lu w la %d) id %#-16lx p %#-14lx s %-7lu -- la %u ref %u %-16p\n",
         i, exp, data[exp].max_lookahead, data[i].id, data[i].p.offset, data[i].s,
         data[i].max_lookahead, data[i].refcnt, data + i);
      occ_num++;
    }
    else
      PE("%2lu. empty -- la %u                                                                 %p\n", i, data[i].max_lookahead,
         data + i);

  }
#undef INDENT
#define INDENT ""
  PE("occupancy: %lu %f\n", occ_num, ((double) occ_num)/hm->s->size);
}
