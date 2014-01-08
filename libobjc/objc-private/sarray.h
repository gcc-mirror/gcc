/* Sparse Arrays for Objective C dispatch tables
   Copyright (C) 1993-2014 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
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

#ifndef __sarray_INCLUDE_GNU
#define __sarray_INCLUDE_GNU

#define OBJC_SPARSE2		/* 2-level sparse array.  */
/* #define OBJC_SPARSE3 */      /* 3-level sparse array.  */

#ifdef OBJC_SPARSE2
extern const char* __objc_sparse2_id;
#endif

#ifdef OBJC_SPARSE3
extern const char* __objc_sparse3_id;
#endif

#include <stddef.h>

extern int nbuckets;		/* for stats */
extern int nindices;
extern int narrays;
extern int idxsize;

/* An unsigned integer of same size as a pointer.  */
#define SIZET_BITS (sizeof (size_t) * 8)

#if defined (__sparc__) || defined (OBJC_SPARSE2)
#define PRECOMPUTE_SELECTORS
#endif

#ifdef OBJC_SPARSE3

/* Buckets are 8 words each.  */
#define BUCKET_BITS 3
#define BUCKET_SIZE (1 << BUCKET_BITS)
#define BUCKET_MASK (BUCKET_SIZE - 1)

/* Indices are 16 words each.  */
#define INDEX_BITS 4
#define INDEX_SIZE (1 << INDEX_BITS)
#define INDEX_MASK (INDEX_SIZE - 1)

#define INDEX_CAPACITY (BUCKET_SIZE * INDEX_SIZE)

#else /* OBJC_SPARSE2 */

/* Buckets are 32 words each.  */
#define BUCKET_BITS 5
#define BUCKET_SIZE (1 << BUCKET_BITS)
#define BUCKET_MASK (BUCKET_SIZE - 1)

#endif /* OBJC_SPARSE2 */

typedef size_t sidx;

#ifdef PRECOMPUTE_SELECTORS

struct soffset
{
#ifdef OBJC_SPARSE3
  unsigned int unused : SIZET_BITS / 4;
  unsigned int eoffset : SIZET_BITS / 4;
  unsigned int boffset : SIZET_BITS / 4;
  unsigned int ioffset : SIZET_BITS / 4;
#else /* OBJC_SPARSE2 */
#ifdef __sparc__
  unsigned long boffset : (SIZET_BITS - 2) - BUCKET_BITS;
  unsigned int eoffset : BUCKET_BITS;
  unsigned int unused  : 2;
#else
  unsigned int boffset : SIZET_BITS / 2;
  unsigned int eoffset : SIZET_BITS / 2;
#endif
#endif /* OBJC_SPARSE2 */
};

union sofftype
{
  struct soffset off;
  sidx idx;
};

#endif /* not PRECOMPUTE_SELECTORS */

union sversion
{
  int	version;
  void *next_free;
};

struct sbucket
{
  /* Elements stored in array.  */
  void* elems[BUCKET_SIZE];

  /* Used for copy-on-write.  */
  union sversion version;
};

#ifdef OBJC_SPARSE3

struct sindex
{
  struct sbucket* buckets[INDEX_SIZE];

  /* Used for copy-on-write. */
  union sversion version;		
};

#endif /* OBJC_SPARSE3 */

struct sarray
{
#ifdef OBJC_SPARSE3
  struct sindex** indices;
  struct sindex* empty_index;
#else /* OBJC_SPARSE2 */
  struct sbucket** buckets;
#endif  /* OBJC_SPARSE2 */
  struct sbucket* empty_bucket;

  /* Used for copy-on-write. */
  union sversion version;

  short ref_count;
  struct sarray* is_copy_of;
  size_t capacity;
};

struct sarray* sarray_new (int, void* default_element);
void sarray_free (struct sarray*);
struct sarray* sarray_lazy_copy (struct sarray*);
void sarray_realloc (struct sarray*, int new_size);
void sarray_at_put (struct sarray*, sidx indx, void* elem);
void sarray_at_put_safe (struct sarray*, sidx indx, void* elem);

struct sarray* sarray_hard_copy (struct sarray*); /* ... like the name ?  */
void sarray_remove_garbage (void);


#ifdef PRECOMPUTE_SELECTORS
/* Transform soffset values to ints and vice versa.  */
static inline unsigned int
soffset_decode (sidx indx)
{
  union sofftype x;
  x.idx = indx;
#ifdef OBJC_SPARSE3
  return x.off.eoffset
    + (x.off.boffset * BUCKET_SIZE)
    + (x.off.ioffset * INDEX_CAPACITY);
#else /* OBJC_SPARSE2 */
  return x.off.eoffset + (x.off.boffset * BUCKET_SIZE);
#endif /* OBJC_SPARSE2 */
}

static inline sidx
soffset_encode (size_t offset)
{
  union sofftype x;
  x.off.eoffset = offset % BUCKET_SIZE;
#ifdef OBJC_SPARSE3
  x.off.boffset = (offset / BUCKET_SIZE) % INDEX_SIZE;
  x.off.ioffset = offset / INDEX_CAPACITY;
#else /* OBJC_SPARSE2 */
  x.off.boffset = offset / BUCKET_SIZE;
#endif
  return (sidx)x.idx;
}

#else /* not PRECOMPUTE_SELECTORS */

static inline size_t
soffset_decode (sidx indx)
{
  return indx;
}

static inline sidx
soffset_encode (size_t offset)
{
  return offset;
}
#endif /* not PRECOMPUTE_SELECTORS */

/* Get element from the Sparse array `array' at offset `indx'.  */
static inline void* sarray_get (struct sarray* array, sidx indx)
{
#ifdef PRECOMPUTE_SELECTORS
  union sofftype x;
  x.idx = indx;
#ifdef OBJC_SPARSE3
  return array->
    indices[x.off.ioffset]->
    buckets[x.off.boffset]->
    elems[x.off.eoffset];
#else /* OBJC_SPARSE2 */
  return array->buckets[x.off.boffset]->elems[x.off.eoffset];
#endif /* OBJC_SPARSE2 */
#else /* not PRECOMPUTE_SELECTORS */
#ifdef OBJC_SPARSE3
  return array->
    indices[indx / INDEX_CAPACITY]->
    buckets[(indx / BUCKET_SIZE) % INDEX_SIZE]->
    elems[indx % BUCKET_SIZE];
#else /* OBJC_SPARSE2 */
  return array->buckets[indx / BUCKET_SIZE]->elems[indx % BUCKET_SIZE];
#endif /* not OBJC_SPARSE3 */
#endif /* not PRECOMPUTE_SELECTORS */
}

static inline void* sarray_get_safe (struct sarray* array, sidx indx)
{
  if (soffset_decode (indx) < array->capacity)
    return sarray_get (array, indx);
  else
    return (array->empty_bucket->elems[0]);
}

#endif /* __sarray_INCLUDE_GNU */
