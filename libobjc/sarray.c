/* Sparse Arrays for Objective C dispatch tables
   Copyright (C) 1993, 1995, 1996, 2002, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "sarray.h"
#include "runtime.h"
#include <stdio.h>
#include "assert.h"

int nbuckets = 0;					/* !T:MUTEX */
int nindices = 0;					/* !T:MUTEX */
int narrays = 0;					/* !T:MUTEX */
int idxsize = 0;					/* !T:MUTEX */

static void *first_free_data = NULL;			/* !T:MUTEX */

#ifdef OBJC_SPARSE2
const char *__objc_sparse2_id = "2 level sparse indices";
#endif

#ifdef OBJC_SPARSE3
const char *__objc_sparse3_id = "3 level sparse indices";
#endif

/* This function removes any structures left over from free operations
   that were not safe in a multi-threaded environment. */
void
sarray_remove_garbage (void)
{
  void **vp;
  void *np;
  
  objc_mutex_lock (__objc_runtime_mutex);

  vp = first_free_data;
  first_free_data = NULL;

  while (vp) {
    np = *vp;
    objc_free (vp);
    vp = np;
  }
  
  objc_mutex_unlock (__objc_runtime_mutex);
}

/* Free a block of dynamically allocated memory.  If we are in multi-threaded
   mode, it is ok to free it.  If not, we add it to the garbage heap to be
   freed later. */

static void
sarray_free_garbage (void *vp)
{
  objc_mutex_lock (__objc_runtime_mutex);
  
  if (__objc_runtime_threads_alive == 1) {
    objc_free (vp);
    if (first_free_data)
      sarray_remove_garbage ();
  }
  else {
    *(void **)vp = first_free_data;
    first_free_data = vp;
  }
      
  objc_mutex_unlock (__objc_runtime_mutex);
}

/* sarray_at_put : copies data in such a way as to be thread reader safe. */
void
sarray_at_put (struct sarray *array, sidx index, void *element)
{
#ifdef OBJC_SPARSE3
  struct sindex **the_index;
  struct sindex *new_index;
#endif
  struct sbucket **the_bucket;
  struct sbucket *new_bucket;
#ifdef OBJC_SPARSE3
  size_t ioffset;
#endif
  size_t boffset;
  size_t eoffset;
#ifdef PRECOMPUTE_SELECTORS
  union sofftype xx; 
  xx.idx = index;
#ifdef OBJC_SPARSE3
  ioffset = xx.off.ioffset;
#endif
  boffset = xx.off.boffset;
  eoffset = xx.off.eoffset;
#else /* not PRECOMPUTE_SELECTORS */
#ifdef OBJC_SPARSE3
  ioffset = index/INDEX_CAPACITY;
  boffset = (index/BUCKET_SIZE)%INDEX_SIZE;
  eoffset = index%BUCKET_SIZE;
#else
  boffset = index/BUCKET_SIZE;
  eoffset = index%BUCKET_SIZE;
#endif
#endif /* not PRECOMPUTE_SELECTORS */

  assert (soffset_decode (index) < array->capacity); /* Range check */

#ifdef OBJC_SPARSE3
  the_index = &(array->indices[ioffset]);
  the_bucket = &((*the_index)->buckets[boffset]);
#else
  the_bucket = &(array->buckets[boffset]);
#endif
  
  if ((*the_bucket)->elems[eoffset] == element)
    return;		/* great! we just avoided a lazy copy */

#ifdef OBJC_SPARSE3

  /* First, perform lazy copy/allocation of index if needed */

  if ((*the_index) == array->empty_index) {

    /* The index was previously empty, allocate a new */
    new_index = (struct sindex *) objc_malloc (sizeof (struct sindex));
    memcpy (new_index, array->empty_index, sizeof (struct sindex));
    new_index->version.version = array->version.version;
    *the_index = new_index;                     /* Prepared for install. */
    the_bucket = &((*the_index)->buckets[boffset]);
    
    nindices += 1;
  } else if ((*the_index)->version.version != array->version.version) {

    /* This index must be lazy copied */
    struct sindex *old_index = *the_index;
    new_index = (struct sindex *) objc_malloc (sizeof (struct sindex));
    memcpy (new_index, old_index, sizeof (struct sindex));
    new_index->version.version = array->version.version;
    *the_index = new_index;                     /* Prepared for install. */
    the_bucket = &((*the_index)->buckets[boffset]);
    
    nindices += 1;
  }

#endif /* OBJC_SPARSE3 */

  /* next, perform lazy allocation/copy of the bucket if needed */

  if ((*the_bucket) == array->empty_bucket) {

    /* The bucket was previously empty (or something like that), */
    /* allocate a new.  This is the effect of `lazy' allocation */  
    new_bucket = (struct sbucket *) objc_malloc (sizeof (struct sbucket));
    memcpy ((void *) new_bucket, (const void *) array->empty_bucket, 
	    sizeof (struct sbucket));
    new_bucket->version.version = array->version.version;
    *the_bucket = new_bucket;                   /* Prepared for install. */
    
    nbuckets += 1;

  } else if ((*the_bucket)->version.version != array->version.version) {

    /* Perform lazy copy. */
    struct sbucket *old_bucket = *the_bucket;
    new_bucket = (struct sbucket *) objc_malloc (sizeof (struct sbucket));
    memcpy (new_bucket, old_bucket, sizeof (struct sbucket));
    new_bucket->version.version = array->version.version;
    *the_bucket = new_bucket;                   /* Prepared for install. */
    
    nbuckets += 1;

  }
  (*the_bucket)->elems[eoffset] = element;
}

void
sarray_at_put_safe (struct sarray *array, sidx index, void *element)
{
  if (soffset_decode (index) >= array->capacity)
    sarray_realloc (array, soffset_decode (index) + 1);
  sarray_at_put (array, index, element);
}

struct sarray *
sarray_new (int size, void *default_element)
{
  struct sarray *arr;
#ifdef OBJC_SPARSE3
  size_t num_indices = ((size - 1)/(INDEX_CAPACITY)) + 1;
  struct sindex **new_indices;
#else /* OBJC_SPARSE2 */
  size_t num_indices = ((size - 1)/BUCKET_SIZE) + 1;
  struct sbucket **new_buckets;
#endif
  size_t counter;

  assert (size > 0);

  /* Allocate core array */
  arr = (struct sarray *) objc_malloc (sizeof (struct sarray));
  arr->version.version = 0;
  
  /* Initialize members */
#ifdef OBJC_SPARSE3
  arr->capacity = num_indices*INDEX_CAPACITY;
  new_indices = (struct sindex **) 
    objc_malloc (sizeof (struct sindex *) * num_indices);

  arr->empty_index = (struct sindex *) objc_malloc (sizeof (struct sindex));
  arr->empty_index->version.version = 0;
  
  narrays  += 1;
  idxsize  += num_indices;
  nindices += 1;

#else /* OBJC_SPARSE2 */
  arr->capacity = num_indices*BUCKET_SIZE;
  new_buckets = (struct sbucket **) 
    objc_malloc (sizeof (struct sbucket *) * num_indices);
  
  narrays  += 1;
  idxsize  += num_indices;

#endif

  arr->empty_bucket = (struct sbucket *) objc_malloc (sizeof (struct sbucket));
  arr->empty_bucket->version.version = 0;
  
  nbuckets += 1;

  arr->ref_count = 1;
  arr->is_copy_of = (struct sarray *) 0;
  
  for (counter = 0; counter < BUCKET_SIZE; counter++)
    arr->empty_bucket->elems[counter] = default_element;

#ifdef OBJC_SPARSE3
  for (counter = 0; counter < INDEX_SIZE; counter++)
    arr->empty_index->buckets[counter] = arr->empty_bucket;

  for (counter = 0; counter < num_indices; counter++)
    new_indices[counter] = arr->empty_index;

#else /* OBJC_SPARSE2 */

  for (counter = 0; counter < num_indices; counter++)
    new_buckets[counter] = arr->empty_bucket;

#endif
  
#ifdef OBJC_SPARSE3
  arr->indices = new_indices;
#else /* OBJC_SPARSE2 */
  arr->buckets = new_buckets;
#endif
  
  return arr;
}


/* Reallocate the sparse array to hold `newsize' entries
   Note: We really allocate and then free.  We have to do this to ensure that
   any concurrent readers notice the update. */

void 
sarray_realloc (struct sarray *array, int newsize)
{
#ifdef OBJC_SPARSE3
  size_t old_max_index = (array->capacity - 1)/INDEX_CAPACITY;
  size_t new_max_index = ((newsize - 1)/INDEX_CAPACITY);
  size_t rounded_size = (new_max_index + 1) * INDEX_CAPACITY;

  struct sindex **new_indices;
  struct sindex **old_indices;
  
#else /* OBJC_SPARSE2 */
  size_t old_max_index = (array->capacity - 1)/BUCKET_SIZE;
  size_t new_max_index = ((newsize - 1)/BUCKET_SIZE);
  size_t rounded_size = (new_max_index + 1) * BUCKET_SIZE;

  struct sbucket **new_buckets;
  struct sbucket **old_buckets;
  
#endif

  size_t counter;

  assert (newsize > 0);

  /* The size is the same, just ignore the request */
  if (rounded_size <= array->capacity)
    return;

  assert (array->ref_count == 1);	/* stop if lazy copied... */

  /* We are asked to extend the array -- allocate new bucket table, */
  /* and insert empty_bucket in newly allocated places. */
  if (rounded_size > array->capacity) 
    {

#ifdef OBJC_SPARSE3
      new_max_index += 4;
      rounded_size = (new_max_index + 1) * INDEX_CAPACITY;
      
#else /* OBJC_SPARSE2 */
      new_max_index += 4;
      rounded_size = (new_max_index + 1) * BUCKET_SIZE;
#endif
      
      /* update capacity */
      array->capacity = rounded_size;

#ifdef OBJC_SPARSE3
      /* alloc to force re-read by any concurrent readers. */
      old_indices = array->indices;
      new_indices = (struct sindex **)
	objc_malloc ((new_max_index + 1) * sizeof (struct sindex *));
#else /* OBJC_SPARSE2 */
      old_buckets = array->buckets;
      new_buckets = (struct sbucket **)
	objc_malloc ((new_max_index + 1) * sizeof (struct sbucket *));
#endif

      /* copy buckets below old_max_index (they are still valid) */
      for (counter = 0; counter <= old_max_index; counter++ ) {
#ifdef OBJC_SPARSE3
	new_indices[counter] = old_indices[counter];
#else /* OBJC_SPARSE2 */
	new_buckets[counter] = old_buckets[counter];
#endif
      }

#ifdef OBJC_SPARSE3
      /* reset entries above old_max_index to empty_bucket */
      for (counter = old_max_index + 1; counter <= new_max_index; counter++)
	new_indices[counter] = array->empty_index;
#else /* OBJC_SPARSE2 */
      /* reset entries above old_max_index to empty_bucket */
      for (counter = old_max_index + 1; counter <= new_max_index; counter++)
	new_buckets[counter] = array->empty_bucket;
#endif
      
#ifdef OBJC_SPARSE3
      /* install the new indices */
      array->indices = new_indices;
#else /* OBJC_SPARSE2 */
      array->buckets = new_buckets;
#endif

#ifdef OBJC_SPARSE3
      /* free the old indices */
      sarray_free_garbage (old_indices);
#else /* OBJC_SPARSE2 */
      sarray_free_garbage (old_buckets);
#endif
      
      idxsize += (new_max_index-old_max_index);
      return;
    }
}


/* Free a sparse array allocated with sarray_new */

void 
sarray_free (struct sarray *array) {
#ifdef OBJC_SPARSE3
  size_t old_max_index = (array->capacity - 1)/INDEX_CAPACITY;
  struct sindex **old_indices;
#else
  size_t old_max_index = (array->capacity - 1)/BUCKET_SIZE;
  struct sbucket **old_buckets;
#endif
  size_t counter = 0;

  assert (array->ref_count != 0);	/* Freed multiple times!!! */

  if (--(array->ref_count) != 0)	/* There exists copies of me */
    return;

#ifdef OBJC_SPARSE3
  old_indices = array->indices;
#else
  old_buckets = array->buckets;
#endif

  /* Free all entries that do not point to empty_bucket */
  for (counter = 0; counter <= old_max_index; counter++ ) {
#ifdef OBJC_SPARSE3
    struct sindex *idx = old_indices[counter];
    if ((idx != array->empty_index) &&
       (idx->version.version == array->version.version)) {
      int c2; 
      for (c2 = 0; c2 < INDEX_SIZE; c2++) {
	struct sbucket *bkt = idx->buckets[c2];
	if ((bkt != array->empty_bucket) &&
	   (bkt->version.version == array->version.version))
	  {
	    sarray_free_garbage (bkt);
	    nbuckets -= 1;
	  }
      }
      sarray_free_garbage (idx);
      nindices -= 1;
    }
#else /* OBJC_SPARSE2 */
    struct sbucket *bkt = array->buckets[counter];
    if ((bkt != array->empty_bucket) &&
	(bkt->version.version == array->version.version))
      {
	sarray_free_garbage (bkt);
	nbuckets -= 1;
      }
#endif
  }
	
#ifdef OBJC_SPARSE3  
  /* free empty_index */
  if (array->empty_index->version.version == array->version.version) {
    sarray_free_garbage (array->empty_index);
    nindices -= 1;
  }
#endif

  /* free empty_bucket */
  if (array->empty_bucket->version.version == array->version.version) {
    sarray_free_garbage (array->empty_bucket);
    nbuckets -= 1;
  }
  idxsize -= (old_max_index + 1);
  narrays -= 1;

#ifdef OBJC_SPARSE3
  /* free bucket table */
  sarray_free_garbage (array->indices);

#else
  /* free bucket table */
  sarray_free_garbage (array->buckets);

#endif
  
  /* If this is a copy of another array, we free it (which might just
   * decrement its reference count so it will be freed when no longer in use).
   */
  if (array->is_copy_of)
    sarray_free (array->is_copy_of);

  /* free array */
  sarray_free_garbage (array);
}

/* This is a lazy copy.  Only the core of the structure is actually */
/* copied.   */

struct sarray *
sarray_lazy_copy (struct sarray *oarr)
{
  struct sarray *arr;

#ifdef OBJC_SPARSE3
  size_t num_indices = ((oarr->capacity - 1)/INDEX_CAPACITY) + 1;
  struct sindex **new_indices;
#else /* OBJC_SPARSE2 */
  size_t num_indices = ((oarr->capacity - 1)/BUCKET_SIZE) + 1;
  struct sbucket **new_buckets;
#endif

  /* Allocate core array */
  arr = (struct sarray *) objc_malloc (sizeof (struct sarray)); /* !!! */
  arr->version.version = oarr->version.version + 1;
#ifdef OBJC_SPARSE3
  arr->empty_index = oarr->empty_index;
#endif
  arr->empty_bucket = oarr->empty_bucket;
  arr->ref_count = 1;
  oarr->ref_count += 1;
  arr->is_copy_of = oarr;
  arr->capacity = oarr->capacity;
  
#ifdef OBJC_SPARSE3
  /* Copy bucket table */
  new_indices = (struct sindex **) 
    objc_malloc (sizeof (struct sindex *) * num_indices);
  memcpy (new_indices, oarr->indices, sizeof (struct sindex *) * num_indices);
  arr->indices = new_indices;
#else 
  /* Copy bucket table */
  new_buckets = (struct sbucket **) 
    objc_malloc (sizeof (struct sbucket *) * num_indices);
  memcpy (new_buckets, oarr->buckets, sizeof (struct sbucket *) * num_indices);
  arr->buckets = new_buckets;
#endif

  idxsize += num_indices;
  narrays += 1;
  
  return arr;
}
