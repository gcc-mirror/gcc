/* Copyright (C) 2023-2025 Free Software Foundation, Inc.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This is a basic "malloc" implementation intended for use with small,
   low-latency memories.

   To use this template, define BASIC_ALLOC_PREFIX, and then #include the
   source file.  The other configuration macros are optional.

   The root heap descriptor is stored in the first bytes of the heap, and each
   free chunk contains a similar descriptor for the next free chunk in the
   chain.

   The descriptor is two values: offset and size, which describe the
   location of a chunk of memory available for allocation. The offset is
   relative to the base of the heap.  The special offset value 0xffffffff
   indicates that the heap (free chain) is locked.  The offset and size are
   32-bit values so the base alignment can be 8-bytes.

   Memory is allocated to the first free chunk that fits.  The free chain
   is always stored in order of the offset to assist coalescing adjacent
   chunks.  */

#include "libgomp.h"

#ifndef BASIC_ALLOC_PREFIX
#error "BASIC_ALLOC_PREFIX not defined."
#endif

#ifndef BASIC_ALLOC_YIELD
#define BASIC_ALLOC_YIELD
#endif

#define ALIGN(VAR) (((VAR) + 7) & ~7)    /* 8-byte granularity.  */

#define fn1(prefix, name) prefix ## _ ## name
#define fn(prefix, name) fn1 (prefix, name)
#define basic_alloc_init fn(BASIC_ALLOC_PREFIX,init)
#define basic_alloc_alloc fn(BASIC_ALLOC_PREFIX,alloc)
#define basic_alloc_calloc fn(BASIC_ALLOC_PREFIX,calloc)
#define basic_alloc_free fn(BASIC_ALLOC_PREFIX,free)
#define basic_alloc_realloc fn(BASIC_ALLOC_PREFIX,realloc)

typedef struct {
  uint32_t offset;
  uint32_t size;
} heapdesc;

void
basic_alloc_init (char *heap, size_t limit)
{
  if (heap == NULL)
    return;

  /* Initialize the head of the free chain.  */
  heapdesc *root = (heapdesc *) heap;
  root->offset = ALIGN(1);
  root->size = limit - root->offset;

  /* And terminate the chain.  */
  heapdesc *next = (heapdesc *) (heap + root->offset);
  next->offset = 0;
  next->size = 0;
}

static void *
basic_alloc_alloc (char *heap, size_t size)
{
  if (heap == NULL)
    return NULL;

  /* Memory is allocated in N-byte granularity.  */
  size = ALIGN (size);

  /* Acquire a lock on the low-latency heap.  */
  heapdesc root, *root_ptr = (heapdesc *) heap;
  do
    {
      root.offset = __atomic_exchange_n (&root_ptr->offset, 0xffffffff, 
					 MEMMODEL_ACQUIRE);
      if (root.offset != 0xffffffff)
	{
	  root.size = root_ptr->size;
	  break;
	}
      /* Spin.  */
      BASIC_ALLOC_YIELD;
    }
  while (1);

  /* Walk the free chain.  */
  heapdesc chunk = root;
  heapdesc *prev_chunkptr = NULL;
  heapdesc *chunkptr = (heapdesc *) (heap + chunk.offset);
  heapdesc onward_chain = *chunkptr;
  while (chunk.size != 0 && (uint32_t) size > chunk.size)
    {
      chunk = onward_chain;
      prev_chunkptr = chunkptr;
      chunkptr = (heapdesc *) (heap + chunk.offset);
      onward_chain = *chunkptr;
    }

  void *result = NULL;
  if (chunk.size != 0)
    {
      /* Allocation successful.  */
      result = chunkptr;

      /* Update the free chain.  */
      heapdesc stillfree = chunk;
      stillfree.offset += size;
      stillfree.size -= size;
      heapdesc *stillfreeptr = (heapdesc *) (heap + stillfree.offset);

      if (stillfree.size == 0)
	/* The whole chunk was used.  */
	stillfree = onward_chain;
      else
	/* The chunk was split, so restore the onward chain.  */
	*stillfreeptr = onward_chain;

      /* The previous free slot or root now points to stillfree.  */
      if (prev_chunkptr)
	*prev_chunkptr = stillfree;
      else
	root = stillfree;
    }

  /* Update the free chain root and release the lock.  */
  root_ptr->size = root.size;
  __atomic_store_n (&root_ptr->offset, root.offset, MEMMODEL_RELEASE);

  return result;
}

static void *
basic_alloc_calloc (char *heap, size_t size)
{
  /* Memory is allocated in N-byte granularity.  */
  size = ALIGN (size);

  uint64_t *result = basic_alloc_alloc (heap, size);
  if (result)
    /* Inline memset in which we know size is a multiple of 8.  */
    for (unsigned i = 0; i < (unsigned) size / 8; i++)
    result[i] = 0;

  return result;
}

static void
basic_alloc_free (char *heap, void *addr, size_t size)
{
  /* Memory is allocated in N-byte granularity.  */
  size = ALIGN (size);

  /* Acquire a lock on the low-latency heap.  */
  heapdesc root, *root_ptr = (heapdesc *) heap;
  do
    {
      root.offset = __atomic_exchange_n (&root_ptr->offset, 0xffffffff, 
					 MEMMODEL_ACQUIRE);
      if (root.offset != 0xffffffff)
	{
	  root.size = root_ptr->size;
	  break;
	}
      /* Spin.  */
      BASIC_ALLOC_YIELD;
    }
  while (1);

  /* Walk the free chain to find where to insert a new entry.  */
  heapdesc chunk = root, prev_chunk = {0};
  heapdesc *prev_chunkptr = NULL, *prevprev_chunkptr = NULL;
  heapdesc *chunkptr = (heapdesc *) (heap + chunk.offset);
  heapdesc onward_chain = *chunkptr;
  while (chunk.size != 0 && addr > (void *) chunkptr)
    {
      prev_chunk = chunk;
      chunk = onward_chain;
      prevprev_chunkptr = prev_chunkptr;
      prev_chunkptr = chunkptr;
      chunkptr = (heapdesc *) (heap + chunk.offset);
      onward_chain = *chunkptr;
    }

  /* Create the new chunk descriptor.  */
  heapdesc newfreechunk;
  newfreechunk.offset = (uint32_t) ((uintptr_t) addr - (uintptr_t) heap);
  newfreechunk.size = (uint32_t) size;

  /* Coalesce adjacent free chunks.  */
  if (newfreechunk.offset + size == chunk.offset)
    {
      /* Free chunk follows.  */
      newfreechunk.size += chunk.size;
      chunk = onward_chain;
    }
  if (prev_chunkptr)
    {
      if (prev_chunk.offset + prev_chunk.size
	  == newfreechunk.offset)
	{
	  /* Free chunk precedes.  */
	  newfreechunk.offset = prev_chunk.offset;
	  newfreechunk.size += prev_chunk.size;
	  addr = heap + prev_chunk.offset;
	  prev_chunkptr = prevprev_chunkptr;
	}
    }

  /* Update the free chain in the new and previous chunks.  */
  *(heapdesc *) addr = chunk;
  if (prev_chunkptr)
    *prev_chunkptr = newfreechunk;
  else
    root = newfreechunk;

  /* Update the free chain root and release the lock.  */
  root_ptr->size = root.size;
  __atomic_store_n (&root_ptr->offset, root.offset, MEMMODEL_RELEASE);

}

static void *
basic_alloc_realloc (char *heap, void *addr, size_t oldsize,
				      size_t size)
{
  /* Memory is allocated in N-byte granularity.  */
  oldsize = ALIGN (oldsize);
  size = ALIGN (size);

  if (oldsize == size)
    return addr;

  /* Acquire a lock on the low-latency heap.  */
  heapdesc root, *root_ptr = (heapdesc *) heap;
  do
    {
      root.offset = __atomic_exchange_n (&root_ptr->offset, 0xffffffff, 
					 MEMMODEL_ACQUIRE);
      if (root.offset != 0xffffffff)
	{
	  root.size = root_ptr->size;
	  break;
	}
      /* Spin.  */
      BASIC_ALLOC_YIELD;
    }
  while (1);

  /* Walk the free chain.  */
  heapdesc chunk = root;
  heapdesc *prev_chunkptr = NULL;
  heapdesc *chunkptr = (heapdesc *) (heap + chunk.offset);
  heapdesc onward_chain = *chunkptr;
  while (chunk.size != 0 && (void *) chunkptr < addr)
    {
      chunk = onward_chain;
      prev_chunkptr = chunkptr;
      chunkptr = (heapdesc *) (heap + chunk.offset);
      onward_chain = *chunkptr;
    }

  void *result = NULL;
  if (size < oldsize)
    {
      /* The new allocation is smaller than the old; we can always
	 shrink an allocation in place.  */
      result = addr;

      heapdesc *nowfreeptr = (heapdesc *) (addr + size);

      /* Update the free chain.  */
      heapdesc nowfree;
      nowfree.offset = (char *) nowfreeptr - heap;
      nowfree.size = oldsize - size;

      if (nowfree.offset + size == chunk.offset)
	{
	  /* Coalesce following free chunk.  */
	  nowfree.size += chunk.size;
	  *nowfreeptr = onward_chain;
	}
      else
	*nowfreeptr = chunk;

      /* The previous free slot or root now points to nowfree.  */
      if (prev_chunkptr)
	*prev_chunkptr = nowfree;
      else
	root = nowfree;
    }
  else if (chunk.size != 0
	   && (char *) addr + oldsize == (char *) chunkptr
	   && chunk.size >= size-oldsize)
    {
      /* The new allocation is larger than the old, and we found a
	 large enough free block right after the existing block,
	 so we extend into that space.  */
      result = addr;

      uint32_t delta = size-oldsize;

      /* Update the free chain.  */
      heapdesc stillfree = chunk;
      stillfree.offset += delta;
      stillfree.size -= delta;
      heapdesc *stillfreeptr = (heapdesc *) (heap + stillfree.offset);

      if (stillfree.size == 0)
	/* The whole chunk was used.  */
	stillfree = onward_chain;
      else
	/* The chunk was split, so restore the onward chain.  */
	*stillfreeptr = onward_chain;

      /* The previous free slot or root now points to stillfree.  */
      if (prev_chunkptr)
	*prev_chunkptr = stillfree;
      else
	root = stillfree;
    }
  /* Else realloc in-place has failed and result remains NULL.  */

  /* Update the free chain root and release the lock.  */
  root_ptr->size = root.size;
  __atomic_store_n (&root_ptr->offset, root.offset, MEMMODEL_RELEASE);

  if (result == NULL)
    {
      /* The allocation could not be extended in place, so we simply
	 allocate fresh memory and move the data.  If we can't allocate
	 from low-latency memory then we leave the original alloaction
	 intact and return NULL.
	 We could do a fall-back to main memory, but we don't know what
	 the fall-back trait said to do.  */
      result = basic_alloc_alloc (heap, size);
      if (result != NULL)
	{
	  /* Inline memcpy in which we know oldsize is a multiple of 8.  */
	  uint64_t *from = addr, *to = result;
	  for (unsigned i = 0; i < (unsigned) oldsize / 8; i++)
	    to[i] = from[i];

	  basic_alloc_free (heap, addr, oldsize);
	}
    }

  return result;
}

#undef ALIGN
#undef fn1
#undef fn
#undef basic_alloc_init
#undef basic_alloc_alloc
#undef basic_alloc_free
#undef basic_alloc_realloc
