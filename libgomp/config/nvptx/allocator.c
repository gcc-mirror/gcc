/* Copyright (C) 2021 Free Software Foundation, Inc.

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

/* The low-latency allocators use space reserved in .shared memory when the
   kernel is launched.  The heap is initialized in gomp_nvptx_main and all
   allocations are forgotten when the kernel exits.  Allocations to other
   memory spaces all use the system malloc syscall.

   The root heap descriptor is stored elsewhere in shared memory, and each
   free chunk contains a similar descriptor for the next free chunk in the
   chain.

   The descriptor is two 16-bit values: offset and size, which describe the
   location of a chunk of memory available for allocation. The offset is
   relative to the base of the heap.  The special value 0xffff, 0xffff
   indicates that the heap is locked.  The descriptor is encoded into a
   single 32-bit integer so that it may be easily accessed atomically.

   Memory is allocated to the first free chunk that fits.  The free chain
   is always stored in order of the offset to assist coalescing adjacent
   chunks.  */

#include "libgomp.h"
#include <stdlib.h>

/* There should be some .shared space reserved for us.  There's no way to
   express this magic extern sizeless array in C so use asm.  */
asm (".extern .shared .u8 __nvptx_lowlat_pool[];\n");

extern uint32_t __nvptx_lowlat_heap_root __attribute__((shared,nocommon));

typedef union {
  uint32_t raw;
  struct {
    uint16_t offset;
    uint16_t size;
  } desc;
} heapdesc;

static void *
nvptx_memspace_alloc (omp_memspace_handle_t memspace, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool;
      asm ("cvta.shared.u64\t%0, __nvptx_lowlat_pool;" : "=r"(shared_pool));

      /* Memory is allocated in 8-byte granularity.  */
      size = (size + 7) & ~7;

      /* Acquire a lock on the low-latency heap.  */
      heapdesc root;
      do
	{
	  root.raw = __atomic_exchange_n (&__nvptx_lowlat_heap_root,
					  0xffffffff, MEMMODEL_ACQUIRE);
	  if (root.raw != 0xffffffff)
	    break;
	  /* Spin.  */
	}
      while (1);

      /* Walk the free chain.  */
      heapdesc chunk = {root.raw};
      uint32_t *prev_chunkptr = NULL;
      uint32_t *chunkptr = (uint32_t*)(shared_pool + chunk.desc.offset);
      heapdesc onward_chain = {chunkptr[0]};
      while (chunk.desc.size != 0 && (uint32_t)size > chunk.desc.size)
	{
	  chunk.raw = onward_chain.raw;
	  prev_chunkptr = chunkptr;
	  chunkptr = (uint32_t*)(shared_pool + chunk.desc.offset);
	  onward_chain.raw = chunkptr[0];
	}

      void *result = NULL;
      if (chunk.desc.size != 0)
	{
	  /* Allocation successful.  */
	  result = chunkptr;

	  /* Update the free chain.  */
	  heapdesc stillfree = {chunk.raw};
	  stillfree.desc.offset += size;
	  stillfree.desc.size -= size;
	  uint32_t *stillfreeptr = (uint32_t*)(shared_pool
					       + stillfree.desc.offset);

	  if (stillfree.desc.size == 0)
	    /* The whole chunk was used.  */
	    stillfree.raw = onward_chain.raw;
	  else
	    /* The chunk was split, so restore the onward chain.  */
	    stillfreeptr[0] = onward_chain.raw;

	  /* The previous free slot or root now points to stillfree.  */
	  if (prev_chunkptr)
	    prev_chunkptr[0] = stillfree.raw;
	  else
	    root.raw = stillfree.raw;
	}

      /* Update the free chain root and release the lock.  */
      __atomic_store_n (&__nvptx_lowlat_heap_root, root.raw, MEMMODEL_RELEASE);
      return result;
    }
  else
    return malloc (size);
}

static void *
nvptx_memspace_calloc (omp_memspace_handle_t memspace, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      /* Memory is allocated in 8-byte granularity.  */
      size = (size + 7) & ~7;

      uint64_t *result = nvptx_memspace_alloc (memspace, size);
      if (result)
	/* Inline memset in which we know size is a multiple of 8.  */
	for (unsigned i = 0; i < (unsigned)size/8; i++)
	  result[i] = 0;

      return result;
    }
  else
    return calloc (1, size);
}

static void
nvptx_memspace_free (omp_memspace_handle_t memspace, void *addr, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool;
      asm ("cvta.shared.u64\t%0, __nvptx_lowlat_pool;" : "=r"(shared_pool));

      /* Memory is allocated in 8-byte granularity.  */
      size = (size + 7) & ~7;

      /* Acquire a lock on the low-latency heap.  */
      heapdesc root;
      do
	{
	  root.raw = __atomic_exchange_n (&__nvptx_lowlat_heap_root,
					  0xffffffff, MEMMODEL_ACQUIRE);
	  if (root.raw != 0xffffffff)
	    break;
	  /* Spin.  */
	}
      while (1);

      /* Walk the free chain to find where to insert a new entry.  */
      heapdesc chunk = {root.raw}, prev_chunk;
      uint32_t *prev_chunkptr = NULL, *prevprev_chunkptr = NULL;
      uint32_t *chunkptr = (uint32_t*)(shared_pool + chunk.desc.offset);
      heapdesc onward_chain = {chunkptr[0]};
      while (chunk.desc.size != 0 && addr > (void*)chunkptr)
	{
	  prev_chunk.raw = chunk.raw;
	  chunk.raw = onward_chain.raw;
	  prevprev_chunkptr = prev_chunkptr;
	  prev_chunkptr = chunkptr;
	  chunkptr = (uint32_t*)(shared_pool + chunk.desc.offset);
	  onward_chain.raw = chunkptr[0];
	}

      /* Create the new chunk descriptor.  */
      heapdesc newfreechunk;
      newfreechunk.desc.offset = (uint16_t)((uintptr_t)addr
					    - (uintptr_t)shared_pool);
      newfreechunk.desc.size = (uint16_t)size;

      /* Coalesce adjacent free chunks.  */
      if (newfreechunk.desc.offset + size == chunk.desc.offset)
	{
	  /* Free chunk follows.  */
	  newfreechunk.desc.size += chunk.desc.size;
	  chunk.raw = onward_chain.raw;
	}
      if (prev_chunkptr)
	{
	  if (prev_chunk.desc.offset + prev_chunk.desc.size
	      == newfreechunk.desc.offset)
	    {
	      /* Free chunk precedes.  */
	      newfreechunk.desc.offset = prev_chunk.desc.offset;
	      newfreechunk.desc.size += prev_chunk.desc.size;
	      addr = shared_pool + prev_chunk.desc.offset;
	      prev_chunkptr = prevprev_chunkptr;
	    }
	}

      /* Update the free chain in the new and previous chunks.  */
      ((uint32_t*)addr)[0] = chunk.raw;
      if (prev_chunkptr)
	prev_chunkptr[0] = newfreechunk.raw;
      else
	root.raw = newfreechunk.raw;

      /* Update the free chain root and release the lock.  */
      __atomic_store_n (&__nvptx_lowlat_heap_root, root.raw, MEMMODEL_RELEASE);
    }
  else
    free (addr);
}

static void *
nvptx_memspace_realloc (omp_memspace_handle_t memspace, void *addr,
			size_t oldsize, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool;
      asm ("cvta.shared.u64\t%0, __nvptx_lowlat_pool;" : "=r"(shared_pool));

      /* Memory is allocated in 8-byte granularity.  */
      oldsize = (oldsize + 7) & ~7;
      size = (size + 7) & ~7;

      if (oldsize == size)
	return addr;

      /* Acquire a lock on the low-latency heap.  */
      heapdesc root;
      do
	{
	  root.raw = __atomic_exchange_n (&__nvptx_lowlat_heap_root,
					  0xffffffff, MEMMODEL_ACQUIRE);
	  if (root.raw != 0xffffffff)
	    break;
	  /* Spin.  */
	}
      while (1);

      /* Walk the free chain.  */
      heapdesc chunk = {root.raw};
      uint32_t *prev_chunkptr = NULL;
      uint32_t *chunkptr = (uint32_t*)(shared_pool + chunk.desc.offset);
      heapdesc onward_chain = {chunkptr[0]};
      while (chunk.desc.size != 0 && (void*)chunkptr < addr)
	{
	  chunk.raw = onward_chain.raw;
	  prev_chunkptr = chunkptr;
	  chunkptr = (uint32_t*)(shared_pool + chunk.desc.offset);
	  onward_chain.raw = chunkptr[0];
	}

      void *result = NULL;
      if (size < oldsize)
	{
	  /* The new allocation is smaller than the old; we can always
	     shrink an allocation in place.  */
	  result = addr;

	  uint32_t *nowfreeptr = (uint32_t*)(addr + size);

	  /* Update the free chain.  */
	  heapdesc nowfree;
	  nowfree.desc.offset = (char*)nowfreeptr - shared_pool;
	  nowfree.desc.size = oldsize - size;

	  if (nowfree.desc.offset + size == chunk.desc.offset)
	    {
	      /* Coalesce following free chunk.  */
	      nowfree.desc.size += chunk.desc.size;
	      nowfreeptr[0] = onward_chain.raw;
	    }
	  else
	    nowfreeptr[0] = chunk.raw;

	  /* The previous free slot or root now points to nowfree.  */
	  if (prev_chunkptr)
	    prev_chunkptr[0] = nowfree.raw;
	  else
	    root.raw = nowfree.raw;
	}
      else if (chunk.desc.size != 0
	       && (char *)addr + oldsize == (char *)chunkptr
	       && chunk.desc.size >= size-oldsize)
	{
	  /* The new allocation is larger than the old, and we found a
	     large enough free block right after the existing block,
	     so we extend into that space.  */
	  result = addr;

	  uint16_t delta = size-oldsize;

	  /* Update the free chain.  */
	  heapdesc stillfree = {chunk.raw};
	  stillfree.desc.offset += delta;
	  stillfree.desc.size -= delta;
	  uint32_t *stillfreeptr = (uint32_t*)(shared_pool
					       + stillfree.desc.offset);

	  if (stillfree.desc.size == 0)
	    /* The whole chunk was used.  */
	    stillfree.raw = onward_chain.raw;
	  else
	    /* The chunk was split, so restore the onward chain.  */
	    stillfreeptr[0] = onward_chain.raw;

	  /* The previous free slot or root now points to stillfree.  */
	  if (prev_chunkptr)
	    prev_chunkptr[0] = stillfree.raw;
	  else
	    root.raw = stillfree.raw;
	}
      /* Else realloc in-place has failed and result remains NULL.  */

      /* Update the free chain root and release the lock.  */
      __atomic_store_n (&__nvptx_lowlat_heap_root, root.raw, MEMMODEL_RELEASE);

      if (result == NULL)
	{
	  /* The allocation could not be extended in place, so we simply
	     allocate fresh memory and move the data.  If we can't allocate
	     from low-latency memory then we leave the original alloaction
	     intact and return NULL.
	     We could do a fall-back to main memory, but we don't know what
	     the fall-back trait said to do.  */
	  result = nvptx_memspace_alloc (memspace, size);
	  if (result != NULL)
	    {
	      /* Inline memcpy in which we know oldsize is a multiple of 8.  */
	      uint64_t *from = addr, *to = result;
	      for (unsigned i = 0; i < (unsigned)oldsize/8; i++)
		to[i] = from[i];

	      nvptx_memspace_free (memspace, addr, oldsize);
	    }
	}
      return result;
    }
  else
    return realloc (addr, size);
}

#define MEMSPACE_ALLOC(MEMSPACE, SIZE) \
  nvptx_memspace_alloc (MEMSPACE, SIZE)
#define MEMSPACE_CALLOC(MEMSPACE, SIZE) \
  nvptx_memspace_calloc (MEMSPACE, SIZE)
#define MEMSPACE_REALLOC(MEMSPACE, ADDR, OLDSIZE, SIZE) \
  nvptx_memspace_realloc (MEMSPACE, ADDR, OLDSIZE, SIZE)
#define MEMSPACE_FREE(MEMSPACE, ADDR, SIZE) \
  nvptx_memspace_free (MEMSPACE, ADDR, SIZE)

#include "../../allocator.c"
