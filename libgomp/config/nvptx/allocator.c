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

#define BASIC_ALLOC_PREFIX __nvptx_lowlat
#include "../../basic-allocator.c"

/* There should be some .shared space reserved for us.  There's no way to
   express this magic extern sizeless array in C so use asm.  */
asm (".extern .shared .u8 __nvptx_lowlat_pool[];\n");

static void *
nvptx_memspace_alloc (omp_memspace_handle_t memspace, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool;
      asm ("cvta.shared.u64\t%0, __nvptx_lowlat_pool;" : "=r" (shared_pool));

      return __nvptx_lowlat_alloc (shared_pool, size);
    }
  else
    return malloc (size);
}

static void *
nvptx_memspace_calloc (omp_memspace_handle_t memspace, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool;
      asm ("cvta.shared.u64\t%0, __nvptx_lowlat_pool;" : "=r" (shared_pool));

      return __nvptx_lowlat_calloc (shared_pool, size);
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
      asm ("cvta.shared.u64\t%0, __nvptx_lowlat_pool;" : "=r" (shared_pool));

      __nvptx_lowlat_free (shared_pool, addr, size);
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
      asm ("cvta.shared.u64\t%0, __nvptx_lowlat_pool;" : "=r" (shared_pool));

      return __nvptx_lowlat_realloc (shared_pool, addr, oldsize, size);
    }
  else
    return realloc (addr, size);
}

static inline int
nvptx_memspace_validate (omp_memspace_handle_t memspace, unsigned access)
{
#if __PTX_ISA_VERSION_MAJOR__ > 4 \
    || (__PTX_ISA_VERSION_MAJOR__ == 4 && __PTX_ISA_VERSION_MINOR >= 1)
  /* Disallow use of low-latency memory when it must be accessible by
     all threads.  */
  return (memspace != omp_low_lat_mem_space
	  || access != omp_atv_all);
#else
  /* Low-latency memory is not available before PTX 4.1.  */
  return (memspace != omp_low_lat_mem_space);
#endif
}

#define MEMSPACE_ALLOC(MEMSPACE, SIZE, PIN) \
  nvptx_memspace_alloc (MEMSPACE, ((void)(PIN), (SIZE)))
#define MEMSPACE_CALLOC(MEMSPACE, SIZE, PIN) \
  nvptx_memspace_calloc (MEMSPACE, ((void)(PIN), (SIZE)))
#define MEMSPACE_REALLOC(MEMSPACE, ADDR, OLDSIZE, SIZE, OLDPIN, PIN) \
  nvptx_memspace_realloc (MEMSPACE, ADDR, OLDSIZE, \
			  ((void)(OLDPIN), (void)(PIN), (SIZE)))
#define MEMSPACE_FREE(MEMSPACE, ADDR, SIZE, PIN) \
  nvptx_memspace_free (MEMSPACE, ADDR, ((void)(PIN), (SIZE)))
#define MEMSPACE_VALIDATE(MEMSPACE, ACCESS, PIN) \
  nvptx_memspace_validate (MEMSPACE, ((void)(PIN), (ACCESS)))

/* The default low-latency memspace implies omp_atv_all, which is incompatible
   with the .shared memory space.  */
#define OMP_LOW_LAT_MEM_ALLOC_INVALID 1

#include "../../allocator.c"
