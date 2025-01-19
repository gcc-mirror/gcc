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

/* The low-latency allocators use space reserved in LDS memory when the
   kernel is launched.  The heap is initialized in gomp_gcn_enter_kernel and
   all allocations are forgotten when the kernel exits.  Allocations to other
   memory spaces all use the system malloc syscall.

   The pointers returned are 64-bit "Flat" addresses indistinguishable from
   regular pointers, but only compatible with the "flat_load/store"
   instructions.  The compiler has been coded to assign default address
   spaces accordingly.

   LDS memory is not visible to other teams, and therefore may only be used
   when the memspace access trait is set accordingly.  */

#include "libgomp.h"
#include <stdlib.h>

#define BASIC_ALLOC_PREFIX __gcn_lowlat
#define BASIC_ALLOC_YIELD asm ("s_sleep 1" ::: "memory")
#include "../../basic-allocator.c"

/* The low-latency heap is located in LDS memory, but we need the __flat
   address space for compatibility reasons.  */
#define FLAT_HEAP_PTR \
  ((void *) (uintptr_t) (void __flat *) (void __lds *) GCN_LOWLAT_HEAP)

static void *
gcn_memspace_alloc (omp_memspace_handle_t memspace, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool = FLAT_HEAP_PTR;

      return __gcn_lowlat_alloc (shared_pool, size);
    }
  else
    return malloc (size);
}

static void *
gcn_memspace_calloc (omp_memspace_handle_t memspace, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool = FLAT_HEAP_PTR;

      return __gcn_lowlat_calloc (shared_pool, size);
    }
  else
    return calloc (1, size);
}

static void
gcn_memspace_free (omp_memspace_handle_t memspace, void *addr, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool = FLAT_HEAP_PTR;

      __gcn_lowlat_free (shared_pool, addr, size);
    }
  else
    free (addr);
}

static void *
gcn_memspace_realloc (omp_memspace_handle_t memspace, void *addr,
			size_t oldsize, size_t size)
{
  if (memspace == omp_low_lat_mem_space)
    {
      char *shared_pool = FLAT_HEAP_PTR;

      return __gcn_lowlat_realloc (shared_pool, addr, oldsize, size);
    }
  else
    return realloc (addr, size);
}

static inline int
gcn_memspace_validate (omp_memspace_handle_t memspace, unsigned access)
{
  /* Disallow use of low-latency memory when it must be accessible by
     all threads.  */
  return (memspace != omp_low_lat_mem_space
	  || access != omp_atv_all);
}

#define MEMSPACE_ALLOC(MEMSPACE, SIZE, PIN) \
  gcn_memspace_alloc (MEMSPACE, ((void)(PIN), (SIZE)))
#define MEMSPACE_CALLOC(MEMSPACE, SIZE, PIN) \
  gcn_memspace_calloc (MEMSPACE, ((void)(PIN), (SIZE)))
#define MEMSPACE_REALLOC(MEMSPACE, ADDR, OLDSIZE, SIZE, OLDPIN, PIN) \
  gcn_memspace_realloc (MEMSPACE, ADDR, OLDSIZE, \
			((void)(PIN), (void)(OLDPIN), (SIZE)))
#define MEMSPACE_FREE(MEMSPACE, ADDR, SIZE, PIN) \
  gcn_memspace_free (MEMSPACE, ADDR, ((void)(PIN), (SIZE)))
#define MEMSPACE_VALIDATE(MEMSPACE, ACCESS, PIN) \
  gcn_memspace_validate (MEMSPACE, ((void)(PIN), (ACCESS)))

/* The default low-latency memspace implies omp_atv_all, which is incompatible
   with the LDS memory space.  */
#define OMP_LOW_LAT_MEM_ALLOC_INVALID 1

#include "../../allocator.c"
