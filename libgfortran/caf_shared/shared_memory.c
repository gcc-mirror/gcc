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
#include "libcoarraynative.h"

#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

/* This implements shared memory based on POSIX mmap.  We start with
   memory block of the size of the global shared memory data, rounded
   up to one pagesize, and enlarge as needed.

   We address the memory via a shared_memory_ptr, which is an offset into
   the shared memory block. The metadata is situated at offset 0.

   In order to be able to resize the memory and to keep pointers
   valid, we keep the old mapping around, so the memory is actually
   visible several times to the process.  Thus, pointers returned by
   shared_memory_get_mem_with_alignment remain valid even when
   resizing.  */

/* Global metadata for shared memory, always kept at offset 0.  */

typedef struct
{
  size_t size;
  size_t used;
  int fd;
} global_shared_memory_meta;

/* Type realization for opaque type shared_memory.  */

typedef struct shared_memory_act
{
  global_shared_memory_meta *meta;
  void *header;
  size_t last_seen_size;

  /* We don't need to free these. We probably also don't need to keep
     track of them, but it is much more future proof if we do.  */

  size_t num_local_allocs;

  struct local_alloc
  {
    void *base;
    size_t size;
  } allocs[];

} shared_memory_act;

/* Convenience wrapper for mmap.  */

static inline void *
map_memory (int fd, size_t size, off_t offset)
{
  void *ret
      = mmap (NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, offset);
  if (ret == MAP_FAILED)
    {
      perror ("mmap failed");
      exit (1);
    }
  return ret;
}

/* Returns the size of shared_memory_act.  */

static inline size_t
get_shared_memory_act_size (int nallocs)
{
  return sizeof (shared_memory_act) + nallocs * sizeof (struct local_alloc);
}

/* When the shared memory block is enlarged, we need to map it into
   virtual memory again.  */

static inline shared_memory_act *
new_base_mapping (shared_memory_act *mem)
{
  shared_memory_act *newmem;
  /* We need another entry in the alloc table.  */
  mem->num_local_allocs++;
  newmem = realloc (mem, get_shared_memory_act_size (mem->num_local_allocs));
  newmem->allocs[newmem->num_local_allocs - 1] = ((struct local_alloc){
      .base = map_memory (newmem->meta->fd, newmem->meta->size, 0),
      .size = newmem->meta->size });
  newmem->last_seen_size = newmem->meta->size;
  return newmem;
}

/* Return the most recently allocated base pointer.  */

static inline void *
last_base (shared_memory_act *mem)
{
  return mem->allocs[mem->num_local_allocs - 1].base;
}

/* Get a pointer into the shared memory block with alignemnt
   (works similar to sbrk).  */

shared_mem_ptr
shared_memory_get_mem_with_alignment (shared_memory_act **pmem, size_t size,
				      size_t align)
{
  shared_memory_act *mem = *pmem;
  size_t new_size;
  size_t orig_used;

  /* Offset into memory block with alignment.  */
  size_t used_wa = alignto (mem->meta->used, align);

  if (used_wa + size <= mem->meta->size)
    {
      memset (last_base (mem) + mem->meta->used, 0xCA,
	      used_wa - mem->meta->used);
      memset (last_base (mem) + used_wa, 0x42, size);
      mem->meta->used = used_wa + size;

      return (shared_mem_ptr){ .offset = used_wa };
    }

  /* We need to enlarge the memory segment.  Double the size if that
     is big enough, otherwise get what's needed.  */

  if (mem->meta->size * 2 > used_wa + size)
    new_size = mem->meta->size * 2;
  else
    new_size = round_to_pagesize (used_wa + size);

  orig_used = mem->meta->used;
  mem->meta->size = new_size;
  mem->meta->used = used_wa + size;
  ftruncate (mem->meta->fd, mem->meta->size);
  /* This also sets the new base pointer where the shared memory
     can be found in the address space.  */

  mem = new_base_mapping (mem);

  *pmem = mem;
  assert (used_wa != 0);

  memset (last_base (mem) + orig_used, 0xCA, used_wa - orig_used);
  memset (last_base (mem) + used_wa, 0x42, size);

  return (shared_mem_ptr){ .offset = used_wa };
}

/* If another image changed the size, update the size accordingly.  */

void
shared_memory_prepare (shared_memory_act **pmem)
{
  shared_memory_act *mem = *pmem;
  if (mem->meta->size == mem->last_seen_size)
    return;
  mem = new_base_mapping (mem);
  *pmem = mem;
}

/* Initialize the memory with one page, the shared metadata of the
   shared memory is stored at the beginning.  */

void
shared_memory_init (shared_memory_act **pmem)
{
  shared_memory_act *mem;
  int fd;

  /* Darwin does not appear to be able to grow shared memory segments.  Choose
     256 GB; that will likely be enough.  If not, the ftruncate will fail
     noisily.  */

#ifdef __APPLE__
  size_t initial_size = ((size_t) 1) << 38;
#else
  size_t initial_size = round_to_pagesize (sizeof (global_shared_memory_meta));
#endif

  mem = malloc (get_shared_memory_act_size (1));
  fd = get_shmem_fd ();

  ftruncate (fd, initial_size);
  mem->meta = map_memory (fd, initial_size, 0);
  *mem->meta = ((global_shared_memory_meta){
      .size = initial_size,
      .used = sizeof (global_shared_memory_meta),
      .fd = fd });
  mem->last_seen_size = initial_size;
  mem->num_local_allocs = 1;
  mem->allocs[0]
      = ((struct local_alloc){ .base = mem->meta, .size = initial_size });

  *pmem = mem;
}

/* Convert a shared memory pointer (i.e. an offset into the shared
   memory block) to a pointer.  */

void *
shared_mem_ptr_to_void_ptr (shared_memory_act **pmem, shared_mem_ptr smp)
{
  return last_base (*pmem) + smp.offset;
}
