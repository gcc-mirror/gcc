/* Copyright (C) 2012-2024 Free Software Foundation, Inc.

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

/* This file is part of the vtable verification runtime library.  It
   contains our memory allocation and deallocation routines, which we
   use in order to keep track of the pages in memory in which our sets
   of valid vtable pointes are stored.  (We need to know the pages so
   we can set the protections on them appropriately).  For more
   information about the vtable verification feature, see the comments
   in vtv_rts.cc.  We use the existing obstack implementation in our
   memory allocation scheme.  */

#include <stdlib.h>
#include <unistd.h>
#if defined (__CYGWIN__) || defined (__MINGW32__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/mman.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>

#include "vtv_utils.h"
#include "vtv_malloc.h"
#include "obstack.h"

/* The following variables are used only for debugging and performance tuning
   purposes. Therefore they do not need to be "protected".  They cannot be used
   to attack the vtable verification system and if they become corrupted it will
   not affect the correctness or security of any of the rest of the vtable
   verification feature.  */

unsigned int num_calls_to_mprotect = 0;
unsigned int num_pages_protected = 0;
unsigned int long long mprotect_cycles = 0;

/* Put the following variables in our ".vtable_map_vars" section so
   that they are protected.  They are explicitly unprotected and
   protected again by calls to __vtv_unprotect and __vtv_protect */

static struct obstack vtv_obstack VTV_PROTECTED_VAR;
static void *current_chunk VTV_PROTECTED_VAR = 0;
static size_t current_chunk_size VTV_PROTECTED_VAR = 0;
static int malloc_initialized VTV_PROTECTED_VAR = 0;

#if defined (__CYGWIN__) || defined (__MINGW32__)
//sysconf(_SC_PAGE_SIZE) port
long sysconf_SC_PAGE_SIZE()
{
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  long pageSize = (long)si.dwPageSize;
  return pageSize;
  //return 4096; // standard usermode 32bit pagesize in bytes // FIXME
}
#endif

/* The function goes through and counts all the pages we have allocated
   so far.  It returns the page count.  */

int
__vtv_count_mmapped_pages (void)
{
  int count = 0;
  struct _obstack_chunk * ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
    {
      count++;
      ci = ci->prev;
    }

  return count;
}

/* This function goes through all of the pages we have allocated so
   far and calls mprotect to change the protections on the pages,
   according to the value of PROTECTION_FLAG.  */

static void
change_protections_on_data_chunks (int protection_flag)
{
  struct _obstack_chunk *ci;
  ci = (struct _obstack_chunk *) current_chunk;

  while (ci)
    {
      /* Initial set up for mprotect call.*/
      struct _obstack_chunk *protect_start = ci;
      size_t chunk_size;
      size_t total_size;
      unsigned int num_pages_in_chunk;
      char *next_page;
      unsigned long long start, end;
      int result;


      /* As long as the next 'chunk' is adjacent to the current one,
         keep going down the list.  */
      do
        {
          chunk_size = (ci->limit - (char *) ci);
          total_size = (ci->limit - (char *) protect_start);
          num_pages_in_chunk = chunk_size / VTV_PAGE_SIZE;
          if (chunk_size % VTV_PAGE_SIZE > 0)
            num_pages_in_chunk++;
          next_page = (char *) ci + (num_pages_in_chunk * VTV_PAGE_SIZE);
          ci = ci->prev;
        } while (ci && (char *) ci == next_page);

      VTV_DEBUG_ASSERT (((unsigned long) protect_start & (VTV_PAGE_SIZE - 1))
                                                                       == 0);

      /* Protect the contiguous chunks so far.  */
      start = rdtsc ();
      result = mprotect (protect_start, total_size, protection_flag);
      end = rdtsc ();
      mprotect_cycles += end - start;
      if (result == -1)
        VTV_error ();
      num_calls_to_mprotect++;
      num_pages_protected += (total_size + VTV_PAGE_SIZE - 1)/ VTV_PAGE_SIZE;
    }

#ifdef VTV_DEBUG
    __vtv_malloc_dump_stats ();
#endif
}

/* This function makes all of our allocated pages read-only.  */

void
__vtv_malloc_protect (void)
{
  change_protections_on_data_chunks (PROT_READ);
}

/* This function makes all of our allocated pages read-write.  */

void
__vtv_malloc_unprotect (void)
{
  change_protections_on_data_chunks (PROT_READ | PROT_WRITE);
}

/* Allocates a SIZE-sized chunk of memory that is aligned to a page
   boundary.  The amount of memory requested (SIZE) must be a multiple
   of the page size.  Note: We must use mmap to allocate the memory;
   using malloc here will cause problems.  */

static void *
obstack_chunk_alloc (size_t size)
{
  /* Increase size to the next multiple of VTV_PAGE_SIZE.   */
  size = (size + (VTV_PAGE_SIZE - 1)) & (~(VTV_PAGE_SIZE - 1));
  VTV_DEBUG_ASSERT ((size & (VTV_PAGE_SIZE - 1)) == 0);
  void *allocated;

#if defined (__CYGWIN__) || defined (__MINGW32__)
  if ((allocated = VirtualAlloc(NULL, size,  MEM_RESERVE|MEM_COMMIT,
                         PAGE_READWRITE)) == 0)
#else
  if ((allocated = mmap (NULL, size, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS,  -1, 0)) == 0)
#endif
    VTV_error ();

  VTV_DEBUG_ASSERT (((unsigned long) allocated & (VTV_PAGE_SIZE - 1)) == 0);

  current_chunk = allocated;
  current_chunk_size = size;
  return allocated;
}

static void
obstack_chunk_free (void *)
{
  /* Do nothing. For our purposes there should be very little
     de-allocation. */
}

/* This function sets up and initializes the obstack pieces for our
   memory allocation scheme.  */

void
__vtv_malloc_init (void)
{
  /* Make sure we only execute the main body of this function ONCE.  */
  if (malloc_initialized)
    return;

#if defined (__CYGWIN__) || defined (__MINGW32__)
  if (VTV_PAGE_SIZE != sysconf_SC_PAGE_SIZE())
#else
  if (VTV_PAGE_SIZE != sysconf (_SC_PAGE_SIZE))
#endif
    VTV_error ();

  /* We guarantee that the obstack alloc failed handler will never be
     called because in case the allocation of the chunk fails, it will
     never return */
  obstack_alloc_failed_handler = NULL;

  obstack_specify_allocation (&vtv_obstack, VTV_PAGE_SIZE, sizeof (long),
			      obstack_chunk_alloc, obstack_chunk_free);
  malloc_initialized = 1;
}

/* This is our external interface for the memory allocation.  SIZE is
   the requested number of bytes to be allocated/  */

void *
__vtv_malloc (size_t size)
{
  return obstack_alloc (&vtv_obstack, size);
}


/* This is our external interface for memory deallocation.  */

void
__vtv_free (void *)
{
  /* Do nothing. We dont care about recovering unneded memory at this
     time.  */
}


/* This is a debugging function tat collects statistics about our
   memory allocation.  */
void
__vtv_malloc_stats (void)
{
  int count = 0;
  struct _obstack_chunk * ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
    {
      count++;
      ci = ci->prev;
    }
  fprintf (stderr,
           "__vtv_malloc_stats:\n  Page Size = %lu bytes\n  "
           "Number of pages = %d\n", static_cast<unsigned long>(VTV_PAGE_SIZE),
	   count);
}

/* This is a debugging function.  It writes out our memory allocation
   statistics to a log file.  */

void
__vtv_malloc_dump_stats (void)
{
  static int fd = -1;

  if (fd == -1)
    fd = __vtv_open_log ("vtv_mem_protection.log");
  if (fd == -1)
    return;

  int count = 0;
  struct _obstack_chunk * ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
    {
      count++;
      ci = ci->prev;
    }

  __vtv_add_to_log (fd, "__vtv_malloc_protect protected=%d pages\n", count);
}
