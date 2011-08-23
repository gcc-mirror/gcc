/* Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

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


#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_sup.h"
#include "upc_sync.h"
#include "upc_numa.h"

/* There is a local page table for each thread. The
   local page table maps a local page to the location
   where it has been mapped into the thread's memory.  */
typedef void *upc_lpte_t;
typedef upc_lpte_t *upc_lpte_p;
GUPCR_THREAD_LOCAL upc_lpte_p __upc_lpt;

/* To speed things up, the last two unique (page, thread)
   lookups are cached.  See __upc_sptr_to_addr() in upc_sup.h. 
   NOTE: for this to work correctly GUPCR_VM_GLOBAL_SET_SIZE
   must be >=2, otherwise a cached mapped entry might be
   swapped out.  */
GUPCR_THREAD_LOCAL unsigned long __upc_page1_ref, __upc_page2_ref;
GUPCR_THREAD_LOCAL void *__upc_page1_base, *__upc_page2_base;

/* Each thread maintains a series of mapped regions
   of memory that are mapped to specific global pages.
   The Global Map Table (gmt) is indexed by a hashed
   global page number to select a row (associative set)
   of entries that are searched to find a per thread
   mapping to the global page.  All pages that do not
   have affinity with the referencing thread are
   considered to be global.  */
typedef struct upc_gme_struct
  {
    upc_page_num_t global_page_num;
    void *local_page;
  } upc_gme_t;
typedef upc_gme_t *upc_gme_p;
typedef upc_gme_t upc_gme_set_t[GUPCR_VM_GLOGAl_MAP_SET_SIZE];
typedef upc_gme_set_t *upc_gme_set_p;
typedef upc_gme_set_t upc_global_map_t[GUPCR_VM_GLOBAL_MAP_SIZE];
typedef upc_global_map_t *upc_global_map_p;
static GUPCR_THREAD_LOCAL upc_global_map_p __upc_gmt;

/* Record the current value of the number of pages allocated.
   This value is updated to the global value in the UPC info.
   structure whenever an attempt is made to access a page
   whose page number is not less than this current value.  */
GUPCR_THREAD_LOCAL upc_page_num_t __upc_cur_page_alloc;

/* If this thread's idea of how many pages have been allocated
   per thread is less than the actual value stored in the
   UPC information structure, map the additional pages allocated
   for this thread into the local page table.  Mappings
   in the local page table are never unmapped; this ensures that
   conversions from pointer to shared to local pointers work
   as expected.  */

upc_page_num_t
__upc_vm_get_cur_page_alloc ()
{
  const upc_info_p u = __upc_info;
  const upc_page_num_t old_page_alloc = __upc_cur_page_alloc;
  upc_page_num_t alloc_pages, p, pt;
  upc_page_num_t  i, j;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  __upc_acquire_lock (&u->lock);
  /* get the latest value */
  GUPCR_FENCE ();
  __upc_cur_page_alloc = u->cur_page_alloc;
  GUPCR_READ_FENCE ();
  __upc_release_lock (&u->lock);
  alloc_pages = __upc_cur_page_alloc - old_page_alloc;
  if (alloc_pages)
    {
      /* Additional pages have been allocated since we last checked.
         Update the local page table to point to the pages
	 newly allocated to this thread.  We need to map contiguous regions
	 in a single mmap call so that conversions to local pointers
	 work properly.  */
      upc_page_num_t first_page = old_page_alloc;
      upc_page_num_t next_global_page = u->gpt[first_page * THREADS + MYTHREAD];
      upc_page_num_t  region_size = 0;
      for (i = 0; i < alloc_pages; ++i)
	{
	  const upc_page_num_t this_global_page = next_global_page;
          region_size += 1;
	  if (i < (alloc_pages - 1))
	    {
	       /* If not at the last page, then calculate the
		  next global page number, so that it can be checked
		  against the current global page number for contiguity.  */
	       p = old_page_alloc + (i + 1);
	       pt = p * THREADS + MYTHREAD;
	       next_global_page = u->gpt[pt];
	    }
	  if ((i == (alloc_pages - 1))
	      || (next_global_page != (this_global_page + 1)))
	    {
	      /* End of region detected.  Map the current region
	         into the current thread's address space and update
		 the Local Page Table.  */
	      const upc_page_num_t gpn = u->gpt[first_page * THREADS + MYTHREAD];
	      const off_t global_mem_offset = (off_t)gpn * GUPCR_VM_PAGE_SIZE;
	      const void *region_base = mmap ((void *) 0,
	                             region_size * GUPCR_VM_PAGE_SIZE,
				     PROT_READ | PROT_WRITE, MAP_SHARED,
				     u->smem_fd, global_mem_offset);
	      if (region_base == MAP_ERROR)
	        {
		  perror ("UPC runtime error: can't map local region");
		  abort ();
	        }
              /* set affinity to this region */
	      __upc_numa_memory_region_affinity_set (u, MYTHREAD, region_base,
                                                       region_size * GUPCR_VM_PAGE_SIZE);
	      /* Update the local page table */
	      for (j = 0; j < region_size; ++j)
	        {
		  p = first_page + j;
		  __upc_lpt[p] = (void *)((size_t)region_base + j * GUPCR_VM_PAGE_SIZE);
		}
	      first_page += region_size;
	      region_size = 0;
	    }
	}
    }
  return __upc_cur_page_alloc;
}

/* For pages in threads other than the current thread,
   check the Global Map Table (gmt) to see if the page
   has already been mapped into this thread's address
   space.  The lookup is implemented by first converting
   (thread, page) into its corresponding global page number.
   The global page number is then converted into a hash
   index by adding its higher order bits into the low order
   bits and then masking modulo the GMT size.  The hash value
   selects a series of entries which are organized into an
   N-way associative set, sorted in MRU order.  If no matching
   entry is found, then map the appropriate global page and
   update the GMT.  */

static void *
__upc_vm_map_global_page (int t, upc_page_num_t p)
{
  const upc_info_p u = __upc_info;
  const upc_page_num_t pt = p * THREADS + t;
  const upc_page_num_t gpn = u->gpt[pt];
  const upc_page_num_t hash_gpn = ((gpn >> GUPCR_VM_GLOBAL_MAP_BITS) + gpn)
                                    & GUPCR_VM_GLOBAL_MAP_MASK;
  const upc_gme_set_p s = &(*__upc_gmt)[hash_gpn];
  upc_gme_p g;
  upc_page_num_t this_gpn;
  off_t global_offset;
  void *page_base;
  int i, j;
  for (i = 0; i < GUPCR_VM_GLOGAl_MAP_SET_SIZE; ++i)
    {
      g = &(*s)[i];
      this_gpn = g->global_page_num;
      if (gpn == this_gpn)
        {
	  /* Found a matching entry. */
	  page_base = g->local_page;
	  if (i > 0)
	    {
	      const upc_gme_t m = *g;
	      /* Move this match to the front to preserve MRU order.  */
	      for (j = i; j > 0; --j) (*s)[j] = (*s)[j - 1];
	      (*s)[0] = m;
	    }
	  return page_base;
	}
      /* If this entry is empty, then no match.  Exit early
         with 'i' pointing to this entry.  */
      if (this_gpn == GUPCR_VM_PAGE_INVALID)
        break;
    }
  if (i == GUPCR_VM_GLOGAl_MAP_SET_SIZE)
    {
      /* The set is full.  Unmap the last entry. */
      g = &(*s)[GUPCR_VM_GLOGAl_MAP_SET_SIZE - 1];
      page_base = g->local_page;
      if (munmap (page_base, GUPCR_VM_PAGE_SIZE))
        { perror ("UPC runtime error: global unmap"); abort (); }
      /* Decrement 'i' so that it points to the last entry. */
      i = i - 1;
    }
  /* Shift entries in associative set; make room at the front. */
  for (j = i; j > 0; --j) (*s)[j] = (*s)[j - 1];
  /* Map the new entry. */
  global_offset = (off_t)gpn << GUPCR_VM_OFFSET_BITS;
  page_base = mmap ((void *) 0, GUPCR_VM_PAGE_SIZE, PROT_READ | PROT_WRITE,
		      MAP_SHARED, u->smem_fd, global_offset);
  if (page_base == MAP_ERROR)
    { perror ("UPC runtime error: can't map global address"); abort (); }
  /* Add the new entry at the front and return the mapped address.  */
  g = &(*s)[0];
  g->global_page_num = gpn;
  g->local_page = page_base;
  return page_base;
}

/* Initialize the VM system.  Create the Global Page Table
   and initially allocate 'num_init_local_pages' per UPC thread.
   Although the required physical storage is allocated, the initial
   mapping occurs is deferred until each thread initializes.  */

void 
__upc_vm_init (upc_page_num_t num_init_local_pages)
{
  if (!__upc_vm_alloc (num_init_local_pages))
    { perror ("UPC runtime error: can't allocate global memory"); abort (); }
}

/* Per thread VM initialization.  Create the Local Page Table (lpt)
   and the Global Map Table (gmt).  Then update the lpt to reflect
   the initially allocated storage.  */

void
__upc_vm_init_per_thread ()
{
  int i, j;
  __upc_lpt = (upc_lpte_p) calloc (GUPCR_VM_MAX_PAGES_PER_THREAD, sizeof (upc_lpte_t));
  if (!__upc_lpt)
    { perror ("UPC runtime error: can't allocate LPT"); abort (); }
  __upc_gmt = (upc_global_map_p) malloc (sizeof (upc_global_map_t));
  if (!__upc_gmt)
    { perror ("UPC runtime error: can't allocate GMT"); abort (); }
  /* All entries in the global map are initially empty */
  for (i = 0; i < GUPCR_VM_GLOBAL_MAP_SIZE; ++i)
    for (j = 0; j < GUPCR_VM_GLOGAl_MAP_SET_SIZE; ++j)
      {
        upc_gme_p g = &(*__upc_gmt)[i][j];
	g->global_page_num = GUPCR_VM_PAGE_INVALID;
        g->local_page = (void *)0;
      }
  /* Invalidate the page lookup cache keys */
  __upc_page1_ref = GUPCR_VM_PAGE_INVALID;
  __upc_page2_ref = GUPCR_VM_PAGE_INVALID;
  /* Update Local Page Table to reflect initial allocation.  */
  __upc_cur_page_alloc = 0;
  (void) __upc_vm_get_cur_page_alloc ();
}

/* Expand the shared memory file to hold an additional
   'alloc_pages' per thread.  Update the '__upc_cur_page_alloc'
   field in the UPC info. block to reflect the size increase.  */

int
__upc_vm_alloc (upc_page_num_t alloc_pages)
{
  const upc_info_p u = __upc_info;
  upc_page_num_t page_alloc;
  upc_page_num_t new_page_alloc;
  off_t smem_size;
  upc_page_num_t i;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  __upc_acquire_lock (&u->lock);
  GUPCR_FENCE ();
  page_alloc = u->cur_page_alloc;
  GUPCR_READ_FENCE ();
  new_page_alloc = __upc_cur_page_alloc + alloc_pages;
  if (new_page_alloc > GUPCR_VM_MAX_PAGES_PER_THREAD)
    {
      __upc_release_lock (&u->lock);
      return 0;
    }
  smem_size = ((off_t)(new_page_alloc * THREADS)) << GUPCR_VM_OFFSET_BITS;
  if (ftruncate (u->smem_fd, smem_size))
    {
      __upc_release_lock (&u->lock);
      return 0;
    }
  for (i = 0; i < alloc_pages; ++i)
    {
      const upc_page_num_t p = page_alloc + i;
      int t;
      for (t = 0; t < THREADS; ++t)
        {
	  upc_page_num_t pt = p * THREADS + t;
	  u->gpt[pt] = (page_alloc * THREADS) + (alloc_pages * t) + i;
	}
    }
  GUPCR_WRITE_FENCE ();
  u->cur_page_alloc = new_page_alloc;
  GUPCR_FENCE ();
  __upc_release_lock (&u->lock);
  return 1;
}

/* Convert a non-null shared pointer into an address mapped
   in the current thread's address space.  */

void *
__upc_vm_map_addr (upc_shared_ptr_t p)
{
  size_t offset, p_offset;
  upc_page_num_t pn;
  int t;
  void *page_base;
  void *addr;
  offset = GUPCR_PTS_OFFSET(p);
  p_offset = (offset & GUPCR_VM_OFFSET_MASK);
  pn = (offset >> GUPCR_VM_OFFSET_BITS) & GUPCR_VM_PAGE_MASK;
  t = GUPCR_PTS_THREAD(p);
  /* If the page number exceeds the current value maintained
     by the referencing thread, update to the most current value,
     and check again.  */
  if (pn >= __upc_cur_page_alloc)
    {
      __upc_cur_page_alloc = __upc_vm_get_cur_page_alloc ();
      if (pn >= __upc_cur_page_alloc)
        __upc_fatal ("Virtual address in shared address is out of range");
    }
  if (t == MYTHREAD)
    {
      /* A local reference:
         Refer to the Local Page Table to find the proper mapping.  */
      page_base = __upc_lpt[pn];
    }
  else
    {
      /* A global reference to another thread's storage:
         Refer to the cached map entries in the Global Map Table.  */
      page_base = __upc_vm_map_global_page (t, pn);
    }
  /* Update the cached lookup entries. */
  __upc_page2_ref = __upc_page1_ref;
  __upc_page2_base = __upc_page1_base;
  __upc_page1_ref = (pn << GUPCR_THREAD_SIZE) | t;
  __upc_page1_base = page_base;
  addr = page_base + p_offset;
  return addr;
}
