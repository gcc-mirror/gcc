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


#include <upc.h>
#ifdef __sgi__
/* UPC's definitions conflict with definitions in SGI's
   header files, which are included by upc_config.h.  */
#undef barrier
#undef fence
#endif /* __sgi__ */
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#define DEBUG_ALLOC 1
#undef DEBUG_ALLOC

/* upc_alloc.upc implements UPC's dynamic memory allocation
   routines.  The implementation is written in UPC, because
   it needs to run above the runtime library's memory mapping
   facility.  Internal runtime locks are used rather than
   the UPC language-defined locks, because those locks
   depend upon dynamic memory management, and we need to
   break the circular dependency.  */

typedef struct upc_heap_struct
  {
    shared struct upc_heap_struct *next;   /* MUST BE FIRST FIELD */
    size_t size;
    int alloc_tag;
    int is_global;
    int alloc_seq;
  } upc_heap_t;
typedef shared upc_heap_t *upc_heap_p;
#define GUPCR_HEAP_OVERHEAD GUPCR_ROUND (sizeof (upc_heap_t), GUPCR_HEAP_ALLOC_MIN)

static shared upc_heap_p __upc_global_heap;
static shared upc_heap_p __upc_local_heap[THREADS];
static shared void * shared __upc_all_alloc_val;
static shared int __upc_alloc_seq;

#undef NULL
#define NULL (shared void *)0

typedef union _pts_as_rep
  {
    shared void *pts;
    upc_shared_ptr_t rep;
  } pts_as_rep_t;

/* Create a shared pointer, given (addrfield, thread)  */
static inline
shared void *
__upc_alloc_build_pts (size_t addrfield, size_t thread)
{
  pts_as_rep_t r;
  r.pts = NULL;
  GUPCR_PTS_SET_VADDR  (r.rep, addrfield);
  GUPCR_PTS_SET_THREAD (r.rep, thread);
  return r.pts;
}

/* Increment a shared pointer, by nbytes */
static inline
shared void *
__upc_alloc_ptr_add (shared void *ptr, ptrdiff_t nbytes)
{
  return (shared void *)(((shared [] char *)ptr) + nbytes);
}

#ifdef DEBUG_ALLOC
static
char *
__upc_alloc_sptostr (shared void *p)
{
  static char s[100];
  sprintf (s, "(0x%012lx,0x%02x,0x%016lx)",
    (long unsigned int)upc_phaseof(p), (unsigned int)upc_threadof(p),
    (long unsigned int)upc_addrfield(p));
  return s;
}
#endif /* DEBUG_ALLOC */

/* upc_heap_init() is called from the runtime to initially
   create the heap.  Heap_base is the virtual address
   of where the heap should begin, and heap_size is the
   initial heap_size.  The caller has already allocated
   the underlying space.  Note that the lower level
   heap manager doesn't use locks -- all locking must
   be done at a higher level.  */

void
__upc_heap_init (upc_shared_ptr_t heap_base, size_t heap_size)
{
  int t;
  upc_heap_p heap;
  heap = *((upc_heap_p *)&heap_base);
  upc_memset (heap, '\0', sizeof (upc_heap_t));
  __upc_alloc_seq = 0;
  /* the size of each free list entry includes its overhead. */
  heap->size = heap_size;
  heap->next = NULL;
  heap->is_global = 1;
  heap->alloc_seq = ++__upc_alloc_seq;
  __upc_global_heap = heap;
  for (t = 0; t < THREADS; ++t)
    __upc_local_heap[t] = NULL;
}

/* Allocate a block of size 'alloc_size' identified indirectly
   via 'heap_p'.  'alloc_size' must include the heap overhead.
   The 'global_flag' is simply copied into the newly allocated
   heap node.  A pointer to the heap node is returned.  */

static
upc_heap_p
__upc_heap_alloc (shared upc_heap_p *heap_p, size_t alloc_size,
                    int global_flag)
{
  shared upc_heap_p *p;
  upc_heap_p alloc;
#ifdef DEBUG_ALLOC
  printf ("%d: --> __upc_heap_alloc (%ld): heap on entry\n", MYTHREAD, (long int) alloc_size);
  for (p = heap_p; *p; p = (shared upc_heap_p *)&(*p)->next)
    printf("%d: addr: %s size: %ld global: %d seq: %d\n", MYTHREAD, __upc_alloc_sptostr(*p),(long int)(*p)->size,(*p)->is_global,(*p)->alloc_seq);
#endif /* DEBUG_ALLOC */
  for (p = heap_p; *p && ((*p)->size < alloc_size);
       p = (shared upc_heap_p *)&(*p)->next) /* loop */ ;
  alloc = *p;
  if (alloc)
    {
      size_t this_size = alloc->size;
      size_t rem = this_size - alloc_size;
      alloc->is_global = global_flag;
      alloc->alloc_tag = GUPCR_HEAP_ALLOC_TAG;
      /* make sure the remaining fragment meets min. size requirement */
      if (rem < (GUPCR_HEAP_ALLOC_MIN + GUPCR_HEAP_OVERHEAD))
	{
 	  alloc_size = this_size;
	  rem = 0;
	}
      alloc->size = alloc_size;
      if (rem > 0)
	{
	  /* link the remainder onto the free list */
	  upc_heap_p frag = __upc_alloc_ptr_add (alloc, alloc_size);
	  frag->next = alloc->next;
	  frag->alloc_seq = alloc->alloc_seq;
	  frag->is_global = alloc->is_global;
	  frag->alloc_tag = 0;
	  frag->size = rem;
	  *p = frag;
	}
      else
	{
	  /* entry exactly fits, delink this free list entry */
	  *p = alloc->next;
	}
#ifdef DEBUG_ALLOC
  printf ("%d:   __upc_heap_alloc: heap on exit\n", MYTHREAD);
  for (p = heap_p; *p; p = ( shared upc_heap_p *)&(*p)->next)
    printf("%d: addr: %s size: %ld global: %d seq: %d\n",MYTHREAD,__upc_alloc_sptostr(*p),(long int)(*p)->size,(*p)->is_global,(*p)->alloc_seq);
#endif /* DEBUG_ALLOC */
    }
#ifdef DEBUG_ALLOC
  printf ("%d: <- __upc_heap_alloc: %s\n", MYTHREAD, __upc_alloc_sptostr (alloc));
#endif /* DEBUG_ALLOC */
  return alloc;
}

static
void
__upc_heap_free (shared upc_heap_p *heap_p, upc_heap_p ptr)
{
  shared upc_heap_p *p;
  upc_heap_p prev;
#ifdef DEBUG_ALLOC
  printf ("%d: --> __upc_heap_free: ", MYTHREAD);
  printf("%d: addr: %s size: %ld global: %d seq: %d\n", MYTHREAD,
    __upc_alloc_sptostr(ptr),(long int)ptr->size,ptr->is_global,ptr->alloc_seq);
  printf ("%d:   heap on entry\n", MYTHREAD);
  for (p = heap_p; *p; p = ( shared upc_heap_p *)&(*p)->next)
    printf("%d: addr: %s size: %ld global: %d seq: %d\n", MYTHREAD, __upc_alloc_sptostr(*p),(long int)(*p)->size,(*p)->is_global,(*p)->alloc_seq);
#endif /* DEBUG_ALLOC */
  for (p = heap_p, prev = NULL; *p && (ptr > *p);
       prev = *p, p = (shared upc_heap_p *)&(*p)->next) /* loop */ ;
  ptr->alloc_tag = 0;
  ptr->next = *p;
  *p = ptr;
  if (ptr->next && (ptr->next == __upc_alloc_ptr_add (ptr, ptr->size))
      && (ptr->alloc_seq == ptr->next->alloc_seq))
    {
      /* adjacent, merge this block with the next */
      ptr->size += ptr->next->size;
      ptr->next =  ptr->next->next;
    }
  if (prev && (ptr  == __upc_alloc_ptr_add (prev, prev->size))
      && (ptr->alloc_seq == prev->alloc_seq))
    {
      /* adjacent, merge this block with previous */
      prev->size += ptr->size;
      prev->next =  ptr->next;
    }
#ifdef DEBUG_ALLOC
  printf ("%d: <- __upc_heap_free: heap on exit\n", MYTHREAD);
  for (p = heap_p; *p; p = ( shared upc_heap_p *)&(*p)->next)
    printf("%d: addr: %s size: %ld global: %d seq: %d\n",MYTHREAD,__upc_alloc_sptostr(*p),(long int)(*p)->size,(*p)->is_global,(*p)->alloc_seq);
#endif /* DEBUG_ALLOC */
}


/* Allocate a block of size 'alloc_size' from the global heap.
   Extend the heap if more space is needed.  'alloc_size' is
   the size of the heap node returned, inclusive of overhead.  */

static
upc_heap_p
__upc_global_heap_alloc (size_t alloc_size)
{
  shared upc_heap_p *heap_p = &__upc_global_heap;
  upc_heap_p alloc;
#ifdef DEBUG_ALLOC
  printf ("%d: -> __upc_global_heap_alloc (%ld)\n", MYTHREAD, (long int)alloc_size);
#endif /* DEBUG_ALLOC */
  alloc = __upc_heap_alloc (heap_p, alloc_size, 1);
  if (!alloc)
    {
      /* Extend the heap.  */
      const size_t chunk_size = GUPCR_ROUND (alloc_size,
                                          GUPCR_HEAP_CHUNK_SIZE);
      const size_t vm_alloc_size = GUPCR_ROUND (chunk_size, GUPCR_VM_PAGE_SIZE);
      const upc_page_num_t vm_alloc_pages = vm_alloc_size / GUPCR_VM_PAGE_SIZE;
      const upc_page_num_t cur_page_alloc = __upc_vm_get_cur_page_alloc ();
      const size_t new_alloc_base = (size_t)cur_page_alloc * GUPCR_VM_PAGE_SIZE;
      const upc_heap_p new_alloc = __upc_alloc_build_pts (new_alloc_base, 0);
#ifdef DEBUG_ALLOC
      printf ("%d: __upc_global_heap_alloc: extend heap by %d pages\n",
         MYTHREAD, vm_alloc_pages);
#endif /* DEBUG_ALLOC */
      if (!__upc_vm_alloc (vm_alloc_pages))
        return NULL;
      upc_memset (new_alloc, '\0', sizeof (upc_heap_t));
      new_alloc->size = vm_alloc_size;
      new_alloc->next = NULL;
      new_alloc->is_global = 1;
      new_alloc->alloc_seq = ++__upc_alloc_seq;;
      /* Return the newly allocated space to the heap.  */
      __upc_heap_free (heap_p, new_alloc);
      alloc = __upc_heap_alloc (heap_p, alloc_size, 1);
      if (!alloc)
        __upc_fatal ("insufficient UPC dynamic shared memory");
    }
#ifdef DEBUG_ALLOC
  printf ("%d: <- __upc_global_heap_alloc: %s\n", MYTHREAD, __upc_alloc_sptostr (alloc));
#endif /* DEBUG_ALLOC */
  return alloc;
}

static
shared void *
__upc_global_alloc (size_t size)
{
  shared void *mem = NULL;
  if (size)
    {
      const size_t alloc_size = GUPCR_ROUND (size + GUPCR_HEAP_OVERHEAD,
                                          GUPCR_HEAP_ALLOC_MIN);
      upc_heap_p alloc;
      __upc_acquire_alloc_lock ();
      alloc = __upc_global_heap_alloc (alloc_size);
      __upc_release_alloc_lock ();
      if (alloc)
        mem = __upc_alloc_ptr_add (alloc, GUPCR_HEAP_OVERHEAD);
#ifdef DEBUG_ALLOC
      printf ("%d: <- __upc_global_alloc: %s\n", MYTHREAD, __upc_alloc_sptostr(mem));
#endif /* DEBUG_ALLOC */
    }
  return mem;
}

static
inline
shared void *
__upc_local_alloc (size_t size)
{
  shared void *mem = NULL;
#ifdef DEBUG_ALLOC
  printf ("%d: --> __upc_local_alloc (%ld)\n", MYTHREAD,(long int)size);
#endif /* DEBUG_ALLOC */
  if (size)
    {
      const size_t alloc_size = GUPCR_ROUND (size + GUPCR_HEAP_OVERHEAD,
                                          GUPCR_HEAP_ALLOC_MIN);
      shared upc_heap_p *heap_p = &__upc_local_heap[MYTHREAD];
      upc_heap_p alloc;
      __upc_acquire_alloc_lock ();
      alloc = __upc_heap_alloc (heap_p, alloc_size, 0);
      if (!alloc)
	{
	  int chunk_seq;
	  int t;
	  size_t chunk_size = GUPCR_ROUND (size + GUPCR_HEAP_OVERHEAD,
                                                  GUPCR_HEAP_CHUNK_SIZE);
	  upc_heap_p chunk = __upc_global_heap_alloc (chunk_size);
	  if (!chunk)
	    return NULL;
	  chunk_size = chunk->size;
	  chunk_seq = chunk->alloc_seq;
	  /* distribute this chunk over each local free list */
	  for (t = 0; t < THREADS; ++t)
	    {
	      shared upc_heap_p *local_heap_p = &__upc_local_heap[t];
	      /* Set the thread to 't' so that we can link
	         this chunk onto the thread's local heap.  */
	      upc_heap_p local_chunk = __upc_alloc_build_pts (
		                          upc_addrfield (chunk), t);
	      upc_fence;
	      /* add this local chunk onto the local free list */
	      upc_memset (local_chunk, '\0', sizeof (upc_heap_t));
	      local_chunk->size = chunk_size;
	      local_chunk->alloc_seq = chunk_seq;
	      __upc_heap_free (local_heap_p, local_chunk);
	    }
	  alloc = __upc_heap_alloc (heap_p, alloc_size, 0);
	}
      __upc_release_alloc_lock ();
      if (alloc)
        mem = __upc_alloc_ptr_add (alloc, GUPCR_HEAP_OVERHEAD);
    }
#ifdef DEBUG_ALLOC
  printf ("%d: <-- __upc_local_alloc: %s\n", MYTHREAD, __upc_alloc_sptostr (mem));
#endif /* DEBUG_ALLOC */
  return mem;
}

shared void *
upc_global_alloc (size_t nblocks, size_t nbytes)
{
  size_t request_size = GUPCR_ROUND(nblocks, THREADS) * nbytes;
  size_t alloc_size = request_size / THREADS;
  shared void *mem = __upc_global_alloc (alloc_size);
  return mem;
}

shared void *
upc_all_alloc (size_t nblocks, size_t nbytes)
{
  size_t request_size = GUPCR_ROUND(nblocks, THREADS) * nbytes;
  size_t alloc_size = request_size / THREADS;
  shared void *mem = NULL;
  if (alloc_size)
    {
      upc_barrier -1;
      if (MYTHREAD == 0)
        __upc_all_alloc_val = __upc_global_alloc (alloc_size);
      upc_barrier -1;
      mem = __upc_all_alloc_val;
    }
  return mem;
}

/* upc_local_alloc is deprecated, but supported in this implementation. */

shared void *
upc_local_alloc (size_t nblocks, size_t nbytes)
{
  size_t alloc_size = nblocks * nbytes;
  shared void *mem = NULL;
  if (alloc_size)
    mem = __upc_local_alloc (alloc_size);
  return mem;
}

shared void *
upc_alloc (size_t nbytes)
{
  shared void *mem = NULL; 
  if (nbytes)
    mem = __upc_local_alloc (nbytes);
  return mem;
}

void
upc_free (shared void *ptr)
{
  if (ptr)
    {
      const size_t offset __attribute__ ((unused)) = upc_addrfield (ptr);
      const int thread = (int)upc_threadof (ptr);
      const size_t phase = upc_phaseof (ptr);
      shared upc_heap_p *heap_p;
      upc_heap_p thisp;
      if (phase || thread >= THREADS)
        __upc_fatal ("upc_free() called with invalid shared pointer");
      thisp = (upc_heap_p) __upc_alloc_ptr_add (ptr, -GUPCR_HEAP_OVERHEAD);
      if (thisp->is_global && thread)
        __upc_fatal ("upc_free() called with invalid shared pointer");
      if (thisp->alloc_tag != GUPCR_HEAP_ALLOC_TAG)
	__upc_fatal ("upc_free() called with pointer to unallocated space");
      if (thisp->is_global)
        heap_p = (shared upc_heap_p *)&__upc_global_heap;
      else
        heap_p = (shared upc_heap_p *)&__upc_local_heap[thread];
      __upc_acquire_alloc_lock ();
      __upc_heap_free (heap_p, thisp);
      __upc_release_alloc_lock ();
    }
}
