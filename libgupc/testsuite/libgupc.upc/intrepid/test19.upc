/* Copyright (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library test suite.
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
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>

#undef min
#undef max
#undef abs
#undef ceil
#undef round_up

#define min(x,y) (((x) < (y)) ? (x): (y))
#define max(x,y) (((x) > (y)) ? (x): (y))
#define abs(x) (((x) > 0) ? (x): -(x))
#define ceil(x, r) (((x) + (r) - 1)/(r))
#define round_up(x, r) (ceil(x,r)*(r))

#define ALLOC_OVERHEAD 64
#define ALLOC_CHUNK_SIZE 64
#define MIN_ALLOC (ALLOC_OVERHEAD + ALLOC_CHUNK_SIZE)
/* We calculate the largest (default) allocation,
   adjusting for overhead and for the allocation of a lock. */
#define MAX_ALLOC_SIZE (10*1024*1024-ALLOC_OVERHEAD-MIN_ALLOC)
#define AVE_ALLOC_SIZE 10000
/* Given that the sizes are generated randomly, it is unlikely that
   we'll need a table as large as twice the expected average number
   of allocations. Further since we're distributing the allocations
   across threads, the actual maximum is likely much lower than this. */
#define AVE_ALLOCS ceil(MAX_ALLOC_SIZE, AVE_ALLOC_SIZE)
#define MAX_ALLOCS 2*AVE_ALLOCS
/* Given that allocations occur in a 2:1 ratio in the first
   phase and that deallocations occur in a 2:1 ratio in the
   second phase, we would expect on average 3 times the expected
   number of allocations for the number of
   iterations inclusive of allocations and deallocations
   in each phase. */
#define MAX_ITERS 3*AVE_ALLOCS
shared void *ptrs[MAX_ALLOCS];
size_t sizes[MAX_ALLOCS];
/* 'N' records the number of entries in (ptrs, sizes) */
int N;
/* 'N_max' is used in the deallocation phase to record
   the high water value of 'N' */
int N_max;

upc_lock_t *lock;
shared void * strict shared global_ptr;

/* Burn a few random cycles */
void keep_busy ()
{
  volatile long busy_count;
  busy_count = (long)(random() % 20000);
  while (--busy_count > 0) /* loop */ ;
}

void
test19()
{
  shared void *p;
  int i;
  size_t sz;
  size_t alloc_quota;
  /* set random seed so each thread generates a different sequence */
  srandom (MYTHREAD + 1);
  lock = upc_all_lock_alloc();
  if (!lock)
    {
      fprintf (stderr, "Error: upc_all_lock_alloc() failed.\n");
      abort ();
    }
  upc_barrier;
  if (!MYTHREAD) global_ptr = 0;
  upc_barrier;
  /* allocate a big block and/or free it 10 times on each thread */
  for (i = 0; i < 10; ++i)
    {
      /* delay a little to randomize sequencing on the lock */
      keep_busy ();
      upc_lock (lock);
      if (global_ptr)
        {
	  upc_free (global_ptr);
	  global_ptr = 0;
        }
      else
        {
	  global_ptr = upc_global_alloc (THREADS, MAX_ALLOC_SIZE);
	  if (!global_ptr)
	    {
	      fprintf (stderr, "%d: Error: failed to "
	               "allocate large block\n", MYTHREAD);
	      abort ();
	         
	    }
	}
      upc_unlock (lock);
    }
  upc_barrier;
  /* clean up */
  if (!MYTHREAD && global_ptr) upc_free (global_ptr);
  upc_barrier;
  /* The following test proceeds in two phases. The first is an
     allocation phase, where about 2/3's of the time the threads
     will allocate and 1/3 of the time, they'll deallocate.

     Then, once everything is allocated, go into a deallocation
     phase where about 2/3's of the time de-allocations are performed
     and 1/3 of the time they'll be allocated.
     
     Once everything has been deallocated, we verify that we
     can once again allocate the largest possible memory
     segment.  */
  /* Each thread will try to allocate its 'quota' of memory
     space. Note that since most dynamic memory managers
     generate internal fragmentation, it is unlikely that
     each thread will allocate its quota. This is
     unimportant, because what we're after is the test at
     the end to see that all the memory was returned. */
  alloc_quota = MAX_ALLOC_SIZE / THREADS;
  if (MYTHREAD == (THREADS - 1))
    alloc_quota += MAX_ALLOC_SIZE % THREADS;
#ifdef VERBOSE
  setbuf(stdout, 0);
  upc_lock (lock);
  printf ("%d: allocation quota = %d\n", MYTHREAD, (int)alloc_quota);
  upc_unlock (lock);
#endif /* VERBOSE */
  N = 0;
  int iter = 0;
  while (alloc_quota > 0)
    {
      int do_alloc = (random() % 3) < 2;
      if (do_alloc || N == 0 || iter++ > MAX_ITERS)
        {
	  size_t alloc_size;
	  sz = random() % (2 * AVE_ALLOC_SIZE) + 1;
	  alloc_size = ALLOC_OVERHEAD + round_up (sz, ALLOC_CHUNK_SIZE);
	  if ((alloc_size >= alloc_quota)
	      || (alloc_quota < MIN_ALLOC))
	    sz = alloc_quota - ALLOC_OVERHEAD;
	  alloc_size = ALLOC_OVERHEAD + round_up (sz, ALLOC_CHUNK_SIZE);
	  if (alloc_size > alloc_quota)
	    {
	      alloc_quota = 0;
	      continue;
	    }
	  alloc_quota -= alloc_size;
#ifdef VERBOSE
          upc_lock (lock);
	  printf ("%d: allocate %ld, %d\n", MYTHREAD, (long int)sz, (int)alloc_quota);
          upc_unlock (lock);
#endif /* VERBOSE */
	  p = upc_global_alloc (THREADS, sz);
	  if (p)
	    {
	      ptrs[N]    = p;
	      sizes[N++] = sz;
	    }
	  else
	    break;
          if (N == MAX_ALLOCS) break; /* DOB: prevent buffer overrun */
	}
      else
        {
	  int k;
	  /* deallocate a random entry */
	  k = random() % N;
	  p = ptrs[k];
	  sz = sizes[k];
	  for (i = k; i < (N - 1); ++i)
	    {
	      ptrs[i]  = ptrs[i+1];
	      sizes[i] = sizes[i+1];
            }
	  N = N - 1;
#ifdef VERBOSE
          upc_lock (lock);
	  printf ("%d: free %d, %d\n", MYTHREAD, (int)sz, (int)alloc_quota);
          upc_unlock (lock);
#endif /* VERBOSE */
	  upc_free (p);
	  alloc_quota +=  ALLOC_OVERHEAD
	                  + round_up (sz, ALLOC_CHUNK_SIZE);
	}
    }
  upc_barrier;
#ifdef VERBOSE
  upc_lock (lock);
  printf ("%d: finished allocation phase, %ld bytes left\n",
	  MYTHREAD, (long int)alloc_quota);
  upc_unlock (lock);
#endif /* VERBOSE */
  /* deallocation phase */
  N_max = N;
  iter = 0;
  while (N > 0)
    {
      int do_alloc = (random() % 3) < 1;
      iter++;
      if (do_alloc && (N < N_max) && iter < MAX_ITERS)
        {
	  sz = sizes[N];
#ifdef VERBOSE
          upc_lock (lock);
	  printf ("%d: allocate %ld\n", MYTHREAD, (long int)sz);
          upc_unlock (lock);
#endif /* VERBOSE */
	  p = upc_global_alloc (THREADS, sz);
	  if (p)
	    ptrs[N++]    = p;
	  else
	    continue;
	}
      else
        {
	  int k;
	  /* deallocate a random entry */
	  k = random() % N;
	  p = ptrs[k];
	  sz = sizes[k];
	  for (i = k; i < (N_max - 1); ++i)
	    {
	      ptrs[i]  = ptrs[i+1];
	      sizes[i] = sizes[i+1];
            }
	  sizes[N_max-1] = sz;
	  N = N - 1;
#ifdef VERBOSE
          upc_lock (lock);
	  printf ("%d: free %d\n", MYTHREAD, (int)sz);
          upc_unlock (lock);
#endif /* VERBOSE */
	  upc_free (p);
	}
    }
  upc_barrier;
  /* At this point, everything should've been deallocated, and
     we can safely allocate the big block again. */
  if (MYTHREAD == 0)
    {
      global_ptr = upc_global_alloc (THREADS, MAX_ALLOC_SIZE);
      if (!global_ptr)
	{
	  fprintf (stderr, "Error: Failed to allocate large block in final stage.\n");
	  abort ();
	     
	}
      upc_free (global_ptr);
      printf ("test19: intensive global memory allocator test - passed.\n");
    }
}

int
main()
{
  test19 ();
  return 0;
}
