/* Copyright (c) 2008, 2009, 2010, 2011, 2012
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

typedef shared[] int *sintptr;
shared[] int bupc_206_A[10];
sintptr bupc_206_S;

typedef struct point_name
{
  double x;
  double y;
  int gid;
} point_t;

typedef struct
{
  int n;
  shared[] point_t *points;
} hullinfo_t;

shared[] hullinfo_t *bupc_bug275a_hull;

int *shared bupc_bug53_p;

shared struct point_name *shared bupc_bug645_p;

typedef struct heap_struct
{
  shared struct heap_struct *next;
  size_t size;
  int alloc_seq;
} heap_t;
typedef shared heap_t *heap_p;

shared heap_p global_heap;

void
bupc_bug53_test ()
{
  if (MYTHREAD == 0)
    {
      int i;
      bupc_bug53_p = (int *) malloc (10 * sizeof (int));
      for (i = 0; i < 10; i++)
	bupc_bug53_p[i] = i;
      for (i = 0; i < 10; i++)
	{
	  if (bupc_bug53_p[i] != i)
	    {
	      printf ("Error: mismatch p[%d] != %d"
		      " - BUPC bug53 test failed.\n", i, i);
	      abort ();
	    }
	}
      free (bupc_bug53_p);
    }
}

void
bupc_bug206_test ()
{
  sintptr *local = &bupc_206_S;
  /* Failed with ICE on following assignment.  */
  bupc_206_S = bupc_206_A;
  /* local[0] is an alias for 'S' */
  local[0] = NULL;
  if (bupc_206_S != NULL)
    {
      fprintf (stderr, "%d: Error: S != NULL"
	       " - BUPC bug206 test failed.\n", MYTHREAD);
      abort ();
    }
}

void
bupc_bug275a_test ()
{
  int i = 0;
  point_t *lpoints;
  /* Allocate hull struct on this thread */
  bupc_bug275a_hull = (shared[]hullinfo_t *) upc_alloc (sizeof (hullinfo_t));
  if (!bupc_bug275a_hull)
    {
      fprintf (stderr, "%d: Error: can't allocate hull struct"
	       " - bupc_bug275a test failed.\n", MYTHREAD);
      abort ();
    }
  /* Allocate 10 points in each hull structure.  */
  bupc_bug275a_hull->n = 10;
  bupc_bug275a_hull->points =
    (shared[]point_t *) upc_alloc (10 * sizeof (point_t));
  if (!bupc_bug275a_hull->points)
    {
      fprintf (stderr, "%d: Error: can't allocate points struct"
	       " - bupc_bug275a test failed.\n", MYTHREAD);
      abort ();
    }
  for (i = 0; i < 10; ++i)
    {
      int k = i + 1;
      /* Failed with ICE on references to fields of points structure.  */
      bupc_bug275a_hull->points[i].x = k * 1;
      bupc_bug275a_hull->points[i].y = k * 2;
      bupc_bug275a_hull->points[i].gid = k * 3;
    }
  upc_fence;
  lpoints = (point_t *) bupc_bug275a_hull->points;
  for (i = 0; i < 10; ++i)
    {
      int k = i + 1;
      if (lpoints[i].x != 1 * k || lpoints[i].y != 2 * k
	  || lpoints[i].gid != 3 * k)
	{
	  fprintf (stderr, "%d: Error: lpoints[%d] mismatch"
		   " got: (%lf,%lf,%d) expected: (%d,%d,%d)"
		   " - BUPC bug275a test failed.\n",
		   MYTHREAD, i,
		   lpoints[i].x, lpoints[i].y, lpoints[i].gid,
		   1 * k, 2 * k, 3 * k);
	}
    }
  upc_free (bupc_bug275a_hull->points);
  upc_free (bupc_bug275a_hull);
}

void
bupc_bug645_f (struct point_name x)
{
  if (x.x != 5.0 || x.y != 6.0 || x.gid != 100)
    fprintf (stderr, "%d: Error: point_name mismatch"
            " got: (%0.3lg,%0.3lg,%i) expected: (5,6.100)"
	    " - BUPC bug645 test failed.\n",
            MYTHREAD, x.x, x.y, x.gid);
}

void
bupc_bug645_test ()
{
  if (MYTHREAD == 0)
    {
      bupc_bug645_p = upc_global_alloc (1, sizeof (struct point_name));
      if (!bupc_bug645_p)
	{
	  fprintf (stderr, "Error: upc_global_alloc() call returned NULL"
	                   " - BUPC bug645 test failed.\n");
	  abort ();
	}
      bupc_bug645_p->x = 5.0;
      bupc_bug645_p->y = 6.0;
      bupc_bug645_p->gid = 100;
    }
  upc_barrier;
  bupc_bug645_f (*bupc_bug645_p);
}

struct bupc_bug979_struct
{
  int curid;
  int last;
};

void
bupc_bug979_test ()
{
  shared struct bupc_bug979_struct *G;
  int got, expected;
  G = (shared struct bupc_bug979_struct *)
      upc_alloc (sizeof (struct bupc_bug979_struct));
  G->curid = 1;
  G->last = 90;
  upc_fence;
  /* Failed with ICE on following assignment. */
  G->last += 10;
  got = G->last;
  expected = 100;
  if (got != expected)
    {
      fprintf (stderr, "%d: Error: G->last mismatch, got: %d expected: %d"
	       " - BUPC bug979 test failed.\n", MYTHREAD, got, expected);
      abort ();
    }
}

void
heap_init (shared void *heap_base, size_t heap_size)
{
  heap_p heap;
  heap = (heap_p)heap_base;
  upc_memset (heap, '\0', sizeof (heap_t));
  /* the size of each free list entry includes its overhead. */
  heap->size = heap_size;
  heap->next = NULL;
  heap->alloc_seq = 1;
  /* Failed with ICE on 64-bit target, and "struct" pointer representation.  */
  global_heap = heap;
}

void
heap_merge (heap_p ptr)
{
  /* Failed with ICE, with "struct" pointer representation,
     on reference to ptr->next->alloc_seq.  */
  if (ptr->next)
    {
      const shared void * const next_block =
	(shared void *)((shared [] char *)ptr + ptr->size);
      if ((ptr->next == next_block)
	   && (ptr->alloc_seq == ptr->next->alloc_seq))
	{
	  /* adjacent, merge this block with the next */
	  ptr->size += ptr->next->size;
	  ptr->next = ptr->next->next;
	}
    }
}

#define round_up(x, r) (((x) + (r) - 1)/(r)*(r))
#define HEAP_ALIGN 64

void
heap_test ()
{
  if (!MYTHREAD)
    {
      const size_t sz1 = round_up (sizeof (heap_t) + 195, 64);
      const size_t sz2 = round_up (sizeof (heap_t) + 311, 64);
      const size_t alloc_size = sz1 + sz2;
      heap_p heap_base;
      heap_base = (heap_p)upc_alloc (alloc_size);
      if (!heap_base)
        {
	  fprintf (stderr, "Error: can't allocate heap memory"
	                   " - heap_test failed.\n");
	  abort ();
	}
      heap_init (heap_base, alloc_size);
      if (heap_base != global_heap)
        {
	  fprintf (stderr, "Error: heap_base != global_heap" 
	                   " - heap_test failed.\n");
	  abort ();
	}
      /* Fake allocations of 195 and 311.  */
      heap_base->size = sz1;
      heap_base->alloc_seq = 2;
      heap_base->next = (heap_p)((shared [] char *)heap_base + sz1);
      heap_base->next->size = sz2;
      heap_base->next->alloc_seq = heap_base->alloc_seq;
      heap_base->next->next = NULL;
      heap_merge (heap_base);
      upc_fence;
      if (global_heap->size != alloc_size)
        {
	  fprintf (stderr, "Error: unexpected heap size after merge,"
	                   "got %ld expected %ld"
			   " - heap_test failed.\n",
			   (long)global_heap->size, (long)alloc_size);
	  abort ();
	}
      if (global_heap->alloc_seq != 2)
        {
	  fprintf (stderr, "Error: unexpected heap alloc sequence id after merge,"
	                   "got %d expected %d"
			   " - heap_test failed.\n",
			   global_heap->alloc_seq, 2);
	  abort ();
	}
    }
}

void
test27 ()
{
  bupc_bug53_test ();
  upc_barrier;
  bupc_bug206_test ();
  upc_barrier;
  bupc_bug275a_test ();
  upc_barrier;
  bupc_bug645_test ();
  upc_barrier;
  bupc_bug979_test ();
  upc_barrier;
  heap_test ();
}

int
main ()
{
  test27 ();
  upc_barrier;
  if (!MYTHREAD)
    {
      printf
	("test27: Test miscellaneous pointer-to-shared references - passed.\n");
    }
  return 0;
}
