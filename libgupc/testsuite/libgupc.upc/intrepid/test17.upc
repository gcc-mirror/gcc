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

#include <upc_strict.h>
#include <stdio.h>
#include <stdlib.h>

#define FACTOR 100
#define BLKSIZE 5
shared [BLKSIZE] int array[FACTOR*THREADS];

/* ptr is a shared pointer that lives in shared memory */
shared [BLKSIZE] int * shared ptr;
shared int counter;

/* serialization lock */
upc_lock_t *lock;

void
test17()
{
  int i;
  lock = upc_all_lock_alloc();
  if (!MYTHREAD)
    {
      counter = 0;
      ptr = &array[0];
    }
  upc_barrier;
  for (i = 0; i < FACTOR; ++i)
    {
      upc_lock (lock);
      counter = counter + 1;
      *ptr = counter;
      ptr = ptr + 1;
      upc_unlock (lock);
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      int expected;
      int got;
      shared [BLKSIZE] int *p, *last;
      for (p = &array[0], last = &array[FACTOR*THREADS-1], i = 0;
	   p <= last; ++p)
	{
	  expected = ++i;
	  got = *p;
	  if (got != expected)
	    {
	      fprintf(stderr, "test17: error at element %d. Expected %d, got %d.\n",
		i, expected, got);
	      abort ();
	    }
	}
      got = i;
      expected = FACTOR*THREADS;
      if (got != expected)
        {
	  fprintf(stderr, "test17: error: loop iteration mismatch."
	                  " Expected %d, got %d.\n", expected, got);
	  abort ();
	}
      printf ("test17: test indirection via a shared pointer to shared, "
              "and pointer comparison, for pointers with "
	      "blocking factors - passed.\n");
    }
}

int
main()
{
  test17 ();
  return 0;
}
