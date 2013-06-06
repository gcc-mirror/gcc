/* Copyright (C) 2001-2013 Free Software Foundation, Inc.
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
shared int array[FACTOR*THREADS];

void
test06()
{
  int i;
  for (i = MYTHREAD; i < FACTOR*THREADS; i += THREADS)
    {
      /* declare, and use a local pointer */
      int *s = (int *)&array[i];
      *s = i+1;
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (i = 0; i < FACTOR*THREADS; ++i)
	{
	  shared int *got;
	  int expected = i+1;
	  got = (shared int *)&array[i];
	  if (*got != expected)
	    {
	      fprintf(stderr, "test06: error at element %d. Expected %d, got %d\n",
		i, expected, *got);
	      abort ();
	    }
	}
      printf ("test06: test shared->local, and shared pointers - passed.\n");
    }
}

int
main()
{
  test06 ();
  return 0;
}
