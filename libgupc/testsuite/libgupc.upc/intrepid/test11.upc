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
shared int array[FACTOR][THREADS];

void
test11()
{
  int i, j;
  for (i = 0; i < FACTOR; i += 1)
    {
      /* declare, and use a local pointer */
      int *s = (int *)&array[i][MYTHREAD];
      *s = (i+1) * (MYTHREAD + 1);
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (i = 0; i < FACTOR; ++i)
	{
	  for (j = 0; j < THREADS; ++j)
	    {
	      shared int *got;
	      int expected = (i+1) * (j+1);
	      got = (shared int *)&array[i][j];
	      if (*got != expected)
		{
		  fprintf(stderr, "test11: error at element %d. Expected %d, got %d\n",
		    i, expected, *got);
		  abort ();
		}
	    }
	}
      printf ("test11: test shared->local, and shared pointers"
	      " using two-dimensional array - passed.\n");
    }
}

int
main()
{
  test11 ();
  return 0;
}
