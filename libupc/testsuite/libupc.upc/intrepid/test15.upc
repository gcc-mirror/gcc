/*
UPC tests

Copyright (C) 2001 
Written by Gary Funck <gary@intrepid.com>
and Nenad Vukicevic <nenad@intrepid.com>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/
#include <upc_strict.h>
#include <stdio.h>
#include <stdlib.h>

#define FACTOR 100
shared int array[FACTOR*THREADS];

void
test15()
{
  int i;
  shared int *p;
  for (i = MYTHREAD; i < FACTOR*THREADS; i += THREADS)
    {
      array[i] = i+1;
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      p = &array[0];
      for (i = 0; i < FACTOR*THREADS; ++i)
	{
	  int expected = i+1;
	  int got = *p++;
	  if (got != expected)
	    {
	      fprintf(stderr, "test15: error at element %d. Expected %d, got %d\n",
		i, expected, got);
	      abort ();
	    }
	}
      /* C standard allows pointers to last element plus one */
      p = &array[FACTOR*THREADS];
      for (i = FACTOR*THREADS-1; i >= 0; --i)
	{
	  int expected = i+1;
	  int got = *--p;
	  if (got != expected)
	    {
	      fprintf(stderr, "test15: error at element %d. Expected %d, got %d\n",
		i, expected, got);
	      abort ();
	    }
	}
      printf ("test15: test increment/decrement operations on shared pointers - passed.\n");
    }
}

int
main()
{
  test15 ();
  return 0;
}
