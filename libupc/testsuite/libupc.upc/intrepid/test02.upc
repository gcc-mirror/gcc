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


#define DIM1 1024

shared int array[DIM1][THREADS];

void
test02 ()
{
  int i, j;
  for (i = 0; i < DIM1; ++i)
    {
      array[i][MYTHREAD] = (i + 1) * (MYTHREAD + 1);
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (i = 0; i < DIM1; ++i)
	{
	  for (j = 0; j < THREADS; ++j)
	    {
	      int got = array[i][j];
	      int expected = (i + 1) * (j + 1);
	      if (got != expected)
		{
		  fprintf (stderr, "test02: Error at element [%d,%d]. Expected %d, got %d\n",
			   i, j, expected, got);
		  abort ();
		}
	    }
	}
      printf ("test02: simple 2-dimensional array test - passed.\n");
    }
}

int
main ()
{
  test02 ();
  return 0;
}
