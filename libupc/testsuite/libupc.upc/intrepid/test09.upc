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

#define BLKSIZE 25 
#define BLKS_PER_THREAD 4
#define FACTOR (BLKSIZE*BLKS_PER_THREAD)
#define NELEM (FACTOR*THREADS)

shared [BLKSIZE] int array[NELEM];

void
test09()
{
  int i, j;
  for (i = 0; i < BLKS_PER_THREAD; ++i)
    {
      int blk_index = (MYTHREAD + i * THREADS) * BLKSIZE;
      int *block = (int *)&array[blk_index];
      for (j = 0; j < BLKSIZE; ++j)
	{
	  block[j] = blk_index + j + 1;
	}
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (i = 0; i < NELEM; ++i)
	{
	  int got = array[i];
	  int expected = i+1;
	  if (got != expected)
	    {
	      fprintf(stderr, "test09: error at element %d. Expected %d, got %d\n",
		i, expected, got);
	      abort ();
	    }
	}
      printf ("test09: simple blocked array test - passed.\n");
    }
}

int
main()
{
  test09 ();
  return 0;
}
