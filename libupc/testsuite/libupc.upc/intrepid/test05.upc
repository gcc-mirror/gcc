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
#include <string.h>

struct data_struct
  {
    char x1;
    short x2;
    int x3;
    long long x4;
    int x5[5];
  };

#define FACTOR 10
shared struct data_struct array[FACTOR*THREADS];

void
test05()
{
  int i, j;
  for (i = MYTHREAD; i < FACTOR*THREADS; i += THREADS)
    {
      struct data_struct * const s = (struct data_struct *)&array[i];
      memset (s, '\0',  sizeof (struct data_struct));
      s->x1 = i*4 + 1;
      s->x2 = i*4 + 2;
      s->x3 = i*4 + 3;
      s->x4 = i*4 + 4;
      for (j = 0; j < 5; ++j)
	s->x5[j] = i*4 + j + 5;
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (i = 0; i < FACTOR*THREADS; ++i)
	{
	  struct data_struct got = array[i];
	  struct data_struct expected;
          memset (&expected, '\0',  sizeof (struct data_struct));
	  expected.x1 = i*4 + 1;
	  expected.x2 = i*4 + 2;
	  expected.x3 = i*4 + 3;
	  expected.x4 = i*4 + 4;
	  for (j = 0; j < 5; ++j)
	    expected.x5[j] = i*4 + j + 5;
	  if (memcmp(&got, &expected, sizeof(struct data_struct)))
	    {
	      fprintf(stderr, "test05: error at element %d."
		" Expected (%d,%d,%d,%lld,%d,%d,%d,%d,%d),"
		" got (%d,%d,%d,%lld,%d,%d,%d,%d,%d)\n",
		i, expected.x1, expected.x2, expected.x3, expected.x4,
		expected.x5[0], expected.x5[1], expected.x5[2],
		expected.x5[3], expected.x5[4],
		got.x1, got.x2, got.x3, got.x4,
		got.x5[0], got.x5[1], got.x5[2], got.x5[3], got.x5[4]);
	      abort ();
	    }
	}
      printf ("test05: access structured shared array element\n"
	      "		using a local pointer - passed.\n");
    }
}

int
main()
{
  test05 ();
  return 0;
}
