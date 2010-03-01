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
#include <upc.h>
#include <stdio.h>
#include <stdlib.h>

#define ALLOC_SIZE 0x100000	/* 1mb at the time */
#define NALLOC 5

void
test22 ()
{
  shared char *x[NALLOC];
  shared char *y;
  int cnt = 0;
  int i;

  y = upc_global_alloc (1, 0x100);
  for (i=0; i < NALLOC; i++)
    {
      x[i] = upc_local_alloc (1, ALLOC_SIZE);
      if (x[i]) cnt++;
    }
  if (cnt != 5) 
    {
      fprintf (stderr, "test22: Error: Thread %d allocted only %d local buffers.\n", MYTHREAD, cnt);
      abort ();
    }
  for (i=0; i < NALLOC; i++)
    {
      upc_free(x[i]);
    }
  upc_barrier;
  if (!MYTHREAD)
    printf ("test22: heap local allocation - passed.\n");
  upc_barrier;
  upc_free(y);
}

int
main ()
{
  test22 ();
  return 0;
}
