/* Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012
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
      fprintf (stderr, "test22: Error: Thread %d allocated "
               "only %d local buffers.\n", MYTHREAD, cnt);
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
