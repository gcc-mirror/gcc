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

/* This test replicates a problem where incorrect code was generated
   whenever an intervening declaration of a type that has the same type
   as the basic elememt type of a shared array whose dimension included
   a reference to THREADS.  In that situation, the compiler "forgot"
   to scale all references to the array, by THREADS (when compiled in
   the dynamic compilation environment. */

#define FACTOR 100
shared int a[FACTOR][THREADS];
/* following variable is not used, but is required to demosntrate problem */
#if !defined(__GNUC__) && !defined(__attribute__)
#define __attribute__(X) 
#endif
shared int unused_var __attribute__ ((unused));
shared int *Ptr;

void
test12 ()
{
  int i, j;
  int got, expected;
  for (i = 0; i < FACTOR; ++i)
    {
      Ptr = &a[i][MYTHREAD];
      got = upc_threadof(Ptr);
      expected = MYTHREAD;
      if (got != MYTHREAD)
	{
	   fprintf (stderr, "Error at element [%d,%d]."
	     " Address of array element has affinity %d,"
	     " but should have affinity to this thread (%d).\n",
	     i, MYTHREAD, got, expected);
	   abort ();
	}
      *Ptr = i * THREADS + MYTHREAD;
    }
  upc_barrier;
  if (!MYTHREAD)
    {
      for (i = 0; i < FACTOR; ++i)
	{
	  for (j = 0; j < THREADS; ++j)
	    {
	      expected = i * THREADS + j;
	      got = a[i][j];
	      if (got != expected)
		{
		  fprintf (stderr, "Error at element [%d,%d]."
		    " Got `%d', expected `%d'.\n",
		    i, j, got, expected);
		  abort ();
		}
	    }
	}
      printf ("test12: test multi-dimension array accesses\n"
	      " in a dynamic compilation environment - passed.\n");
    }
}

int
main ()
{
  test12 ();
  return 0;
}
