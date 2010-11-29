/*
UPC tests

Copyright (C) 2010
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

typedef long double xdouble;

#define N_PER_THREAD 10000
#define N (N_PER_THREAD * THREADS)
strict shared xdouble A[N];
relaxed shared xdouble B[N];
strict shared xdouble xA1, xA2;
relaxed shared xdouble xB1, xB2;

strict shared int pass_fail = 1;

#define FAIL(msg) \
  { \
    fprintf (stderr, "%s:%d (thread %d) %s\n", \
                     __FILE__, __LINE__, MYTHREAD, msg); \
    pass_fail = 0; \
    abort (); \
  }

void
test29 ()
{
  int i;
  if (!MYTHREAD)
    {
      xA1 = 100.0;
      xB1 = 200.0;
    }
  for (i = MYTHREAD; i < N; i += THREADS)
    {
      A[i] = 100 + i;
    }
  upc_barrier;
  for (i = MYTHREAD; i < N; i += THREADS)
    {
      int j = (i + 1) % N; 
      B[j] = A[j] + 100.0;
    }
  if (!MYTHREAD)
    {
      xA2 = xA1;
      xB2 = xB1;
    }
  upc_barrier;
  if (xA1 != 100.0)
    FAIL ("xA1 != 100.0");
  if (xB1 != 200.0)
    FAIL ("xB1 != 200.0");
  if (xA1 != xA2)
    FAIL ("xA1 != xA2");
  if (xB1 != xB2)
    FAIL ("xB1 != xB2");
  if (xA1 >= xB1)
    FAIL ("xA1 >= xB1");
  if (xA2 >= xB2)
    FAIL ("xA2 >= xB2");
  if (xB1 <= xA1)
    FAIL ("xB1 <= xA1");
  if (xB2 <= xA2)
    FAIL ("xB2 <= xA2");
  for (i = MYTHREAD; i < N; i += THREADS)
    {
      int expected = (100 + i);
      xdouble got = A[i];
      if (got != expected)
	{
	  char msg[1000];
	  sprintf (msg, "A[%d] is: %0.1f expected: %0.1f",
		   i, (float) got, (float) expected);
	  FAIL (msg);
	}
    }
  for (i = MYTHREAD; i < N; i += THREADS)
    {
      int expected = (200 + i);
      xdouble got = B[i];
      if (got != expected)
	{
	  char msg[1000];
	  sprintf (msg, "B[%d] is: %0.1f expected: %0.1f",
		   i, (float) got, (float) expected);
	  FAIL (msg);
	}
    }
}

int
main ()
{
  test29 ();
  upc_barrier;
  if (!MYTHREAD)
    printf ("test29: test shared access to long double type: %s.\n",
	    pass_fail ? "passed" : "failed");
  return 0;
}
