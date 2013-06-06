/* Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

/* Test Assumptions
   1. A "long double" has at least 64 bits of mantissa, which
      offers 19 significant decimal digits.
   2. The value of PI below has 15 digits of fraction
      and one whole digit, thus 16 significant decimal digits.
   3. There is some round-off in this calculation:
          B[j] = A[j] + (xdouble) 100.0L;
      Thus, we might expect that B[j] drops one significant
      digit here/there.
   4. By allowing for a comparison to 18 significant digits, we
      force the comparison to require at least 4 more significant
      digits than a regular "double" (14 digits), but leave some
      room for accumulated round-off errors.  */

#undef PI
/* PI to 15 places */
#define PI 3.141592653589793L
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

#define MSG_LEN 80

void
test29 ()
{
  int i;
  if (!MYTHREAD)
    {
      xA1 = 100.0L * PI;
      xB1 = 200.0L * PI;
    }
  for (i = MYTHREAD; i < N; i += THREADS)
    {
      A[i] = 100.0L + (xdouble) i * PI;
    }
  upc_barrier;
  for (i = MYTHREAD; i < N; i += THREADS)
    {
      int j = (i + 1) % N;
      B[j] = A[j] + (xdouble) 100.0L;
    }
  if (!MYTHREAD)
    {
      xA2 = xA1;
      xB2 = xB1;
    }
  upc_barrier;
  if (xA1 != 100.0L * PI)
    FAIL ("xA1 != 100.0 * PI");
  if (xB1 != 200.0L * PI)
    FAIL ("xB1 != 200.0 * PI");
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
      xdouble expected = (100.0L + (xdouble) i * PI);
      xdouble got = A[i];
      /* This comparison should be exact. */
      if (got != expected)
	{
	  char msg[MSG_LEN];
	  int n_chars;
	  xdouble rel_err;
	  rel_err = got / expected - 1.0L;
	  if (rel_err < 0.0)
	    rel_err = -rel_err;
	  n_chars = snprintf (msg, MSG_LEN,
	             "A[%d] is: %0.15Lf expected: %0.15Lf error: %0.2Le\n",
		     i, got, expected, rel_err);
	  if (n_chars >= MSG_LEN)
	    sprintf (msg, "A[%d] got != expected: output overflow\n", i);
	  FAIL (msg);
	}
    }
  for (i = MYTHREAD; i < N; i += THREADS)
    {
      xdouble expected = (200.0L + (xdouble) i * PI);
      xdouble got = B[i];
      xdouble rel_err;
      rel_err = got / expected - 1.0L;
      if (rel_err < 0.0)
	rel_err = -rel_err;
      /* This comparison should be within 18 (ie, 3+15) digits. */
      if (rel_err > 1.0e-18)
	{
	  char msg[MSG_LEN];
	  int n_chars;
	  n_chars = snprintf (msg, MSG_LEN,
	    "B[%d] is: %0.15Lf expected: %0.15Lf error: %0.2Le\n",
	    i, got, expected, rel_err);
	  if (n_chars >= MSG_LEN)
	    sprintf (msg, "B[%d] got != expected: output overflow\n", i);
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
