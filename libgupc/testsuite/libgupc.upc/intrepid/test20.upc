/* Copyright (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
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

typedef struct {
  double real;
  double imag;
} dcomplex;

#define NTDIVNP 65536

typedef struct dcomplex_cell_s dcomplex_cell_t;
struct dcomplex_cell_s {
  dcomplex cell[NTDIVNP];
};

shared dcomplex_cell_t *sh0;
shared dcomplex_cell_t *sh1;

void
test20 ()
{
  int i;
  sh0 = (shared dcomplex_cell_t *)
        upc_all_alloc (THREADS, sizeof (dcomplex_cell_t));
  sh1 = (shared dcomplex_cell_t *)
        upc_all_alloc (THREADS, sizeof (dcomplex_cell_t));
  for (i = 0; i < NTDIVNP; ++i)
    {
      sh0[MYTHREAD].cell[i].real = (double)(MYTHREAD * NTDIVNP + i); 
      sh0[MYTHREAD].cell[i].imag = -sh0[MYTHREAD].cell[i].real;
    }
  upc_barrier;
  /* block copy from shared memory to local slice, same thread. */
  upc_memget ((dcomplex *)&sh1[MYTHREAD].cell[0],
             &sh0[MYTHREAD].cell[0], sizeof(dcomplex) * NTDIVNP);
  upc_barrier;
  for (i = 0; i < NTDIVNP; ++i)
    {
      double expected_real = (double)(MYTHREAD * NTDIVNP + i);
      double expected_imag = -expected_real;
      double got_real = sh1[MYTHREAD].cell[i].real;
      double got_imag = sh1[MYTHREAD].cell[i].imag;
      if (got_real != expected_real)
        {
	  fprintf (stderr, "%d: Error mismatch at %i"
	           "- real, expected: %7.0lf got: %7.0lf\n",
	           MYTHREAD, i, expected_real, got_real);
          abort ();
	}
      if (got_imag != expected_imag)
        {
	  fprintf (stderr, "%d: Error mismatch at %i"
	           "- imag, expected: %7.0lf got: %7.0lf\n",
	           MYTHREAD, i, expected_imag, got_imag);
          abort ();
	}
    }
  upc_barrier;
  if (MYTHREAD == 0)
    printf ("test20: block copy via upc_memget test passed.\n");
  upc_barrier;
}

int main ()
{
  test20 ();
  return 0;
}
