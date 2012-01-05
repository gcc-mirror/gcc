/* Copyright (c) 2008, 2009, 2010, 2011, 2012
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

shared float f1 = 1.0;
shared float f9 = 9.0;

typedef struct { double real; double imag; } dcomplex;
shared dcomplex complex_1_2 = { 1.0, 2.0 };

/* ICE: In function '__upc_init_decls':
   internal compiler error: in upc_shared_addr,  */
shared int * shared ptr = NULL;

void
test25 ()
{
   if (!MYTHREAD)
     {
       int error_flag = 0;
       float f_expected, f_got;
       double dr_expected, dr_got;
       double di_expected, di_got;
       f_expected = 1.0; f_got = f1;
       if (f_got != f_expected)
	 {
	   fprintf (stderr, "Error: f1 initialization failed, "
	            "expected: %0.4g got: %0.4g\n", f_expected, f_got);
	   error_flag = 1;
	 }
       f_expected = 9.0; f_got = f9;
       if (f_got != f_expected)
	 {
	   fprintf (stderr, "Error: f9 initialization failed, "
	            "expected: %0.4g got: %0.4g\n", f_expected, f_got);
	   error_flag = 1;
	 }
       dr_expected = 1.0; dr_got = complex_1_2.real;
       di_expected = 2.0; di_got = complex_1_2.imag;
       if ((dr_got != dr_expected) || (di_got != di_expected))
	 {
	   fprintf (stderr, "Error: complex_1_2 initialization failed, "
	   "expected: (%0.4lg,%0.4lg) got: (%0.4lg,%0.4lg)\n",
	   dr_expected, di_expected, dr_got, di_got);
	   error_flag = 1;
	 }
      if (ptr)
        {
	   fprintf (stderr, "Error: ptr initialization failed, "
	            " expected NULL pointer\n");
	   error_flag = 1;
	}
      if (error_flag)
	abort();
    }
}

int
main()
{
  test25 ();
  if (MYTHREAD == 0)
    printf ("test25: UPC initialization test - passed.\n");
  return 0;
}
