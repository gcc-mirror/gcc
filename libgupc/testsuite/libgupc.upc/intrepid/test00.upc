/* Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011, 2012
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

#include <upc_strict.h>
#include <stdio.h>
#include <stdlib.h>

shared char x1;
shared short x2;
shared int x3;
shared long long x4;
shared float x5;
shared double x6;

#define CHAR_VAL  0x7f
#define SHORT_VAL 0x1234
#define INT_VAL   0x12345678
#define LL_VAL    0x1234567887654321LL

#define SF_VAL    1193046.0
#define DF_VAL    20015998343868.0

void
test00 ()
{
  if (MYTHREAD == 0)
    {
      x1 = CHAR_VAL;
      x2 = SHORT_VAL;
      x3 = INT_VAL;
      x4 = LL_VAL;
      x5 = SF_VAL;
      x6 = DF_VAL;
    }
  upc_barrier;
  if (x1 != CHAR_VAL)
    {
      fprintf (stderr, "%d: Error %s : %d = %d\n",
               MYTHREAD, "char", x1, CHAR_VAL);
      abort ();
    }
  if (x2 != SHORT_VAL)
    {
      fprintf (stderr, "%d: Error %s : %d = %d\n",
               MYTHREAD, "short", x2, SHORT_VAL);
      abort ();
    }
  if (x3 != INT_VAL)
    {
      fprintf (stderr, "%d: Error %s : %d = %d\n",
               MYTHREAD, "int", x3, INT_VAL);
      abort ();
    }
  if (x4 != LL_VAL)
    {
      fprintf (stderr, "%d: Error %s : %lld = %lld\n",
               MYTHREAD, "long long", x4, LL_VAL);
      abort ();
    }
  if (x5 != SF_VAL)
    {
      fprintf (stderr, "%d: Error %s : %f = %f\n",
               MYTHREAD, "float", x5, SF_VAL);
      abort ();
    }
  if (x6 != DF_VAL)
    {
      fprintf (stderr, "%d: Error %s : %f = %f\n",
               MYTHREAD, "double", x6, DF_VAL);
      abort ();
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      printf ("test00: access shared scalars - passed.\n");
    }
}

int
main ()
{
  test00 ();
  return 0;
}
