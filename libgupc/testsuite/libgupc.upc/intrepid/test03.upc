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

struct data_struct
{
  char x1;
  short x2;
  int x3;
  long long x4;
};

shared struct data_struct s;

void
test03 ()
{
  if (MYTHREAD == 0)
    {
      s.x1 = 127;
      s.x2 = -2;
      s.x3 = -3;
      s.x4 = -4;
    }
  upc_barrier;
  if (s.x1 != 127)
    {
      fprintf (stderr, "%d: Error %s : %d = 255\n", MYTHREAD, "char", s.x1);
      abort ();
    }
  if (s.x2 != -2)
    {
      fprintf (stderr, "%d: Error %s : %d = -2\n", MYTHREAD, "short", s.x2);
      abort ();
    }
  if (s.x3 != -3)
    {
      fprintf (stderr, "%d: Error %s : %d = -3\n", MYTHREAD, "int", s.x3);
      abort ();
    }
  if (s.x4 != -4)
    {
      fprintf (stderr, "%d: Error %s : %lld = -4\n",
               MYTHREAD, "long long", s.x4);
      abort ();
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      printf ("test03 (access shared struct) - passed.\n");
    }
}

int
main ()
{
  test03 ();
  return 0;
}
