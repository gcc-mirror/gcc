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


void
set_proc (shared char *p1, char v1, shared short *p2,
	  short v2, shared int *p3, int v3, shared long long *p4,
	  long long v4)
{
  *p1 = v1;
  *p2 = v2;
  *p3 = v3;
  *p4 = v4;
}

void
get_proc (shared char *p1, char *v1, shared short *p2,
	  short *v2, shared int *p3, int *v3, shared long long *p4,
	  long long *v4)
{
  *v1 = *p1;
  *v2 = *p2;
  *v3 = *p3;
  *v4 = *p4;
}

void
test04 ()
{
  char xv1;
  short xv2;
  int xv3;
  long long xv4;
  if (MYTHREAD == 0)
    {
      set_proc (&x1, 127, &x2, -2, &x3, -3, &x4, -4);
    }
  upc_barrier;
  get_proc (&x1, &xv1, &x2, &xv2, &x3, &xv3, &x4, &xv4);
  if (xv1 != 127)
    {
      fprintf (stderr, "%d: Error %s : %d = 127\n", MYTHREAD, "char", xv1);
      abort ();
    }
  if (xv2 != -2)
    {
      fprintf (stderr, "%d: Error %s : %d = -2\n", MYTHREAD, "short", xv2);
      abort ();
    }
  if (xv3 != -3)
    {
      fprintf (stderr, "%d: Error %s : %d = -3\n", MYTHREAD, "int", xv3);
      abort ();
    }
  if (xv4 != -4)
    {
      fprintf (stderr, "%d: Error %s : %lld = -4\n", MYTHREAD, "long long",
	       xv4);
      abort ();
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      printf ("test04 (access shared values via"
              " (shared *) parameters) - passed.\n");
    }
}

int
main ()
{
  test04 ();
  return 0;
}
