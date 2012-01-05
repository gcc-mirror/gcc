/* Copyright (c) 2003, 2004, 2005, 2006, 2007, 2008, 2009,
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
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

void
test14 ()
{
  int shared * res; 
  int *p;
  res = 0; 
  upc_barrier;
  if (MYTHREAD == 0)
    {
      if (res)
        {
	  fprintf (stderr, "Error: null PTS test 'if (res)' failed.\n");
	  abort ();
        }
      if (res != 0)
        {
	  fprintf (stderr, "Error: null PTS test 'if (res !=0)' failed.\n");
	  abort ();
	}
      p = (int *)res;
      if (p)
        {
	  fprintf (stderr, "Error: null PTS to local test 'if (p)' failed.\n");
	  abort ();
	}
      if ((int *) res != 0)
        {
	  fprintf (stderr, "Error: null PTS to local "
	           "test 'if ((int *) res != 0)' failed.\n");
	  abort ();
	}
      if (0 != (int *) res)
        {
	  fprintf (stderr, "Error: null PTS to local "
	           "test 'if (0 != (int *) res)' failed.\n");
	  abort ();
	}
      printf ("test14: test assignment, comparison, and "
              "conversion of a null shared pointer - passed.\n");
    }
}

int main ()
{
   test14 ();
   return 0;
}
