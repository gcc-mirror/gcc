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
      fprintf (stderr, "%d: Error %s : %lld = -4\n", MYTHREAD, "long long", s.x4);
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
