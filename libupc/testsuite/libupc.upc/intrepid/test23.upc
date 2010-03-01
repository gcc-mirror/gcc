/*
UPC tests

Copyright (C) 2006
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
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

struct node_s
  {
    shared struct node_s *next;
    int data;
  };
typedef struct node_s node_t;
typedef shared node_t *node_p;

shared void *
add_ptr(node_p ptr, size_t offset)
{
  return (shared void *)((shared [] char *)ptr + offset);
}

shared node_t S;

void
test23()
{
  node_p node = &S;
  shared void *ptr;
  size_t diff;
  ptr = add_ptr (node, 64);
  if (upc_phaseof (ptr) != 0)
    {
      fprintf (stderr, "Error: phase of (ptr) != 0 equals: %d\n",
                       (int)upc_phaseof(ptr));
      abort ();
    }
  if (upc_threadof (ptr) != 0)
    {
      fprintf (stderr, "Error: thread of (ptr) != 0 equals: %d\n",
                       (int)upc_threadof(ptr));
      abort ();
    }
  diff = (shared [] char *)ptr - (shared [] char *)node;
  if (diff != 64)
    {
      fprintf (stderr, "Error: address of (ptr) - address of (node) != 64  equals: %ld\n", (long)diff);
      abort ();
    }
  upc_barrier;
  if (!MYTHREAD)
    printf ("test23: add integer to (shared void *)  - passed.\n");
}

int
main ()
{
  test23 ();
}
