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
#include <stdio.h>
#include <stdlib.h>

/* The size needs to be large enough to cross several pages
   of the underlying runtime implemented virtual memory.  */
#define BUF_SIZE 31419265

void
test21 ()
{
  shared char *ptr;
  shared [] char *str;
  char *s;
  int k;
  ptr = upc_all_alloc (THREADS, BUF_SIZE);
  if (!ptr)
    {
      fprintf (stderr, "%d: Error: upc_all_alloc() failed\n",
	       MYTHREAD);
      abort ();
    }
  upc_barrier;
  /* Verify that the local addresses are sequential and that
     they map back to the remote addresses */
  s = (char *)&ptr[MYTHREAD]; /* local <- shared */
  str = (shared [] char *)&ptr[MYTHREAD];
  for (k = 0; k < BUF_SIZE; ++k)
    {
      void *local_addr = (void *)&s[k];
      void *remote_to_local_addr = (void *)&str[k];
      if (local_addr != remote_to_local_addr)
	{
	  fprintf (stderr, "%d: Error: address compare for size %ld failed at index %d\n"
	    "    Local address %016lx != %016lx vaddr = %016lx\n",
	    MYTHREAD, (long int)BUF_SIZE, k,
	    (long unsigned)local_addr,
	    (long unsigned)remote_to_local_addr,
	    (long unsigned)upc_addrfield(
	       &(((shared [] char *)&ptr[MYTHREAD])[k])));
	  abort ();
	}
    }
  upc_barrier;
}

int
main ()
{
  test21 ();
  if (MYTHREAD == 0)
    printf ("test21: global/local address check - passed\n");
  return 0;
}
