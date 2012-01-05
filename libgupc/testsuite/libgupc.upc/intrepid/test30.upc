/* Copyright (c) 2011, 2012
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
#include <stdint.h>

#if defined(__GCC_UPC__) && defined(__UPC_PTS_STRUCT_REP__)

struct sptr
{
#if __UPC_VADDR_FIRST__
  __UPC_VADDR_TYPE__ vaddr;
  __UPC_THREAD_TYPE__ thread;
  __UPC_PHASE_TYPE__ phase;
#else
  __UPC_PHASE_TYPE__ phase;
  __UPC_THREAD_TYPE__ thread;
  __UPC_VADDR_TYPE__ vaddr;
#endif
}
#ifdef __UPC_PTS_ALIGN__
__attribute__ ((aligned (__UPC_PTS_ALIGN__)))
#endif
  ;

#define EXPECTED_PTS_ALIGN (__alignof (shared int *))

struct upc_struct
{
  size_t len;
  shared int *addr;
};

struct c_struct
{
  size_t len;
  struct sptr addr;
};

struct upc_struct upc_a[10];
struct c_struct c_a[10];

int pass_fail = 1;

void
test30 ()
{
  if (!MYTHREAD)
    {
      if (sizeof (struct upc_struct) != sizeof (struct c_struct))
	{
	  fprintf (stderr,
		   "Error: struct with PTS has different size (%d)\n"
		   "       than the size of a struct\n"
		   "       with an equivalent representation (%d)\n",
		   (int) sizeof (struct upc_struct),
		   (int) sizeof (struct c_struct));
	  pass_fail = 0;
	}
      if (sizeof (upc_a) != sizeof (c_a))
	{
	  fprintf (stderr,
		   "Error: array of structs with PTS has different size (%d)\n"
		   "       than the size of an array of structs\n"
		   "       with an equivalent representation (%d)\n",
		   (int) sizeof (upc_a), (int) sizeof (c_a));
	  pass_fail = 0;
	}
      if (__alignof (upc_a[0].addr) != __alignof (c_a[0].addr))
	{
	  fprintf (stderr,
		   "Error: PTS field of struct has different alignment (%d)\n"
		   "       than the similar field of a struct with\n"
		   "       with an equivalent representation (%d)\n",
		   (int) __alignof (upc_a[0].addr),
		   (int) __alignof (c_a[0].addr));
	  pass_fail = 0;
	}
      if (__alignof (c_a[0].addr) != EXPECTED_PTS_ALIGN)
	{
	  fprintf (stderr,
		   "Error: PTS representation field of a struct\n"
		   "       has different alignment (%d)\n"
		   "       than the expected alignment of a PTS (%d)\n",
		   (int) __alignof (c_a[0].addr), (int) EXPECTED_PTS_ALIGN);
	  pass_fail = 0;
	}
    }
}
#endif

int
main ()
{
#ifdef __GCC_UPC__
  #ifdef __UPC_PTS_STRUCT_REP__
  test30 ();
  upc_barrier;
  if (!MYTHREAD)
    {
      printf ("test30: test GUPC struct PTS alignment: %s.\n",
	      pass_fail ? "passed" : "failed");
      if (!pass_fail)
	abort ();
    }
  #else
  if (!MYTHREAD)
    printf ("test30: test GUPC struct PTS alignment: passed.\n"
	    "(This test applies only to GUPC's struct "
	    "PTS representation)\n");
  #endif
#else
  if (!MYTHREAD)
    printf ("test30: test GUPC struct PTS alignment: non-applicable\n"
	    "(This test must be compiled with GUPC)\n");
#endif
  return 0;
}
