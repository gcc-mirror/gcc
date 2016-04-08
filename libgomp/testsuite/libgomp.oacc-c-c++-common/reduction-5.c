/* { dg-do run } */
/* { dg-additional-options "-w" } */

/* Ignore vector_length warnings for offloaded (nvptx) targets.  */
/* { dg-additional-options "-foffload=-w" } */

/* Multiple reductions.  */

#include <stdio.h>
#include <stdlib.h>

const int ng = 8;
const int nw = 4;
const int vl = 32;

const int n = 100;

#define DO_PRAGMA(x) _Pragma (#x)

#define check_reduction(gwv_par, gwv_loop)		\
  {							\
  s1 = 2; s2 = 5;					\
DO_PRAGMA (acc parallel gwv_par copy (s1, s2))		\
DO_PRAGMA (acc loop gwv_loop reduction (+:s1, s2))	\
    for (i = 0; i < n; i++)				\
      {							\
         s1 = s1 + 3;					\
         s2 = s2 + 5;					\
      }							\
							\
    if (s1 != v1 && s2 != v2)				\
      abort ();						\
  }

int
main (void)
{
  int s1 = 2, s2 = 5, v1 = 2, v2 = 5;
  int i;

  for (i = 0; i < n; i++)
    {
      v1 = v1 + 3;
      v2 = v2 + 2;
    }

  check_reduction (num_gangs (ng), gang);

  /* Nvptx targets require a vector_length or 32 in to allow spinlocks with
     gangs.  */
  check_reduction (num_workers (nw) vector_length (vl), worker);
  check_reduction (vector_length (vl), vector);
  check_reduction (num_gangs (ng) num_workers (nw) vector_length (vl), gang
		   worker vector);

  return 0;
}
