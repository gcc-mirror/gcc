/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

/* Multiple reductions.  */

#include <stdio.h>
#include <stdlib.h>

#define ng 8
#define nw 4
#define vl 32

const int n = 100;

#define DO_PRAGMA(x) _Pragma (#x) /* { dg-line pragma_loc } */

#define check_reduction(gwv_par, gwv_loop)		\
  {							\
  s1 = 2; s2 = 5;					\
DO_PRAGMA (acc parallel gwv_par copy (s1, s2)) /* { dg-line DO_PRAGMA_loc } */ \
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
  check_reduction (num_workers (nw) vector_length (vl), worker); /* { dg-line check_reduction_loc } */
  /* { dg-warning "22:region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } pragma_loc }
     { dg-note "1:in expansion of macro 'DO_PRAGMA'" "" { target *-*-* xfail offloading_enabled } DO_PRAGMA_loc }
     { dg-note "3:in expansion of macro 'check_reduction'" "" { target *-*-* xfail offloading_enabled } check_reduction_loc }
     TODO See PR101551 for 'offloading_enabled' XFAILs.  */
  check_reduction (vector_length (vl), vector);
  check_reduction (num_gangs (ng) num_workers (nw) vector_length (vl), gang
		   worker vector);

  return 0;
}
