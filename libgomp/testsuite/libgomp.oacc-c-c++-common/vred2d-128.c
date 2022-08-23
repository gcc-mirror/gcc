/* Test large vector lengths.  */

/* { dg-additional-options "-DEXPENSIVE" { target run_expensive_tests } } */

/* { dg-additional-options -Wuninitialized } */

#include <assert.h>

#ifdef EXPENSIVE
#define n 10000
#else
#define n 2500
#endif
int a1[n], a2[n];

#define gentest(name, outer, inner)		\
  void name ()					\
  {						\
  long i, j, t1, t2, t3; /* { dg-line vars } */	\
  _Pragma(outer) /* { dg-line outer } */	\
  for (i = 0; i < n; i++)			\
    {						\
      t1 = 0;					\
      t2 = 0;					\
      _Pragma(inner)				\
      for (j = i; j < n; j++)			\
	{					\
	  t1++;					\
	  t2--;					\
	}					\
      a1[i] = t1;				\
      a2[i] = t2;				\
    }						\
  for (i = 0; i < n; i++)			\
    {						\
      assert (a1[i] == n-i);			\
      assert (a2[i] == -(n-i));			\
    }						\
  }						\

gentest (test1, "acc parallel loop gang vector_length (128) firstprivate (t1, t2)",
	 "acc loop vector reduction(+:t1) reduction(-:t2)")
/* { dg-warning {'t1' is used uninitialized} {} { target *-*-* } outer }
   { dg-note {'t1' was declared here} {} { target *-*-* } vars }
   { dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-4 }
     TODO See PR101551 for 'offloading_enabled' differences.  */
/* { dg-warning {'t2' is used uninitialized} {} { target *-*-* } outer }
   { dg-note {'t2' was declared here} {} { target *-*-* } vars }
   { DUP_dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-8 }
     TODO See PR101551 for 'offloading_enabled' differences.  */

gentest (test2, "acc parallel loop gang vector_length (128) firstprivate (t1, t2)",
	 "acc loop worker vector reduction(+:t1) reduction(-:t2)")
/* { DUPdg-warning {'t1' is used uninitialized} {} { target *-*-* } outer }
   { DUP_dg-note {'t1' was declared here} {} { target *-*-* } vars }
   { dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-4 }
     TODO See PR101551 for 'offloading_enabled' differences.  */
/* { DUPdg-warning {'t2' is used uninitialized} {} { target *-*-* } outer }
   { DUP_dg-note {'t2' was declared here} {} { target *-*-* } vars }
   { DUP_dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-8 }
     TODO See PR101551 for 'offloading_enabled' differences.  */

gentest (test3, "acc parallel loop gang worker vector_length (128) firstprivate (t1, t2)",
	 "acc loop vector reduction(+:t1) reduction(-:t2)")
/* { DUPdg-warning {'t1' is used uninitialized} {} { target *-*-* } outer }
   { DUP_dg-note {'t1' was declared here} {} { target *-*-* } vars }
   { dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-4 }
     TODO See PR101551 for 'offloading_enabled' differences.  */
/* { DUPdg-warning {'t2' is used uninitialized} {} { target *-*-* } outer }
   { DUP_dg-note {'t2' was declared here} {} { target *-*-* } vars }
   { DUP_dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-8 }
     TODO See PR101551 for 'offloading_enabled' differences.  */

gentest (test4, "acc parallel loop firstprivate (t1, t2)",
	 "acc loop reduction(+:t1) reduction(-:t2)")
/* { DUPdg-warning {'t1' is used uninitialized} {} { target *-*-* } outer }
   { DUP_dg-note {'t1' was declared here} {} { target *-*-* } vars }
   { dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-4 }
     TODO See PR101551 for 'offloading_enabled' differences.  */
/* { DUPdg-warning {'t2' is used uninitialized} {} { target *-*-* } outer }
   { DUP_dg-note {'t2' was declared here} {} { target *-*-* } vars }
   { DUP_dg-note {in expansion of macro 'gentest'} {} { target { ! offloading_enabled } } .-8 }
     TODO See PR101551 for 'offloading_enabled' differences.  */


int
main ()
{
  test1 ();
  test2 ();
  test3 ();
  test4 ();

  return 0;
}
