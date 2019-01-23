/* Test large vector lengths.  */

#include <assert.h>

#define n 10000
int a1[n], a2[n];

#define gentest(name, outer, inner)		\
  void name ()					\
  {						\
  long i, j, t1, t2, t3;			\
  _Pragma(outer)				\
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

gentest (test2, "acc parallel loop gang vector_length (128) firstprivate (t1, t2)",
	 "acc loop worker vector reduction(+:t1) reduction(-:t2)")

gentest (test3, "acc parallel loop gang worker vector_length (128) firstprivate (t1, t2)",
	 "acc loop vector reduction(+:t1) reduction(-:t2)")

gentest (test4, "acc parallel loop firstprivate (t1, t2)",
	 "acc loop reduction(+:t1) reduction(-:t2)")


int
main ()
{
  test1 ();
  test2 ();
  test3 ();
  test4 ();

  return 0;
}
