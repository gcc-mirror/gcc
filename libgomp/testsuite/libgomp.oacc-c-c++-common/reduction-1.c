/* { dg-do run } */

/* Integer reductions.  */

#include <stdlib.h>
#include <stdbool.h>

#define vl 32

#define DO_PRAGMA(x) _Pragma (#x)

#define check_reduction_op(type, op, init, b)	\
  {						\
    type res, vres;				\
    res = (init);				\
DO_PRAGMA (acc parallel vector_length (vl))\
DO_PRAGMA (acc loop reduction (op:res))\
    for (i = 0; i < n; i++)			\
      res = res op (b);				\
						\
    vres = (init);				\
    for (i = 0; i < n; i++)			\
      vres = vres op (b);			\
						\
    if (res != vres)				\
      abort ();					\
  }

static void
test_reductions_int (void)
{
  const int n = 1000;
  int i;
  int array[n];

  for (i = 0; i < n; i++)
    array[i] = i;

  check_reduction_op (int, +, 0, array[i]);
  check_reduction_op (int, *, 1, array[i]);
  check_reduction_op (int, &, -1, array[i]);
  check_reduction_op (int, |, 0, array[i]);
  check_reduction_op (int, ^, 0, array[i]);
}

static void
test_reductions_bool (void)
{
  const int n = 1000;
  int i;
  int array[n];
  int cmp_val;

  for (i = 0; i < n; i++)
    array[i] = i;

  cmp_val = 5;
  check_reduction_op (bool, &&, true, (cmp_val > array[i]));
  check_reduction_op (bool, ||, false, (cmp_val > array[i]));
}

#define check_reduction_macro(type, op, init, b)	\
  {							\
    type res, vres;					\
    res = (init);					\
DO_PRAGMA (acc parallel vector_length (vl))\
DO_PRAGMA (acc loop reduction (op:res))\
    for (i = 0; i < n; i++)				\
      res = op (res, (b));				\
							\
    vres = (init);					\
    for (i = 0; i < n; i++)				\
      vres = op (vres, (b));				\
							\
    if (res != vres)					\
      abort ();						\
  }

#define max(a, b) (((a) > (b)) ? (a) : (b))
#define min(a, b) (((a) < (b)) ? (a) : (b))

static void
test_reductions_minmax (void)
{
  const int n = 1000;
  int i;
  int array[n];

  for (i = 0; i < n; i++)
    array[i] = i;

  check_reduction_macro (int, min, n + 1, array[i]);
  check_reduction_macro (int, max, -1, array[i]);
}

int
main (void)
{
  test_reductions_int ();
  test_reductions_bool ();
  test_reductions_minmax ();
  return 0;
}
