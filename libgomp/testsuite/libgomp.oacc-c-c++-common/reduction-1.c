/* { dg-do run } */

/* Integer reductions.  */

#include <stdlib.h>
#include "reduction.h"

#define ng 8
#define nw 4
#define vl 32

static void
test_reductions (void)
{
  const int n = 10;
  int i;
  int array[n];

  for (i = 0; i < n; i++)
    array[i] = i+1;

  /* Gang reductions.  */
  check_reduction_op (int, +, 0, array[i], num_gangs (ng), gang);
  check_reduction_op (int, *, 1, array[i], num_gangs (ng), gang);
  check_reduction_op (int, &, -1, array[i], num_gangs (ng), gang);
  check_reduction_op (int, |, 0, array[i], num_gangs (ng), gang);
  check_reduction_op (int, ^, 0, array[i], num_gangs (ng), gang);

  /* Worker reductions.  */
  check_reduction_op (int, +, 0, array[i], num_workers (nw), worker);
  check_reduction_op (int, *, 1, array[i], num_workers (nw), worker);
  check_reduction_op (int, &, -1, array[i], num_workers (nw), worker);
  check_reduction_op (int, |, 0, array[i], num_workers (nw), worker);
  check_reduction_op (int, ^, 0, array[i], num_workers (nw), worker);

  /* Vector reductions.  */
  check_reduction_op (int, +, 0, array[i], vector_length (vl), vector);
  check_reduction_op (int, *, 1, array[i], vector_length (vl), vector);
  check_reduction_op (int, &, -1, array[i], vector_length (vl), vector);
  check_reduction_op (int, |, 0, array[i], vector_length (vl), vector);
  check_reduction_op (int, ^, 0, array[i], vector_length (vl), vector);

  /* Combined reductions.  */
  check_reduction_op (int, +, 0, array[i], num_gangs (ng) num_workers (nw)
		      vector_length (vl), gang worker vector);
  check_reduction_op (int, *, 1, array[i], num_gangs (ng) num_workers (nw)
		      vector_length (vl), gang worker vector);
  check_reduction_op (int, &, -1, array[i], num_gangs (ng) num_workers (nw)
		      vector_length (vl), gang worker vector);
  check_reduction_op (int, |, 0, array[i], num_gangs (ng) num_workers (nw)
		      vector_length (vl), gang worker vector);
  check_reduction_op (int, ^, 0, array[i], num_gangs (ng) num_workers (nw)
		      vector_length (vl), gang worker vector);
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

  /* Gang reductions.  */
  check_reduction_op (int, &&, 1, (cmp_val > array[i]), num_gangs (ng),
		      gang);
  check_reduction_op (int, ||, 0, (cmp_val > array[i]), num_gangs (ng),
		      gang);

  /* Worker reductions.  */
  check_reduction_op (int, &&, 1, (cmp_val > array[i]), num_workers (nw),
		      worker);
  check_reduction_op (int, ||, 0, (cmp_val > array[i]), num_workers (nw),
		      worker);

  /* Vector reductions.  */
  check_reduction_op (int, &&, 1, (cmp_val > array[i]), vector_length (vl),
		      vector);
  check_reduction_op (int, ||, 0, (cmp_val > array[i]), vector_length (vl),
		      vector);

  /* Combined reductions.  */
  check_reduction_op (int, &&, 1, (cmp_val > array[i]), num_gangs (ng)
		      num_workers (nw) vector_length (vl), gang worker vector);
  check_reduction_op (int, ||, 0, (cmp_val > array[i]), num_gangs (ng)
		      num_workers (nw) vector_length (vl), gang worker vector);
}

static void
test_reductions_minmax (void)
{
  const int n = 1000;
  int i;
  int array[n];

  for (i = 0; i < n; i++)
    array[i] = i;

  /* Gang reductions.  */
  check_reduction_macro (int, min, n + 1, array[i], num_gangs (ng), gang);
  check_reduction_macro (int, max, -1, array[i], num_gangs (ng), gang);

  /* Worker reductions.  */
  check_reduction_macro (int, min, n + 1, array[i], num_workers (nw), worker);
  check_reduction_macro (int, max, -1, array[i], num_workers (nw), worker);

  /* Vector reductions.  */
  check_reduction_macro (int, min, n + 1, array[i], vector_length (vl),
			 vector);
  check_reduction_macro (int, max, -1, array[i], vector_length (vl), vector);

  /* Combined reductions.  */
  check_reduction_macro (int, min, n + 1, array[i], num_gangs (ng)
			 num_workers (nw) vector_length (vl), gang worker
			 vector);
  check_reduction_macro (int, max, -1, array[i], num_gangs (ng)
			 num_workers (nw) vector_length (vl), gang worker
			 vector);
}

int
main (void)
{
  test_reductions ();
  test_reductions_bool ();
  test_reductions_minmax ();
  return 0;
}
