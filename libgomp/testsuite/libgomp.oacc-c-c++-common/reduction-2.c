/* { dg-do run } */

/* Ignore vector_length warnings for offloaded (nvptx) targets.  */
/* { dg-additional-options "-foffload=-w" } */

/* float reductions.  */

#include <stdlib.h>
#include "reduction.h"

const int ng = 8;
const int nw = 4;
const int vl = 32;

static void
test_reductions (void)
{
  const int n = 100;
  int i;
  float array[n];

  for (i = 0; i < n; i++)
    array[i] = i+1;

  /* Gang reductions.  */
  check_reduction_op (float, +, 0, array[i], num_gangs (ng), gang);
  check_reduction_op (float, *, 1, array[i], num_gangs (ng), gang);

  /* Worker reductions.  */
  check_reduction_op (float, +, 0, array[i], num_workers (nw), worker);
  check_reduction_op (float, *, 1, array[i], num_workers (nw), worker);

  /* Vector reductions.  */
  check_reduction_op (float, +, 0, array[i], vector_length (vl), vector);
  check_reduction_op (float, *, 1, array[i], vector_length (vl), vector);

  /* Combined reductions.  */
  check_reduction_op (float, +, 0, array[i], num_gangs (ng) num_workers (nw)
		      vector_length (vl), gang worker vector);
  check_reduction_op (float, *, 1, array[i], num_gangs (ng) num_workers (nw)
		      vector_length (vl), gang worker vector);
}

static void
test_reductions_minmax (void)
{
  const int n = 1000;
  int i;
  float array[n];

  for (i = 0; i < n; i++)
    array[i] = i;

  /* Gang reductions.  */
  check_reduction_macro (float, min, n + 1, array[i], num_gangs (ng), gang);
  check_reduction_macro (float, max, -1, array[i], num_gangs (ng), gang);

  /* Worker reductions.  */
  check_reduction_macro (float, min, n + 1, array[i], num_workers (nw),
			 worker);
  check_reduction_macro (float, max, -1, array[i], num_workers (nw), worker);

  /* Vector reductions.  */
  check_reduction_macro (float, min, n + 1, array[i], vector_length (vl),
			 vector);
  check_reduction_macro (float, max, -1, array[i], vector_length (vl), vector);

  /* Combined reductions.  */
  check_reduction_macro (float, min, n + 1, array[i], num_gangs (ng)
			 num_workers (nw) vector_length (vl), gang worker
			 vector);
  check_reduction_macro (float, max, -1, array[i], num_gangs (ng)
			 num_workers (nw)vector_length (vl), gang worker
			 vector);
}

int
main (void)
{
  test_reductions ();
  test_reductions_minmax ();
  return 0;
}
