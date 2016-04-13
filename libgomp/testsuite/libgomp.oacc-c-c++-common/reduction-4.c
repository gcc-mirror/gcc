/* { dg-do run { target { ! { hppa*-*-hpux* } } } } */

/* Ignore vector_length warnings for offloaded (nvptx) targets.  */
/* { dg-additional-options "-foffload=-w" } */

/* complex reductions.  */

#include <stdlib.h>
#include <complex.h>
#include "reduction.h"

const int ng = 8;
const int nw = 4;
const int vl = 32;

static void
test_reductions (void)
{
  const int n = 10;
  int i;
  double _Complex array[n];

  for (i = 0; i < n; i++)
    array[i] = i+1;

  /* Gang reductions.  */
  check_reduction_op (double, +, 0, creal (array[i]), num_gangs (ng), gang);
  check_reduction_op (double, *, 1, creal (array[i]), num_gangs (ng), gang);

  /* Worker reductions.  */
  check_reduction_op (double, +, 0, creal (array[i]), num_workers (nw),
		      worker);
  check_reduction_op (double, *, 1, creal (array[i]), num_workers (nw),
		      worker);

  /* Vector reductions.  */
  check_reduction_op (double, +, 0, creal (array[i]), vector_length (vl),
		      vector);
  check_reduction_op (double, *, 1, creal (array[i]), vector_length (vl),
		      vector);

  /* Combined reductions.  */
  check_reduction_op (double, +, 0, creal (array[i]), num_gangs (ng)
			 num_workers (nw) vector_length (vl), gang worker
			 vector);
  check_reduction_op (double, *, 1, creal (array[i]), num_gangs (ng)
			 num_workers (nw) vector_length (vl), gang worker
			 vector);
}

int
main (void)
{
  test_reductions ();
  return 0;
}
