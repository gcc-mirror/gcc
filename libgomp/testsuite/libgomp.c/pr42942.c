/* PR libgomp/42942 */
/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main (void)
{
  int e = 0;
  omp_set_dynamic (0);
  omp_set_nested (1);
  omp_set_max_active_levels (1);
  if (omp_get_max_active_levels () != 1)
    abort ();
#pragma omp parallel num_threads(2) reduction(|:e)
  if (!omp_in_parallel ()
      || omp_get_num_threads () != 2)
    e = 1;
  else
#pragma omp parallel num_threads(2) reduction(|:e)
    if (!omp_in_parallel ()
	|| omp_get_num_threads () != 1)
      e = 1;
  if (e)
    abort ();
  omp_set_max_active_levels (0);
  if (omp_get_max_active_levels () != 0)
    abort ();
#pragma omp parallel num_threads(2) reduction(|:e)
  if (omp_in_parallel ()
      || omp_get_num_threads () != 1)
    e = 1;
  else
#pragma omp parallel num_threads(2) reduction(|:e)
    if (omp_in_parallel ()
	|| omp_get_num_threads () != 1)
      e = 1;
  if (e)
    abort ();
  omp_set_max_active_levels (2);
  if (omp_get_max_active_levels () != 2)
    abort ();
#pragma omp parallel num_threads(2) reduction(|:e)
  if (!omp_in_parallel ()
      || omp_get_num_threads () != 2)
    e = 1;
  else
#pragma omp parallel num_threads(2) reduction(|:e)
    if (!omp_in_parallel ()
	|| omp_get_num_threads () != 2)
      e = 1;
    else
#pragma omp parallel num_threads(2) reduction(|:e)
      if (!omp_in_parallel ()
	  || omp_get_num_threads () != 1)
	e = 1;
  if (e)
    abort ();
  return 0;
}
