/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_0 "42" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_1 "43" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_2 "44" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL "45" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV "46" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS "47" } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (omp_get_max_teams () != 47)
    abort ();

  int num_devices = omp_get_num_devices () > 3 ? 3 : omp_get_num_devices ();
  for (int i=0; i < num_devices; i++)
    #pragma omp target device (i)
      if (omp_get_max_teams () != 42 + i)
	abort ();

  return 0;
}
