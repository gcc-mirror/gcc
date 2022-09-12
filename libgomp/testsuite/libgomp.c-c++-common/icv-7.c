/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL "42" } */

/* This tests the hierarchical usage of ICVs on the host and on devices, i.e. if
   OMP_NUM_TEAMS_DEV_<device_num>, OMP_NUM_TEAMS_DEV, and
   OMP_NUM_TEAMS are not configured, then the value of
   OMP_NUM_TEAMS_ALL should be used for the host as well as for the
   devices.  */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (omp_get_max_teams () != 42)
    abort ();

  int num_devices = omp_get_num_devices () > 3 ? 3 : omp_get_num_devices ();
  for (int i=0; i < num_devices; i++)
    #pragma omp target device (i)
      if (omp_get_max_teams () != 42)
	abort ();

  return 0;
}
