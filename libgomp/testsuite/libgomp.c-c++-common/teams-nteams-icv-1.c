/* Check that the nteams ICV is honored. */
/* PR libgomp/109875  */

/*  This base version of testcases is supposed to be run with all
    OMP_NUM_TEAMS* env vars being unset.

    The variants teams-nteams-icv-{2,3,4}.c test it by setting the
    various OMP_NUM_TEAMS* env vars and #define MY_... for checking.

    Currently, only <num> 0,1,2 is supported for the envar via #define
    and with remote execution, dg-set-target-env-var does not work with
    DejaGNU, hence, gcc/testsuite/lib/gcc-dg.exp marks those tests as
    UNSUPPORTED. */

#define MY_MAX_DEVICES 3

/* OpenMP currently has:
   - nteams-var ICV is initialized to 0; one ICV per device
   - OMP_NUM_TEAMS(_DEV(_<dev-num>)) overrides it
     OMP_NUM_TEAMS_ALL overrides it
   - Number of teams is:
     -> the value specific by num_teams([lower:]upper)
	with lower := upper if unspecified
     -> Otherwise, if nteams-var ICV > 0, #teams <= nteams-var ICV
     -> Otherwise, if nteams-var ICV <= 0, #teams > 1
 GCC uses 3 as default on the host and 1 for host fallback.
 For offloading, it is device specific >> 1.  */

#include <omp.h>

int
main ()
{
  int num_teams_env = -1, num_teams_env_dev = -1;
  int num_teams_env_devs[MY_MAX_DEVICES];

#ifdef MY_OMP_NUM_TEAMS_ALL
  num_teams_env = num_teams_env_dev = MY_OMP_NUM_TEAMS_ALL;
#endif

#ifdef MY_OMP_NUM_TEAMS
  num_teams_env = MY_OMP_NUM_TEAMS;
#endif

#ifdef MY_OMP_NUM_TEAMS_DEV
  num_teams_env_dev = MY_OMP_NUM_TEAMS_DEV;
#endif

#if MY_MAX_DEVICES != 3
  #error "Currently strictly assuming MY_MAX_DEVICES = 3"
#endif

#if defined(MY_OMP_NUM_TEAMS_DEV_4) || defined(MY_OMP_NUM_TEAMS_DEV_5)
  #error "Currently strictly assuming MY_MAX_DEVICES = 3"
#endif

#ifdef MY_OMP_NUM_TEAMS_DEV_0
  num_teams_env_devs[0] = MY_OMP_NUM_TEAMS_DEV_0;
#else
  num_teams_env_devs[0] = num_teams_env_dev;
#endif

#ifdef MY_OMP_NUM_TEAMS_DEV_1
  num_teams_env_devs[1] = MY_OMP_NUM_TEAMS_DEV_1;
#else
  num_teams_env_devs[1] = num_teams_env_dev;
#endif

#ifdef MY_OMP_NUM_TEAMS_DEV_2
  num_teams_env_devs[2] = MY_OMP_NUM_TEAMS_DEV_2;
#else
  num_teams_env_devs[2] = num_teams_env_dev;
#endif

  /* Check that the number of teams (initial device and in target) is
     >= 1 and, if omp_get_max_teams() > 0, it does not
     exceed omp_get_max_teams (). */

  int nteams, num_teams;

  /* Assume that omp_get_max_teams (); returns the ICV, i.e. 0 as default init
     and not the number of teams that would be run; hence: '>='.  */
  nteams = omp_get_max_teams ();
  if (nteams < 0 || (num_teams_env >= 0 && nteams != num_teams_env))
    __builtin_abort ();
  num_teams = -1;

  #pragma omp teams
   if (omp_get_team_num () == 0)
     num_teams = omp_get_num_teams ();
  if (num_teams < 1 || (nteams > 0 && num_teams > nteams))
    __builtin_abort ();

  /* GCC hard codes 3 teams - check for it.  */
  if (nteams <= 0 && num_teams != 3)
    __builtin_abort ();

  /* For each device, including host fallback.  */
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    {
      int num_teams_icv = num_teams_env_dev;
      if (dev == omp_get_num_devices ())
	num_teams_icv = num_teams_env;
      else if (dev < MY_MAX_DEVICES)
	num_teams_icv = num_teams_env_devs[dev];

      nteams = -1;
      #pragma omp target device(dev) map(from: nteams)
	nteams = omp_get_max_teams ();
      if (nteams < 0 || (num_teams_icv >= 0 && nteams != num_teams_icv))
	__builtin_abort ();

      num_teams = -1;
      #pragma omp target teams device(dev) map(from: num_teams)
	if (omp_get_team_num () == 0)
	  num_teams = omp_get_num_teams ();

      if (num_teams < 1 || (nteams > 0 && num_teams > nteams))
	__builtin_abort ();

      /* GCC hard codes 1 team for host fallback - check for it.  */
      if (dev == omp_get_num_devices () && num_teams != 1)
	__builtin_abort ();
    }

  /* Now set the nteams-var ICV and check that omp_get_max_teams()
     returns the set value and that the following holds:
     num_teams >= 1 and num_teams <= nteams-var ICV.

     Additionally, implementation defined, assume:
     - num_teams == (not '<=') nteams-var ICV, except:
     - num_teams == 1 for host fallback.  */

  omp_set_num_teams (5);

  nteams = omp_get_max_teams ();
  if (nteams != 5)
    __builtin_abort ();
  num_teams = -1;

  #pragma omp teams
   if (omp_get_team_num () == 0)
     num_teams = omp_get_num_teams ();
  if (num_teams != 5)
    __builtin_abort ();

  /* For each device, including host fallback.  */
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    {
      #pragma omp target device(dev) firstprivate(dev)
	omp_set_num_teams (7 + dev);

      #pragma omp target device(dev) map(from: nteams)
	nteams = omp_get_max_teams ();
      if (nteams != 7 + dev)
	__builtin_abort ();

      num_teams = -1;
      #pragma omp target teams device(dev) map(from: num_teams)
	if (omp_get_team_num () == 0)
	  num_teams = omp_get_num_teams ();

      if (dev == omp_get_num_devices ())
	{
	  if (num_teams != 1)
	    __builtin_abort ();
	}
      else
	{
	  if (num_teams != 7 + dev)
	    __builtin_abort ();
	}
    }

  /* Now use the num_teams clause explicitly.  */

  num_teams = -1;
  #pragma omp teams num_teams(6)
   if (omp_get_team_num () == 0)
     num_teams = omp_get_num_teams ();
  if (num_teams != 6)
    __builtin_abort ();

  /* For each device, including host fallback.  */
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    {
      num_teams = -1;
      #pragma omp target teams device(dev) map(from: num_teams) num_teams(dev+3)
	if (omp_get_team_num () == 0)
	  num_teams = omp_get_num_teams ();

      /* This must match the set value, also with host fallback.  */
      if (num_teams != 3 + dev)
	__builtin_abort ();
    }

  return 0;
}
