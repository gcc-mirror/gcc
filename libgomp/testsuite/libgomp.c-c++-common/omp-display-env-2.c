/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS "42" } */

/* This test checks if omp_display_env outputs the initial ICV values although
   the value was updated.  */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  omp_display_env (1);
  omp_set_num_teams (24);
  if (omp_get_max_teams () != 24)
    abort ();
  omp_display_env (1);

  return 0;
}

/* { dg-output ".*\\\[host] OMP_NUM_TEAMS = '42'.*\\\[host] OMP_NUM_TEAMS = '42'" { target native } } */
