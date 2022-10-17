/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_1234567890 "42" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_ "43" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_01 "44" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_a "45" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_12345678901 "46" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_-1 "47" } */
/* { dg-set-target-env-var "OMP_NUM_TEAMS_DEV_ 1" "48" } */
/* { dg-set-target-env-var "OMP_NUM_TEAMS_DEV_00" "49" } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  return 0;
}

/* { dg-output ".*Invalid device number in OMP_NUM_TEAMS_DEV_=43.*" { target native } } */
/* { dg-output ".*Invalid device number in OMP_NUM_TEAMS_DEV_01=44.*" { target native } } */
/* { dg-output ".*Invalid device number in OMP_NUM_TEAMS_DEV_a=45.*" { target native } } */
/* { dg-output ".*Invalid device number in OMP_NUM_TEAMS_DEV_12345678901=46.*" { target native } } */
/* { dg-output ".*Invalid device number in OMP_NUM_TEAMS_DEV_-1=47.*" { target native } } */
/* { dg-output ".*Invalid device number in OMP_NUM_TEAMS_DEV_ 1=48.*" { target native } } */
/* { dg-output ".*Invalid device number in OMP_NUM_TEAMS_DEV_00=49.*" { target native } } */
