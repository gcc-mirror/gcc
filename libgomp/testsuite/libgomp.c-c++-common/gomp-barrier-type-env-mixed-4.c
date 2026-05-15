/* { dg-do run } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "verbose" } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "centralized" } */
/* { dg-set-target-env-var GOMP_FORKJOIN_BARRIER_TYPE "tree" } */

int
main (void)
{
  return 0;
}

/* { dg-output ".*\\\[host] GOMP_BARRIER_TYPE = 'centralized'.*\\\[host] GOMP_FORKJOIN_BARRIER_TYPE = 'tree'.*" { target native } } */
