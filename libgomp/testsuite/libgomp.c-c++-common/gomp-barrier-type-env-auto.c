/* { dg-do run } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "verbose" } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "auto" } */

int
main (void)
{
  return 0;
}

/* { dg-output ".*\\\[host] GOMP_BARRIER_TYPE = 'auto'.*\\\[host] GOMP_FORKJOIN_BARRIER_TYPE = 'auto'.*" { target native } } */
