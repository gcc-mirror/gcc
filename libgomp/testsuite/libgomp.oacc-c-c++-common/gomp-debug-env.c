/* { dg-do run } */
/* { dg-set-target-env-var GOMP_DEBUG "1" } */

/* Check that GOMP_DEBUG=1 triggers some output.  */

int
main (void)
{
#pragma acc parallel
  ;
}

/* { dg-output "GOACC_parallel_keyed" } */
