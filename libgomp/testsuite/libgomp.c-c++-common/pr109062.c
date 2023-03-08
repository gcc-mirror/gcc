/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  omp_display_env (1);

  return 0;
}

/* { dg-output ".*\\\[host] GOMP_SPINCOUNT = '300000'.*" { target native } } */
