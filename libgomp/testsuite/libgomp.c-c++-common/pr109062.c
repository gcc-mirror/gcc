/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  omp_display_env (1);

  return 0;
}

/* On hybrid x86-64, i.e. with P and E cores, the default is GOMP_SPINCOUNT=1;
   hence, handle either value; see PR109812. */
/* { dg-output ".*\\\[host] GOMP_SPINCOUNT = '(?:300000|1)'.*" { target { native && { x86_64-*-* i?86-*-* } } } } */

/* { dg-output ".*\\\[host] GOMP_SPINCOUNT = '300000'.*" { target { native && { ! { x86_64-*-* i?86-*-* } } } } } */
