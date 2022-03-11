/* Verify that a simple, explicit acc loop reduction works inside
 a kernels region.  */

/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

#include <stdlib.h>

#define N 100

int
main ()
{
  int i, red = 0;
  /*TODO <https://gcc.gnu.org/PR104892> */
  (volatile int *) &red;

#pragma acc kernels /* { dg-line l_compute1 } */
  /* { dg-note {variable 'red\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute1 } */
  {
#pragma acc loop reduction (+:red) /* { dg-line l_loop_i1 } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i1 } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i1 } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i1 } */
  for (i = 0; i < N; i++)
    red++;
  }

  if (red != N)
    abort ();

  return 0;
}
