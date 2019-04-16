/* { dg-set-target-env-var "GOMP_OPENACC_DIM" ":33:" } */
/* { dg-shouldfail "" { openacc_nvidia_accel_selected } } */

#include <stdlib.h>
#include <stdio.h>

int
main (void)
{
#pragma acc parallel vector_length (32)
  {
#pragma acc loop worker
    for (unsigned int i = 0; i < 32; i++)
#pragma acc loop vector
      for (unsigned int j = 0; j < 64; j++)
	;
  }

  return 0;
}
/* { dg-output "The Nvidia accelerator has insufficient resources" { target openacc_nvidia_accel_selected } } */
