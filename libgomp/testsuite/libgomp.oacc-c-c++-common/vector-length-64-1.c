#include <stdlib.h>
#include <stdio.h>

int
main (void)
{
#pragma acc parallel vector_length (64) num_workers (16) /* { dg-warning "using .num_workers \\(15\\)., ignoring 16" "" { target openacc_nvidia_accel_selected } } */
  {
#pragma acc loop worker
    for (unsigned int i = 0; i < 32; i++)
#pragma acc loop vector
      for (unsigned int j = 0; j < 64; j++)
	;
  }

  return 0;
}
