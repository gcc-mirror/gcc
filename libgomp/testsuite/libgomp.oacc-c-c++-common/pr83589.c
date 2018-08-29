/* { dg-do run } */
/* { dg-set-target-env-var GOMP_NVPTX_JIT "-O0" } */

#define n 32

int
main (void)
{
  int arr_a[n];

#pragma acc parallel copyout(arr_a) num_gangs(1) num_workers(1) vector_length(32)
  {
    #pragma acc loop vector
    for (int m = 0; m < 32; m++)
      ;

    #pragma acc loop vector
    for (int m = 0; m < 32; m++)
      arr_a[m] = 0;
  }
}
