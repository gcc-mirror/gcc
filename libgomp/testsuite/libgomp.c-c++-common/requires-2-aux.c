/* { dg-skip-if "" { *-*-* } } */

#pragma omp requires reverse_offload

int x;

void foo (void)
{
  #pragma omp target
  x = 1;
}
