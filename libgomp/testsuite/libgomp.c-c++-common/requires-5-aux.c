/* { dg-skip-if "" { *-*-* } } */

#pragma omp requires unified_shared_memory, unified_address, reverse_offload

int x;

void foo (void)
{
  #pragma omp target
  x = 1;
}
