/* { dg-skip-if "" { *-*-* } } */

#pragma omp requires unified_address

int x;

void foo (void)
{
  #pragma omp target
  x = 1;
}
