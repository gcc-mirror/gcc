/* { dg-skip-if "" { *-*-* } } */

#pragma omp requires unified_address

int x;

void foo (void)
{
  x = 1;
  #pragma omp target enter data map(always,to: x)
}
