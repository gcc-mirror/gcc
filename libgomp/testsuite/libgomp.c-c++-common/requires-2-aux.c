/* { dg-skip-if "" { *-*-* } } */

int x;

void foo (void)
{
  #pragma omp target
  x = 1;
}
