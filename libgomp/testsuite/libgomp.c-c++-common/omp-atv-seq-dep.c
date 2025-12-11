// { dg-do run }
#include <omp.h>

int
main()
{
  int test = omp_atv_sequential; // { dg-warning "'omp_atv_sequential' is deprecated \\\[-Wdeprecated-declarations\\\]" }
  return 0;
}
