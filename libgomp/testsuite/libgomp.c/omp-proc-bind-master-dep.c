// { dg-do run }
// { dg-additional-options "-Wno-deprecated-openmp" }
#include <omp.h>
#include <stdlib.h>

int
main()
{
  if (omp_proc_bind_master != omp_proc_bind_primary) // { dg-warning "'omp_proc_bind_master' is deprecated \\\[-Wdeprecated-declarations\\\]" }
    abort();
  return 0;
}
