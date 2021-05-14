/* { dg-do link } */
/* { dg-require-effective-target large_long_double } */

/* PR middle-end/86416  */
/* { dg-error "bit-precision floating-point numbers unsupported .mode '.F'." "" { target { offload_target_nvptx || offload_target_amdgcn } } 0 }  */
/* { dg-excess-errors "Follow-up errors from mkoffload and lto-wrapper" { target { offload_target_nvptx || offload_target_amdgcn } } }  */

#include <stdlib.h>  /* For abort. */

long double foo (long double x)
{
  #pragma omp target map(tofrom:x)
    x *= 2.0;
  return x;
}

int main()
{
  long double v = foo (10.0L) - 20.0L;
  if (v > 1.0e-5L || v < -1.0e-5L) abort();
  return 0;
}
