/* { dg-do link { target __float128 } } */
/* { dg-add-options __float128 } */

/* PR middle-end/86416  */
/* { dg-error "bit-precision floating-point numbers unsupported .mode '.F'." "" { target { offload_target_nvptx || offload_target_amdgcn } } 0 }  */
/* { dg-excess-errors "Follow-up errors from mkoffload and lto-wrapper" { target { offload_target_nvptx || offload_target_amdgcn } } }  */

#include <stdlib.h>  /* For abort. */

__float128 foo(__float128 y)
{
  #pragma omp target map(tofrom: y)
    y *= 4.0;
  return y;
}

int main()
{
  __float128 v = foo (5.0Q) - 20.0Q;
  if (v > 1.0e-5Q || v < -1.0e-5Q) abort();
  return 0;
}
