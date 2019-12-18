/* { dg-do link { target __float128 } } */
/* { dg-add-options __float128 } */

/* PR middle-end/86416  */
/* { dg-error "bit-precision floating-point numbers unsupported .mode '.F'." "" { target offload_device } 0 }  */
/* { dg-excess-errors "Follow-up errors from mkoffload and lto-wrapper" { target offload_device } }  */

#include <stdlib.h>  /* For abort. */

__float128 foo(__float128 y)
{
  #pragma omp target map(tofrom: y)
    y *= 4.0;
  return y;
}

int main()
{
  __float128 v = foo (5.0L) - 20.0L;
  if (v > 1.0e-5 || v < -1.0e-5) abort();
  return 0;
}
