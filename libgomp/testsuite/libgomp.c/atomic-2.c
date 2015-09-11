/* { dg-do run } */
/* { dg-additional-options "-mcx16" { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

#ifdef __x86_64__
#include "cpuid.h"
#endif

double d = 1.5;
long double ld = 3;
extern void abort (void);

void
test (void)
{
#pragma omp atomic
  d *= 1.25;
#pragma omp atomic
  ld /= 0.75;
  if (d != 1.875 || ld != 4.0L)
    abort ();
}

int
main (void)
{
#ifdef __x86_64__
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  if (!(ecx & bit_CMPXCHG16B))
    return 0;
#endif
  test ();
  return 0;
}
