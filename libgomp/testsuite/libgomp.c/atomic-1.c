/* { dg-do run } */
/* { dg-options "-O2 -march=pentium" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

#ifdef __i386__
#include "cpuid.h"
#endif

extern void abort (void);
double d;
struct
{
  int i;
  double e;
  int j;
} x;

void
f1 (void)
{
  #pragma omp atomic
    d += 7.5;
  #pragma omp atomic
    d *= 2.5;
  #pragma omp atomic
    d /= 0.25;
}

void
f2 (void)
{
  #pragma omp atomic
    x.e += 7.5;
  #pragma omp atomic
    x.e *= 2.5;
  #pragma omp atomic
    x.e /= 0.25;
}

int
main (void)
{
#ifdef __i386__
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  if (!(edx & bit_CMPXCHG8B))
    return 0;
#endif

  d = 1.0;
  f1 ();
  if (d != 85.0)
    abort ();

  x.e = 1.0;
  f2 ();
  if (x.i != 0 || x.e != 85.0 || x.j != 0)
    abort ();
  return 0;
}
