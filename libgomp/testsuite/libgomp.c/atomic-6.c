/* PR middle-end/36106 */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mieee" { target alpha*-*-* } } */
/* { dg-options "-O2 -march=i586" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

#ifdef __i386__
# include "cpuid.h"
#endif

extern void abort (void);

union { unsigned long long l; double d; } u = { .l = 0x7ff0000000072301ULL };

int __attribute__((noinline))
do_test (void)
{
#pragma omp atomic
  u.d += 1.0L;
  return 0;
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

  do_test ();

  return 0;
}
