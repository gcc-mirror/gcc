/* PR middle-end/36106 */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mcx16" { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

#ifdef __x86_64__
# include "../../../gcc/config/i386/cpuid.h"
#endif

extern void abort (void);

int __attribute__((noinline))
do_test (void)
{
  long double d = .0L;
  int i;
  #pragma omp parallel for shared (d)
    for (i = 0; i < 10; i++)
      #pragma omp atomic
	d += 1.0L;
  if (d != 10.0L)
    abort ();
  return 0;
}

int
main (void)
{
#ifdef __x86_64__
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  if (ecx & bit_CMPXCHG16B)
    do_test ();
#else
  do_test ();
#endif
  return 0;
}
