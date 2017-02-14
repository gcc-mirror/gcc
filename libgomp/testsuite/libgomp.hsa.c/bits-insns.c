#include <math.h>

#define N 12

int main()
{
  unsigned int arguments[N] = {0u, 1u, 2u, 3u, 111u, 333u, 444u, 0x80000000u, 0x0000ffffu, 0xf0000000u, 0xff000000u, 0xffffffffu};
  int clrsb[N] = {};
  int clz[N] = {};
  int ctz[N] = {};
  int ffs[N] = {};
  int parity[N] = {};
  int popcount[N] = {};

  int ref_clrsb[N] = {};
  int ref_clz[N] = {};
  int ref_ctz[N] = {};
  int ref_ffs[N] = {};
  int ref_parity[N] = {};
  int ref_popcount[N] = {};

  for (unsigned i = 0; i < N; i++)
    {
      ref_clrsb[i] = __builtin_clrsb (arguments[i]);
      ref_clz[i] = __builtin_clz (arguments[i]);
      ref_ctz[i] = __builtin_ctz (arguments[i]);
      ref_ffs[i] = __builtin_ffs (arguments[i]);
      ref_parity[i] = __builtin_parity (arguments[i]);
      ref_popcount[i] = __builtin_popcount (arguments[i]);
    }

  #pragma omp target map(from:clz, ctz, ffs, parity, popcount)
  {
    for (unsigned i = 0; i < N; i++)
    {
      clrsb[i] = __builtin_clrsb (arguments[i]);
      clz[i] = __builtin_clz (arguments[i]);
      ctz[i] = __builtin_ctz (arguments[i]);
      ffs[i] = __builtin_ffs (arguments[i]);
      parity[i] = __builtin_parity (arguments[i]);
      popcount[i] = __builtin_popcount (arguments[i]);
    }
  }

  for (unsigned i = 0; i < N; i++)
    if (ref_clrsb[i] != clrsb[i])
      __builtin_abort ();

  /* CLZ of zero is undefined for zero.  */
  for (unsigned i = 1; i < N; i++)
    if (ref_clz[i] != clz[i])
      __builtin_abort ();

  /* Likewise for ctz */
  for (unsigned i = 1; i < N; i++)
    if (ref_ctz[i] != ctz[i])
      __builtin_abort ();

  for (unsigned i = 0; i < N; i++)
    if (ref_ffs[i] != ffs[i])
      __builtin_abort ();

  for (unsigned i = 0; i < N; i++)
    if (ref_parity[i] != parity[i])
      __builtin_abort ();

  for (unsigned i = 0; i < N; i++)
    if (ref_popcount[i] != popcount[i])
      __builtin_abort ();

  return 0;
}

