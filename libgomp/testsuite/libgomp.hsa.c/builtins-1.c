/* { dg-additional-options "-ffast-math" } */

#include <assert.h>
#include <math.h>

#define N 10
#define N2 14

#define c1 1.2345f
#define c2 1.2345

#define DELTA 0.001

#define TEST_BIT_BUILTINS(T, S, S2)                                            \
  {                                                                            \
    T arguments[N2]                                                            \
      = {0##S,		1##S,	  2##S,	  3##S,                    \
	 111##S,	333##S,	444##S,	0x80000000##S,           \
	 0x0000ffff##S, 0xf0000000##S, 0xff000000##S, 0xffffffff##S};          \
    int clrsb[N2] = {};                                                        \
    int clz[N2] = {};                                                          \
    int ctz[N2] = {};                                                          \
    int ffs[N2] = {};                                                          \
    int parity[N2] = {};                                                       \
    int popcount[N2] = {};                                                     \
                                                                               \
    _Pragma ("omp target map(to:clz[:N2], ctz[:N2], ffs[:N2], parity[:N2], popcount[:N2])")                                                 \
    {                                                                          \
      for (unsigned i = 0; i < N2; i++)                                        \
	{                                                                      \
	  clrsb[i] = __builtin_clrsb##S2 (arguments[i]);                       \
	  clz[i] = __builtin_clz##S2 (arguments[i]);                           \
	  ctz[i] = __builtin_ctz##S2 (arguments[i]);                           \
	  ffs[i] = __builtin_ffs##S2 (arguments[i]);                           \
	  parity[i] = __builtin_parity##S2 (arguments[i]);                     \
	  popcount[i] = __builtin_popcount##S2 (arguments[i]);                 \
	}                                                                      \
    }                                                                          \
                                                                               \
    for (unsigned i = 0; i < N2; i++)                                          \
      {                                                                        \
	assert (clrsb[i] == __builtin_clrsb##S2 (arguments[i]));               \
	if (arguments[0] != 0)                                                 \
	  {                                                                    \
	    assert (clz[i] == __builtin_clz##S2 (arguments[i]));               \
	    assert (ctz[i] == __builtin_ctz##S2 (arguments[i]));               \
	  }                                                                    \
	assert (ffs[i] == __builtin_ffs##S2 (arguments[i]));                   \
	assert (parity[i] == __builtin_parity##S2 (arguments[i]));             \
	assert (popcount[i] == __builtin_popcount##S2 (arguments[i]));         \
      }                                                                        \
  }

#define ASSERT(v1, v2) assert (fabs (v1 - v2) < DELTA)

int
main ()
{
  float f[N] = {};
  float d[N] = {};

/* 1) test direct mapping to HSA insns.  */

#pragma omp target map(to: f[ : N], d[ : N])
  {
    f[0] = sinf (c1);
    f[1] = cosf (c1);
    f[2] = exp2f (c1);
    f[3] = log2f (c1);
    f[4] = truncf (c1);
    f[5] = sqrtf (c1);

    d[0] = trunc (c2);
    d[1] = sqrt (c2);
  }

  ASSERT (f[0], sinf (c1));
  ASSERT (f[1], cosf (c1));
  ASSERT (f[2], exp2f (c1));
  ASSERT (f[3], log2f (c1));
  ASSERT (f[4], truncf (c1));
  ASSERT (f[5], sqrtf (c1));

  ASSERT (d[0], trunc (c2));
  ASSERT (d[1], sqrt (c2));

  /* 2) test bit builtins for unsigned int.  */
  TEST_BIT_BUILTINS (int, , );

  /* 3) test bit builtins for unsigned long int.  */
  TEST_BIT_BUILTINS (long, l, l);

  /* 4) test bit builtins for unsigned long long int.  */
  TEST_BIT_BUILTINS (long long, ll, ll);

  return 0;
}
