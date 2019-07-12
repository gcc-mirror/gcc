/* { dg-additional-options "-std=gnu99" } */

extern void abort ();

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#ifndef ONE_TEST
#define TEST_ALL 1
#else
#define TEST_ALL 0
#endif

#pragma omp declare target

#if TEST_ALL || TEST_NR == 1
#define F distribute
#define G d
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 2
#define F distribute
#define G d_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 3
#define F distribute simd
#define G ds
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 4
#define F distribute simd
#define G ds_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || (5 <= TEST_NR && TEST_NR <= 9)
#define F distribute parallel for
#define G dpf
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (10 <= TEST_NR && TEST_NR <= 14)
#define F distribute parallel for dist_schedule(static, 128)
#define G dpf_ds128
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (15 <= TEST_NR && TEST_NR <= 19)
#define F distribute parallel for simd
#define G dpfs
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (20 <= TEST_NR && TEST_NR <= 24)
#define F distribute parallel for simd dist_schedule(static, 128)
#define G dpfs_ds128
#include "for-1.h"
#undef F
#undef G
#endif

#pragma omp end declare target

int
main ()
{
  int err = 0;

  #pragma omp target teams reduction(|:err)
  {
#define DO_TEST_1(test) \
    do {	      \
      err |= test (); \
    } while (0)

#ifdef ONE_TEST
  DO_TEST_1 (ONE_TEST);
#else
#define DO_TEST(test) DO_TEST_1(test);
#include "for-3.list"
#undef DO_TEST
#endif
#undef DO_TEST_1
  }

  if (err)
    abort ();
  return 0;
}
