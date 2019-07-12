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

#define F for
#define G f
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#pragma omp end declare target

#undef OMPTGT
#undef OMPFROM
#undef OMPTO
#define DO_PRAGMA(x) _Pragma (#x)
#define OMPTGT DO_PRAGMA (omp target)
#define OMPFROM(v) DO_PRAGMA (omp target update from(v))
#define OMPTO(v) DO_PRAGMA (omp target update to(v))

#if TEST_ALL || TEST_NR == 1
#define F teams distribute
#define G td
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 2
#define F teams distribute
#define G td_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 3
#define F teams distribute simd
#define G tds
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 4
#define F teams distribute simd
#define G tds_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || (5 <= TEST_NR && TEST_NR <= 9)
#define F teams distribute parallel for
#define G tdpf
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (10 <= TEST_NR && TEST_NR <= 14)
#define F teams distribute parallel for dist_schedule(static, 128)
#define G tdpf_ds128
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (15 <= TEST_NR && TEST_NR <= 19)
#define F teams distribute parallel for simd
#define G tdpfs
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (20 <= TEST_NR && TEST_NR <= 24)
#define F teams distribute parallel for simd dist_schedule(static, 128)
#define G tdpfs_ds128
#include "for-1.h"
#undef F
#undef G
#endif

int
main ()
{
#define DO_TEST_1(test)				\
  do {						\
    if (test ())				\
      abort ();					\
  } while (0)

#ifdef ONE_TEST
  DO_TEST_1 (ONE_TEST);
#else
#define DO_TEST(test) DO_TEST_1 (test);
#include "for-6.list"
#undef DO_TEST
#endif
#undef DO_TEST_1

  return 0;
}
