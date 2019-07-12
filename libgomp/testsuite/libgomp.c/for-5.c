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

#undef OMPFROM
#undef OMPTO
#define DO_PRAGMA(x) _Pragma (#x)
#define OMPFROM(v) DO_PRAGMA (omp target update from(v))
#define OMPTO(v) DO_PRAGMA (omp target update to(v))

#if TEST_ALL || (1 <= TEST_NR && TEST_NR <= 5)
#define F target parallel for
#define G tpf
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 6
#define F target simd
#define G t_simd
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || (7 <= TEST_NR && TEST_NR <= 11)
#define F target parallel for simd
#define G tpf_simd
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 12
#define F target teams distribute
#define G ttd
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 13
#define F target teams distribute
#define G ttd_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 14
#define F target teams distribute simd
#define G ttds
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || TEST_NR == 15
#define F target teams distribute simd
#define G ttds_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#endif

#if TEST_ALL || (16 <= TEST_NR && TEST_NR <= 20)
#define F target teams distribute parallel for
#define G ttdpf
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (21 <= TEST_NR && TEST_NR <= 25)
#define F target teams distribute parallel for dist_schedule(static, 128)
#define G ttdpf_ds128
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (26 <= TEST_NR && TEST_NR <= 30)
#define F target teams distribute parallel for simd
#define G ttdpfs
#include "for-1.h"
#undef F
#undef G
#endif

#if TEST_ALL || (31 <= TEST_NR && TEST_NR <= 35)
#define F target teams distribute parallel for simd dist_schedule(static, 128)
#define G ttdpfs_ds128
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
#include "for-5.list"
#undef DO_TEST
#endif
#undef DO_TEST_1

  return 0;
}
