/* { dg-additional-options "-std=gnu99" { target c } } */

extern
#ifdef __cplusplus
"C"
#endif
void abort ();

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#define F distribute
#define G d
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F distribute
#define G d_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F distribute simd
#define G ds
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F distribute simd
#define G ds_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F distribute parallel for
#define G dpf
#include "for-1.h"
#undef F
#undef G

#define F distribute parallel for dist_schedule(static, 128)
#define G dpf_ds128
#include "for-1.h"
#undef F
#undef G

#define F distribute parallel for simd
#define G dpfs
#include "for-1.h"
#undef F
#undef G

#define F distribute parallel for simd dist_schedule(static, 128)
#define G dpfs_ds128
#include "for-1.h"
#undef F
#undef G

int
main ()
{
  int err = 0;
  #pragma omp teams reduction(|:err)
    {
      err |= test_d_normal ();
      err |= test_d_ds128_normal ();
      err |= test_ds_normal ();
      err |= test_ds_ds128_normal ();
      err |= test_dpf_static ();
      err |= test_dpf_static32 ();
      err |= test_dpf_auto ();
      err |= test_dpf_guided32 ();
      err |= test_dpf_runtime ();
      err |= test_dpf_ds128_static ();
      err |= test_dpf_ds128_static32 ();
      err |= test_dpf_ds128_auto ();
      err |= test_dpf_ds128_guided32 ();
      err |= test_dpf_ds128_runtime ();
      err |= test_dpfs_static ();
      err |= test_dpfs_static32 ();
      err |= test_dpfs_auto ();
      err |= test_dpfs_guided32 ();
      err |= test_dpfs_runtime ();
      err |= test_dpfs_ds128_static ();
      err |= test_dpfs_ds128_static32 ();
      err |= test_dpfs_ds128_auto ();
      err |= test_dpfs_ds128_guided32 ();
      err |= test_dpfs_ds128_runtime ();
    }
  if (err)
    abort ();
  return 0;
}
