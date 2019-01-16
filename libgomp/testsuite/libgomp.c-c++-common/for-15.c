/* { dg-additional-options "-std=gnu99" { target c } } */

extern
#ifdef __cplusplus
"C"
#endif
void abort ();

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#define F for
#define G f
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F teams distribute
#define G td
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F teams distribute
#define G td_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F teams distribute simd
#define G tds
#define S
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F teams distribute simd
#define G tds_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F teams distribute parallel for
#define G tdpf
#include "for-1.h"
#undef F
#undef G

#define F teams distribute parallel for dist_schedule(static, 128)
#define G tdpf_ds128
#include "for-1.h"
#undef F
#undef G

#define F teams distribute parallel for simd
#define G tdpfs
#include "for-1.h"
#undef F
#undef G

#define F teams distribute parallel for simd dist_schedule(static, 128)
#define G tdpfs_ds128
#include "for-1.h"
#undef F
#undef G

int
main ()
{
  if (test_td_normal ()
      || test_td_ds128_normal ()
      || test_tds_normal ()
      || test_tds_ds128_normal ()
      || test_tdpf_static ()
      || test_tdpf_static32 ()
      || test_tdpf_auto ()
      || test_tdpf_guided32 ()
      || test_tdpf_runtime ()
      || test_tdpf_ds128_static ()
      || test_tdpf_ds128_static32 ()
      || test_tdpf_ds128_auto ()
      || test_tdpf_ds128_guided32 ()
      || test_tdpf_ds128_runtime ()
      || test_tdpfs_static ()
      || test_tdpfs_static32 ()
      || test_tdpfs_auto ()
      || test_tdpfs_guided32 ()
      || test_tdpfs_runtime ()
      || test_tdpfs_ds128_static ()
      || test_tdpfs_ds128_static32 ()
      || test_tdpfs_ds128_auto ()
      || test_tdpfs_ds128_guided32 ()
      || test_tdpfs_ds128_runtime ())
    abort ();
  return 0;
}
