extern "C" void abort ();

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#pragma omp declare target

#define F for
#define G f
#define S
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
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

#define F target parallel for
#define G tpf
#include "../libgomp.c/for-1.h"
#undef F
#undef G

#define F target simd
#define G t_simd
#define S
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F target parallel for simd
#define G tpf_simd
#include "../libgomp.c/for-1.h"
#undef F
#undef G

#define F target teams distribute
#define G ttd
#define S
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F target teams distribute
#define G ttd_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F target teams distribute simd
#define G ttds
#define S
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F target teams distribute simd
#define G ttds_ds128
#define S dist_schedule(static, 128)
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F target teams distribute parallel for
#define G ttdpf
#include "../libgomp.c/for-1.h"
#undef F
#undef G

#define F target teams distribute parallel for dist_schedule(static, 128)
#define G ttdpf_ds128
#include "../libgomp.c/for-1.h"
#undef F
#undef G

#define F target teams distribute parallel for simd
#define G ttdpfs
#include "../libgomp.c/for-1.h"
#undef F
#undef G

#define F target teams distribute parallel for simd dist_schedule(static, 128)
#define G ttdpfs_ds128
#include "../libgomp.c/for-1.h"
#undef F
#undef G

int
main ()
{
  if (test_tpf_static ()
      || test_tpf_static32 ()
      || test_tpf_auto ()
      || test_tpf_guided32 ()
      || test_tpf_runtime ()
      || test_t_simd_normal ()
      || test_tpf_simd_static ()
      || test_tpf_simd_static32 ()
      || test_tpf_simd_auto ()
      || test_tpf_simd_guided32 ()
      || test_tpf_simd_runtime ()
      || test_ttd_normal ()
      || test_ttd_ds128_normal ()
      || test_ttds_normal ()
      || test_ttds_ds128_normal ()
      || test_ttdpf_static ()
      || test_ttdpf_static32 ()
      || test_ttdpf_auto ()
      || test_ttdpf_guided32 ()
      || test_ttdpf_runtime ()
      || test_ttdpf_ds128_static ()
      || test_ttdpf_ds128_static32 ()
      || test_ttdpf_ds128_auto ()
      || test_ttdpf_ds128_guided32 ()
      || test_ttdpf_ds128_runtime ()
      || test_ttdpfs_static ()
      || test_ttdpfs_static32 ()
      || test_ttdpfs_auto ()
      || test_ttdpfs_guided32 ()
      || test_ttdpfs_runtime ()
      || test_ttdpfs_ds128_static ()
      || test_ttdpfs_ds128_static32 ()
      || test_ttdpfs_ds128_auto ()
      || test_ttdpfs_ds128_guided32 ()
      || test_ttdpfs_ds128_runtime ())
    abort ();
}
