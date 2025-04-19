/* { dg-options "-fopenmp-target=acc" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#define DO_PRAGMA(x) _Pragma (#x)

#undef OMPFROM
#undef OMPTO
#define OMPFROM(v) DO_PRAGMA (omp target update from(v))
#define OMPTO(v) DO_PRAGMA (omp target update to(v))

#pragma omp declare target

#define OMPTGT DO_PRAGMA (omp target)
#define F parallel for
#define G pf
#define S
#define N(x) M(x, G, ompacc)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G
#undef OMPTGT

#pragma omp end declare target

#define F target parallel for
#define G tpf
#define S
#define N(x) M(x, G, ompacc)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F target teams distribute
#define G ttd
#define S
#define N(x) M(x, G, ompacc)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F target teams distribute parallel for
#define G ttdpf
#define S
#define N(x) M(x, G, ompacc)
#include "for-2.h"
#undef S
#undef N
#undef F
#undef G

int
main ()
{
  if (test_pf_ompacc ()
      || test_tpf_ompacc ()
      || test_ttd_ompacc ()
      || test_ttdpf_ompacc ())
    __builtin_abort ();
  return 0;
}
