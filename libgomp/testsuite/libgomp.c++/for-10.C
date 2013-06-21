extern "C" void abort ();

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#define F simd
#define G simd
#define S
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F parallel for simd
#define G pf_simd
#include "../libgomp.c/for-1.h"
#undef F
#undef G

#define F for simd
#define G f_simd
#include "../libgomp.c/for-1.h"
#undef F
#undef G

int
main ()
{
  if (test_simd_normal ()
      || test_pf_simd_static ()
      || test_pf_simd_static32 ()
      || test_pf_simd_auto ()
      || test_pf_simd_guided32 ()
      || test_pf_simd_runtime ()
      || test_f_simd_static ()
      || test_f_simd_static32 ()
      || test_f_simd_auto ()
      || test_f_simd_guided32 ()
      || test_f_simd_runtime ())
    abort ();
  return 0;
}
