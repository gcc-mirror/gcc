extern "C" void abort (void);

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#define F taskloop
#define G taskloop
#define S
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

#define F taskloop simd
#define G taskloop_simd
#define S
#define N(x) M(x, G, normal)
#include "../libgomp.c/for-2.h"
#undef S
#undef N
#undef F
#undef G

int
main ()
{
  int err = 0;
  #pragma omp parallel reduction(|:err)
    #pragma omp single
      {
	if (test_taskloop_normal ()
	    || test_taskloop_simd_normal ())
	  err = 1;
      }
  if (err)
    abort ();
  return 0;
}
