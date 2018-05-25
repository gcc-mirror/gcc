/* { dg-additional-options "-std=gnu99" {target c } } */

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#define F parallel for
#define G pf
#include "for-1.h"
#undef F
#undef G

#define F for
#define G f
#include "for-1.h"
#undef F
#undef G

int
main ()
{
  if (test_pf_static ()
      || test_pf_static32 ()
      || test_pf_auto ()
      || test_pf_guided32 ()
      || test_pf_runtime ()
      || test_f_static ()
      || test_f_static32 ()
      || test_f_auto ()
      || test_f_guided32 ()
      || test_f_runtime ())
    abort ();
  return 0;
}
