/* { dg-do run } */

#include <stdlib.h>

#define PK parallel
#define M(x, y, z) O(x, y, z)
#define O(x, y, z) x ## _ ## y ## _ ## z

#define F
#define G none
#define L
#include "parallel-loop-1.h"
#undef L
#undef F
#undef G

#define F num_gangs (10)
#define G gangs
#define L gang
#include "parallel-loop-1.h"
#undef L
#undef F
#undef G

int
main ()
{
  if (test_none_none ()
      || test_none_auto ()
      || test_none_independent ()
      || test_none_seq ()
      || test_gangs_none ()
      || test_gangs_auto ()
      || test_gangs_independent ()
      || test_gangs_seq ())
    abort ();
  return 0;
}
