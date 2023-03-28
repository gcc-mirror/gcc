/* Test 'nohost' clause via 'weak'.

   { dg-require-effective-target weak_undefined }

   When the OpenACC 'routine' with 'nohost' clauses gets discarded, the weak symbol then resolves to 'NULL'.
*/

/* { dg-additional-sources routine-nohost-2_2.c } */

/* { dg-additional-options "-fno-inline" } for stable results regarding OpenACC 'routine'.  */
/* { dg-add-options weak_undefined } */

#include <assert.h>
#include <openacc.h>

#pragma acc routine //nohost
__attribute__((weak))
extern int f1(int);

int main()
{
  int x = -10;

#pragma acc serial copy(x)
  /* { dg-warning {using .vector_length \(32\)., ignoring 1} "" { target openacc_nvidia_accel_selected } .-1 } */
  {
    if (f1)
      x = f1(x);
    else
      x = 0;

  }

  if (acc_get_device_type() == acc_device_host)
    assert(x == 0);
  else
    assert(x == -20);

  return 0;
}
