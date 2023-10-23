/* Test 'if' and 'self' clause appearing together.  */

#include <openacc.h>

static int test(float i, long double s)
{
  int ret;
#pragma acc serial copyout(ret) if(i) self(s)
  /* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { xfail openacc_nvidia_accel_selected } .-1 } */
  {
    ret = acc_on_device(acc_device_host);
  }
  return ret;
}

int main()
{
  if (!test(0, 0))
    __builtin_abort();

  if (!test(0, 1))
    __builtin_abort();

#if ACC_MEM_SHARED
  if (!test(1, 0))
    __builtin_abort();
#else
  if (test(1, 0))
    __builtin_abort();
#endif

  if (!test(1, 1))
    __builtin_abort();

  return 0;
}
