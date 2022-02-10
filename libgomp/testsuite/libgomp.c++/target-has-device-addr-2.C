/* Testing 'has_device_addr' clause on the target construct with reference. */

#include <omp.h>

int
main ()
{
  int *dp = (int*)omp_target_alloc (sizeof(int), 0);

  #pragma omp target is_device_ptr(dp)
    *dp = 42;

  int &x = *dp;

  #pragma omp target has_device_addr(x)
    x = 24;

  #pragma omp target has_device_addr(x)
    if (x != 24)
      __builtin_abort ();

  omp_target_free(dp, 0);
}
