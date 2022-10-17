/* Testing 'has_device_addr' clause on the target construct with template.  */

#include <omp.h>

template <typename T>
void
foo (T (&x))
{
  #pragma omp target has_device_addr(x)
    x = 24;
}

int
main ()
{
  int *dp = (int*)omp_target_alloc (sizeof(int), 0);
  int &x = *dp;

  foo <int> (x);

  int y = 42;
  int h = omp_get_initial_device ();
  int t = omp_get_default_device ();
  omp_target_memcpy (&y, dp, sizeof(int), 0, 0, h, t);
  if (y != 24)
    __builtin_abort ();

  omp_target_free (dp, 0);
  return 0;
}
