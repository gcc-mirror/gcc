/* Testing 'has_device_addr' clause on the target construct with template.  */

#include <omp.h>

template <typename T>
void
foo (T (&x)[])
{
  #pragma omp target has_device_addr(x)
    for (int i = 0; i < 15; i++)
      x[i] = 2 * i;

  #pragma omp target has_device_addr(x[15:15])
    for (int i = 15; i < 30; i++)
      x[i] = 3 * i;
}

int
main ()
{
  int *dp = (int*)omp_target_alloc (30*sizeof(int), 0);

  #pragma omp target is_device_ptr(dp)
    for (int i = 0; i < 30; i++)
      dp[i] = i;

  int (&x)[30] = *static_cast<int(*)[30]>(static_cast<void*>(dp));

  foo <int> (x);

  int y[30];
  for (int i = 0; i < 30; ++i)
    y[i] = 0;
  int h = omp_get_initial_device ();
  int t = omp_get_default_device ();
  omp_target_memcpy (&y, dp, 30 * sizeof(int), 0, 0, h, t);
  for (int i = 0; i < 15; ++i)
    if (y[i] != 2 * i)
      __builtin_abort ();
  for (int i = 15; i < 30; ++i)
    if (y[i] != 3 * i)
      __builtin_abort ();

  omp_target_free (dp, 0);

  return 0;
}
