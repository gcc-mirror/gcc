#include <omp.h>

int
main ()
{
  int *dp = (int*)omp_target_alloc (30*sizeof(int), 0);

  #pragma omp target is_device_ptr(dp)
    for (int i = 0; i < 30; i++)
      dp[i] = i;

  int (&x)[30] = *static_cast<int(*)[30]>(static_cast<void*>(dp));

  #pragma omp target has_device_addr(x)
    for (int i = 0; i < 30; i++)
      x[i] = 2 * i;

  #pragma omp target has_device_addr(x)
    for (int i = 0; i < 30; i++)
      if (x[i] != 2 * i)
	__builtin_abort ();

  #pragma omp target has_device_addr(x[1:5])
    for (int i = 1; i < 6; i++)
      x[i] = 3 * i;

  #pragma omp target has_device_addr(x[1:5])
    for (int i = 1; i < 6; i++)
      if (x[i] != 3 * i)
	__builtin_abort ();

  omp_target_free (dp, 0);
}
