/* Testing 'has_device_addr' clause on the target construct with reference. */

#include <omp.h>

int
main ()
{
  int *dpx = (int*)omp_target_alloc (sizeof(int), 0);
  int **dpy = (int**)omp_target_alloc (sizeof(int*), 0);

  #pragma omp target is_device_ptr(dpx, dpy)
    {
      *dpx = 42;
      int z = 77;
      *dpy = &z;
    }

  int& x = *dpx;
  int*& y = *dpy;

  #pragma omp target has_device_addr(x, y)
    {
      x = 24;
      y = &x;
    }

  #pragma omp target has_device_addr(x, y)
    if (x != 24 || y != &x)
      __builtin_abort ();

  omp_target_free(dpx, 0);
  omp_target_free(dpy, 0);
}
