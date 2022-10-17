/* Testing 'has_device_addr' clause on the target construct with reference. */

#include <omp.h>

int
main ()
{
  int *dpx = (int*)omp_target_alloc (sizeof(int), 0);
  double *dpy = (double*)omp_target_alloc (sizeof(double), 0);

  #pragma omp target is_device_ptr(dpx, dpy)
    {
      *dpx = 42;
      *dpy = 43.5;
    }

  int &x = *dpx;
  double &y = *dpy;

  #pragma omp target has_device_addr(x, y)
    {
      x = 24;
      y = 25.7;
    }

  #pragma omp target has_device_addr(y, x)
    if (x != 24 || y != 25.7)
      __builtin_abort ();

  omp_target_free(dpx, 0);
  omp_target_free(dpy, 0);
}
