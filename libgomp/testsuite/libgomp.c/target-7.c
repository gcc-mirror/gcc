#include <omp.h>
#include <stdlib.h>

volatile int v;

void
foo (int f)
{
  int d = f ? omp_get_num_devices () : omp_get_default_device ();
  int h = 5;
  #pragma omp target device (d)
  if (omp_get_level () != 0)
    abort ();
  #pragma omp target if (v > 1)
  if (omp_get_level () != 0 || !omp_is_initial_device ())
    abort ();
  #pragma omp target device (d) if (v > 1)
  if (omp_get_level () != 0 || !omp_is_initial_device ())
    abort ();
  #pragma omp target if (v <= 1)
  if (omp_get_level () != 0 || (f && !omp_is_initial_device ()))
    abort ();
  #pragma omp target device (d) if (v <= 1)
  if (omp_get_level () != 0 || (f && !omp_is_initial_device ()))
    abort ();
  #pragma omp target if (0)
  if (omp_get_level () != 0 || !omp_is_initial_device ())
    abort ();
  #pragma omp target device (d) if (0)
  if (omp_get_level () != 0 || !omp_is_initial_device ())
    abort ();
  #pragma omp target if (1)
  if (omp_get_level () != 0 || (f && !omp_is_initial_device ()))
    abort ();
  #pragma omp target device (d) if (1)
  if (omp_get_level () != 0 || (f && !omp_is_initial_device ()))
    abort ();
  #pragma omp target data device (d) map (to: h)
  {
    #pragma omp target device (d)
    if (omp_get_level () != 0 || (f && !omp_is_initial_device ()) || h++ != 5)
      abort ();
    #pragma omp target update device (d) from (h)
  }
  #pragma omp target data if (v > 1) map (to: h)
  {
    #pragma omp target if (v > 1)
    if (omp_get_level () != 0 || !omp_is_initial_device () || h++ != 6)
      abort ();
    #pragma omp target update if (v > 1) from (h)
  }
  #pragma omp target data device (d) if (v > 1) map (to: h)
  {
    #pragma omp target device (d) if (v > 1)
    if (omp_get_level () != 0 || !omp_is_initial_device () || h++ != 7)
      abort ();
    #pragma omp target update device (d) if (v > 1) from (h)
  }
  #pragma omp target data if (v <= 1) map (to: h)
  {
    #pragma omp target if (v <= 1)
    if (omp_get_level () != 0 || (f && !omp_is_initial_device ()) || h++ != 8)
      abort ();
    #pragma omp target update if (v <= 1) from (h)
  }
  #pragma omp target data device (d) if (v <= 1) map (to: h)
  {
    #pragma omp target device (d) if (v <= 1)
    if (omp_get_level () != 0 || (f && !omp_is_initial_device ()) || h++ != 9)
      abort ();
    #pragma omp target update device (d) if (v <= 1) from (h)
  }
  #pragma omp target data if (0) map (to: h)
  {
    #pragma omp target if (0)
    if (omp_get_level () != 0 || !omp_is_initial_device () || h++ != 10)
      abort ();
    #pragma omp target update if (0) from (h)
  }
  #pragma omp target data device (d) if (0) map (to: h)
  {
    #pragma omp target device (d) if (0)
    if (omp_get_level () != 0 || !omp_is_initial_device () || h++ != 11)
      abort ();
    #pragma omp target update device (d) if (0) from (h)
  }
  #pragma omp target data if (1) map (to: h)
  {
    #pragma omp target if (1)
    if (omp_get_level () != 0 || (f && !omp_is_initial_device ()) || h++ != 12)
      abort ();
    #pragma omp target update if (1) from (h)
  }
  #pragma omp target data device (d) if (1) map (to: h)
  {
    #pragma omp target device (d) if (1)
    if (omp_get_level () != 0 || (f && !omp_is_initial_device ()) || h++ != 13)
      abort ();
    #pragma omp target update device (d) if (1) from (h)
  }
  if (h != 14)
    abort ();
}

int
main ()
{
  foo (0);
  foo (1);
  return 0;
}
