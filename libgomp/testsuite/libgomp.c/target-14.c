#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int err;
  void *p;

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  p = omp_target_alloc (128 * sizeof (int), d);
  if (p == NULL)
    return 0;

  #pragma omp target is_device_ptr (p) if (d >= 0) device (d >= 0 ? d : 0)
  {
    int i, *q = (int *) p;
    for (i = 0; i < 128; i++)
      q[i] = i + 7;
  }
  #pragma omp target is_device_ptr (p) if (d >= 0) device (d >= 0 ? d : 0) map(from:err)
  {
    int i;
    err = 0;
    for (i = 0; i < 128; i++)
      if (((int *) p)[i] != i + 7)
	err = 1;
  }
  if (err)
    abort ();

  omp_target_free (p, d);
  return 0;
}
