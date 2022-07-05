#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int x;
  void *p, *q;

  q = (void *) &x;

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  p = omp_target_alloc (sizeof (int), d);
  if (p == NULL)
    return 0;

  if (omp_target_associate_ptr (q, p, sizeof (int), 0, d) != 0)
    return 0;

  if (omp_get_mapped_ptr (q, -5) != NULL)
    abort ();

  if (omp_get_mapped_ptr (q, omp_get_num_devices () + 1) != NULL)
    abort ();

  if (omp_get_mapped_ptr (q, id) != q)
    abort ();

  if (omp_get_mapped_ptr (q, omp_initial_device) != q)
    abort ();

  if (omp_get_mapped_ptr (q, d) != p)
    abort ();

  if (omp_target_disassociate_ptr (q, d) != 0)
    abort ();

  if (omp_get_mapped_ptr (q, d) != NULL)
    abort ();

  omp_target_free (p, d);
  return 0;
}
