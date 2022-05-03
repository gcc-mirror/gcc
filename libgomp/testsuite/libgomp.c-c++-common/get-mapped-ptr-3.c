#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int a[0];
  int b[] = { 24, 42 };
  void *p1 = NULL, *p2 = NULL;

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  void *p = omp_target_alloc (sizeof (int), d);
  if (p == NULL)
    return 0;

  if (omp_target_associate_ptr (a, p, sizeof (int), 0, d) != 0)
    return 0;

  if (omp_get_mapped_ptr (a, d) != (d == id ? a : p))
    abort ();

  if (omp_target_disassociate_ptr (a, d) != 0)
    abort ();

  if (omp_get_mapped_ptr (a, d) != (d == id ? a : NULL))
    abort ();

  #pragma omp target data map(alloc: a, b[1:0]) device(d)
  {
    #pragma omp target map(from: p1, p2) map(alloc: a, b[1:0]) device(d)
    {
      p1 = &a;
      p2 = &b[1];
    }

    /* This is probably expected to be p1/p2 instead of NULL. Zero-length arrays
       as list items of the map clause are currently not inserted into the mem
       map ?! However by returning NULL, omp_get_mapped_ptr is consistent with
       omp_target_is_present.  */
    if (omp_get_mapped_ptr (a, d) != NULL
	|| omp_get_mapped_ptr (&b[1], d) != NULL)
      abort ();
  }

  omp_target_free (p, d);
  return 0;
}
