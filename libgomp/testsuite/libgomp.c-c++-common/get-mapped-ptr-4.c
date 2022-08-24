#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  struct s_t { int m1; char m2; } s;
  void *p1 = NULL, *p2 = NULL;

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  #pragma omp target data map(alloc: s, s.m2) device(d)
  {
    #pragma omp target map(from: p1, p2) map(alloc: s, s.m2) device(d)
    {
      p1 = &s;
      p2 = &s.m2;
    }
    if (omp_get_mapped_ptr (&s, d) != (d == id ? &s : p1)
	|| omp_get_mapped_ptr (&s.m2, d) != (d == id ? &s.m2 : p2))
      abort ();
  }

  if (omp_get_mapped_ptr (&s, d) != (d == id ? &s : NULL)
      || omp_get_mapped_ptr (&s.m2, d) != (d == id ? &s.m2 : NULL))
    abort ();

  #pragma omp target enter data map(alloc: s, s.m2) device (d)
  #pragma omp target map(from: p1, p2) map(alloc: s, s.m2) device(d)
  {
    p1 = &s;
    p2 = &s.m2;
  }

  if (omp_get_mapped_ptr (&s, d) != (d == id ? &s : p1)
      || omp_get_mapped_ptr (&s.m2, d) != (d == id ? &s.m2 : p2))
    abort ();

  #pragma omp target exit data map (delete: s, s.m2) device (d)

  if (omp_get_mapped_ptr (&s, d) != (d == id ? &s : NULL)
      || omp_get_mapped_ptr (&s.m2, d) != (d == id ? &s.m2 : NULL))
    abort ();

  return 0;
}
