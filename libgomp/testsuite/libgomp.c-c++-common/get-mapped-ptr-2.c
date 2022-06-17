#include <omp.h>
#include <stdlib.h>
#include <stdint.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int a = 42;
  int b[] = { 24, 42 };
  int c[] = { 47, 11 };
  int e[128];
  int *q = &a;
  void *p1 = NULL, *p2 = NULL, *p3 = NULL;
  void *devptrs[128];

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  for (int i = 0; i < 128; i++)
    e[i] = i;

  #pragma omp target data map(alloc: a, b, c[1], e[32:64]) device(d)
  {
    #pragma omp target map(from: p1, p2, p3, devptrs) map(alloc: a, b, c[1], e[32:64]) device(d)
    {
      p1 = &a;
      p2 = &b;
      p3 = &c[1];
      for (int i = 32; i < 96; i++)
	devptrs[i] = &e[i];
    }

    if (omp_get_mapped_ptr (&a, d) != (d == id ? &a : p1)
	|| omp_get_mapped_ptr (q, d) != (d == id ? q : p1)
	|| omp_get_mapped_ptr (b, d) != (d == id ? b : p2)
	|| omp_get_mapped_ptr (&b[0], d) != (d == id ? &b[0] : p2)
	|| omp_get_mapped_ptr (&c[1], d) != (d == id ? &c[1] : p3)
	|| omp_get_mapped_ptr (&c[0], d) != (d == id ? &c[0] : NULL))
      abort ();

    for (int i = 0; i < 32; i++)
      if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : NULL))
	abort ();
    for (int i = 32; i < 96; i++)
      if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : devptrs[i]))
	abort ();
    for (int i = 96; i < 128; i++)
      if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : NULL))
	abort ();
  }

  if (omp_get_mapped_ptr (&a, d) != (d == id ? &a : NULL)
      || omp_get_mapped_ptr (q, d) != (d == id ? q : NULL)
      || omp_get_mapped_ptr (b, d) != (d == id ? b : NULL)
      || omp_get_mapped_ptr (&b[0], d) != (d == id ? &b[0] : NULL)
      || omp_get_mapped_ptr (&c[1], d) != (d == id ? &c[1] : NULL)
      || omp_get_mapped_ptr (&c[0], d) != (d == id ? &c[0] : NULL))
    abort ();
  for (int i = 0; i < 128; i++)
    if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : NULL))
      abort ();

  #pragma omp target enter data map (alloc: a, b, c[1], e[32:64]) device (d)
  #pragma omp target map(from: p1, p2, p3, devptrs) map(alloc: a, b, c[1], e[32:64]) device(d)
  {
    p1 = &a;
    p2 = &b;
    p3 = &c[1];
    for (int i = 32; i < 96; i++)
      devptrs[i] = &e[i];
  }

  if (omp_get_mapped_ptr (&a, d) != (d == id ? &a : p1)
      || omp_get_mapped_ptr (q, d) != (d == id ? q : p1)
      || omp_get_mapped_ptr (b, d) != (d == id ? b : p2)
      || omp_get_mapped_ptr (&b[0], d) != (d == id ? &b[0] : p2)
      || omp_get_mapped_ptr (&c[1], d) != (d == id ? &c[1] : p3)
      || omp_get_mapped_ptr (&c[0], d) != (d == id ? &c[0] : NULL))
    abort ();
  for (int i = 0; i < 32; i++)
    if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : NULL))
      abort ();
  for (int i = 32; i < 96; i++)
    if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : devptrs[i]))
      abort ();
  for (int i = 96; i < 128; i++)
    if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : NULL))
      abort ();

  #pragma omp target exit data map (delete: a, b, c[1], e[32:64]) device (d)

  if (omp_get_mapped_ptr (&a, d) != (d == id ? &a : NULL)
      || omp_get_mapped_ptr (q, d) != (d == id ? q : NULL)
      || omp_get_mapped_ptr (b, d) != (d == id ? b : NULL)
      || omp_get_mapped_ptr (&b[0], d) != (d == id ? &b[0] : NULL)
      || omp_get_mapped_ptr (&c[1], d) != (d == id ? &c[1] : NULL)
      || omp_get_mapped_ptr (&c[0], d) != (d == id ? &c[0] : NULL))
    abort ();
  for (int i = 0; i < 128; i++)
    if (omp_get_mapped_ptr (&e[i], d) != (d == id ? &e[i] : NULL))
      abort ();

  return 0;
}
