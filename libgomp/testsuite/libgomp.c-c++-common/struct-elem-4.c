#include <omp.h>
#include <stdlib.h>

struct S
{
  int a, b, c, d, e;
};
typedef struct S S;

int main (void)
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  S s = { 1, 2, 3, 4, 5 };
  #pragma omp target enter data map (to:s)

  int *p = &s.b;
  int *q = &s.d;
  #pragma omp target enter data map (alloc: p[:1], q[:1])

  s.b = 88;
  s.d = 99;

  #pragma omp target exit data map (release: s)
  if (d != id)
    {
      if (!omp_target_is_present (&s, d))
	abort ();
      if (!omp_target_is_present (&p[0], d))
	abort ();
      if (!omp_target_is_present (&q[0], d))
	abort ();
    }

  #pragma omp target exit data map (from: q[:1])
  if (d != id)
    {
      if (omp_target_is_present (&s, d))
	abort ();
      if (omp_target_is_present (&p[0], d))
	abort ();
      if (omp_target_is_present (&q[0], d))
	abort ();

      if (q[0] != 4)
	abort ();
      if (p[0] != 88)
	abort ();
    }

  return 0;
}
