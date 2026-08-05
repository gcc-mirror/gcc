#include <omp.h>
#include <stdlib.h>

int main (void)
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  unsigned int a = 0xcdcdcdcd;
  #pragma omp target enter data map (to:a)

  a = 0xabababab;
  unsigned char *p = (unsigned char *) &a;
  unsigned char *q = p + 2;

  #pragma omp target enter data map (alloc:p[:1], q[:1])

  if (d != id)
    {
      if (!omp_target_is_present (&a, d))
	abort ();
      if (!omp_target_is_present (&p[0], d))
	abort ();
      if (!omp_target_is_present (&q[0], d))
	abort ();
    }

  #pragma omp target exit data map (release:a)

  if (d != id)
    {
      if (!omp_target_is_present (&a, d))
	abort ();
      if (!omp_target_is_present (&p[0], d))
	abort ();
      if (!omp_target_is_present (&q[0], d))
	abort ();
    }

  #pragma omp target exit data map (from:q[:1])

  if (d != id)
    {
      if (omp_target_is_present (&a, d))
	abort ();
      if (omp_target_is_present (&p[0], d))
	abort ();
      if (omp_target_is_present (&q[0], d))
	abort ();

      if (q[0] != 0xcd)
	abort ();
      if (p[0] != 0xab)
	abort ();
    }

  return 0;
}
