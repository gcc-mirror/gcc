#include <omp.h>
#include <stdlib.h>

struct S
{
  int a, b, c, d;
};
typedef struct S S;

int main (void)
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  S s;
  #pragma omp target enter data map (alloc: s.a, s.b, s.c, s.d)
  #pragma omp target enter data map (alloc: s.c)
  #pragma omp target enter data map (alloc: s.b, s.d)
  #pragma omp target enter data map (alloc: s.a, s.c, s.b)

  #pragma omp target exit data map (release: s.a)
  #pragma omp target exit data map (release: s.d)
  #pragma omp target exit data map (release: s.c)
  #pragma omp target exit data map (release: s.b)

  /* OpenMP 5.0 structure element mapping rules describe that elements of same
     structure variable should allocate/deallocate in a uniform fashion, so
     all elements of 's' should be removed together by above 'exit data's.  */
  if (d != id)
    {
      if (omp_target_is_present (&s, d))
	abort ();
      if (omp_target_is_present (&s.a, d))
	abort ();
      if (omp_target_is_present (&s.b, d))
	abort ();
      if (omp_target_is_present (&s.c, d))
	abort ();
      if (omp_target_is_present (&s.d, d))
	abort ();
    }

  return 0;
}
