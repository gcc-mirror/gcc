#include <omp.h>
#include <stdlib.h>

struct S
{
  int a, b;
};
typedef struct S S;

int main (void)
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  S s;
  #pragma omp target enter data map (alloc: s.a, s.b)
  #pragma omp target exit data map (release: s.b)

  /* OpenMP 5.0 structure element mapping rules describe that elements of same
     structure variable should allocate/deallocate in a uniform fashion, so
     "s.a" should be removed together by above 'exit data'.  */
  if (d != id && omp_target_is_present (&s.a, d))
    abort ();

  return 0;
}
