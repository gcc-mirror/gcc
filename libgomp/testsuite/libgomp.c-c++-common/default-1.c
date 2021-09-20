#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int v = 42;
  #pragma omp parallel num_threads(4) default(firstprivate)
  {
    if (v != 42)
      abort ();
    v = omp_get_thread_num ();
    #pragma omp barrier
    if (v != omp_get_thread_num ())
      abort ();
  }
  #pragma omp parallel num_threads(4) default(private)
  {
    v = omp_get_thread_num () + 13;
    #pragma omp barrier
    if (v != omp_get_thread_num () + 13)
      abort ();
  }
  return 0;
}
