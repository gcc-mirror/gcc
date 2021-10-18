#include <omp.h>
#include <stdlib.h>

struct S { S () : s (42) {} S (const S &x) : s (x.s) {}; ~S () {} int s; };

int
main ()
{
  S s;
  s.s = 113;
  #pragma omp parallel num_threads(4) default(firstprivate)
  {
    if (s.s != 113)
      abort ();
    s.s = omp_get_thread_num ();
    #pragma omp barrier
    if (s.s != omp_get_thread_num ())
      abort ();
  }
  #pragma omp parallel num_threads(4) default(private)
  {
    if (s.s != 42)
      abort ();
    s.s = omp_get_thread_num () + 13;
    #pragma omp barrier
    if (s.s != omp_get_thread_num () + 13)
      abort ();
  }
}
