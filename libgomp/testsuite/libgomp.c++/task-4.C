#include <omp.h>
extern "C" void *memset (void *, int, __SIZE_TYPE__);
extern "C" void abort (void);

int e;

void
baz (int i, int *p, int j, int *q)
{
  if (p[0] != 1 || p[i] != 3 || q[0] != 2 || q[j] != 4)
    #pragma omp atomic
      e++;
}

void
foo (int i, int j)
{
  int p[i + 1];
  int q[j + 1];
  memset (p, 0, sizeof (p));
  memset (q, 0, sizeof (q));
  p[0] = 1;
  p[i] = 3;
  q[0] = 2;
  q[j] = 4;
  #pragma omp task firstprivate (p, q)
    baz (i, p, j, q);
}

int
main ()
{
  #pragma omp parallel num_threads (4)
    foo (5 + omp_get_thread_num (), 7 + omp_get_thread_num ());
  if (e)
    abort ();
}
