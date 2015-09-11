/* { dg-do run } */
/* { dg-options "-O0" } */

#include <omp.h>
#include <stdlib.h>

short e[64];
int g;
_Complex double d, f;
int num_threads;

__attribute__((noinline)) void
foo (int x, long long y)
{
#pragma omp parallel num_threads (4)
  {
    int i;
    #pragma omp barrier
    for (i = 0; i < 2400; i++)
      {
	if (i == 0)
	  num_threads = omp_get_num_threads ();
	#pragma omp atomic
	  e[0] += x;
	#pragma omp atomic
	  e[16] += x;
	#pragma omp atomic
	  g += y;
	#pragma omp atomic
	  __real__ d += x;
	#pragma omp atomic
	  __imag__ f += x;
      }
  }
}

int
main (void)
{
  int i;
  foo (3, 3LL);
  if (g != 3 * 2400 * num_threads
      || __real__ d != g || __imag__ d != 0
      || __real__ f != 0 || __imag__ f != g)
    abort ();
  for (i = 0; i < 64; i++)
    if (e[i] != ((i && i != 16) ? 0 : g))
      abort ();
  return 0;
}
