/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-additional-options "-std=gnu99" {target c } } */

#include <omp.h>
#include <stdlib.h>

int r, s, u, v, t;
int *x;

void
foo (int *a)
{
  int i;
  long long j;
  #pragma omp for lastprivate (conditional: u, x) ordered
  for (i = 15; i < 64; i++)
    {
      #pragma omp critical
      {
	if ((a[i] % 5) == 3)
	  u = i;
      }
      #pragma omp ordered
      {
	if ((a[i] % 7) == 2)
	  x = &a[i];
      }
    }
  #pragma omp for lastprivate (conditional: v) reduction (+:r, s) schedule (nonmonotonic: static) reduction (task, +: t)
  for (i = -3; i < 119; i += 2)
    {
      ++s;
      #pragma omp taskgroup
      {
	#pragma omp task in_reduction (+: t)
	  ++t;
	if ((a[i + 4] % 11) == 9)
	  v = i;
	else
	  ++r;
      }
    }
}

int
main ()
{
  int a[128], i;
  for (i = 0; i < 128; i++)
    a[i] = i;
  #pragma omp parallel
  foo (a);
  if (u != 63 || v != 115 || x != &a[58] || r != 55 || s != 61 || t != 61)
    abort ();
  return 0;
}
