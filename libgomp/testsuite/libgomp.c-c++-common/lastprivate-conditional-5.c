/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-additional-options "-std=gnu99" {target c } } */

#include <omp.h>
#include <stdlib.h>

int r, s, u, v, r2, s2, u2, v2, r3, s3, u3, v3;
long long w, w2, w3, p, p2, p3;
int *x, *x2, *x3;
short y, y2, y3;
int z;
int thr1, thr2;
#pragma omp threadprivate (thr1, thr2)

void
foo (int *a, long long int b, long long int c)
{
  int i;
  long long j;
  #pragma omp parallel for lastprivate (conditional: u, x)
  for (i = 15; i < 64; i++)
    {
      if ((a[i] % 5) == 3)
	u = i;
      if ((a[i] % 7) == 2)
	x = &a[i];
    }
  #pragma omp parallel for lastprivate (conditional: v) reduction (+:r, s) schedule (nonmonotonic: static)
  for (i = -3; i < 119; i += 2)
    {
      ++s;
      if ((a[i + 4] % 11) == 9)
	v = i;
      else
	++r;
    }
  #pragma omp parallel for schedule (monotonic: static) lastprivate (conditional: w)
  for (j = b; j < b + 115 * c; j += (b & 3) + 7)
    if ((a[j] % 13) == 5)
      w = j * 2;
  #pragma omp parallel for schedule (auto) lastprivate (conditional: p) collapse(3)
  for (i = -5; i < (int) (b + 5); i += 2)
    for (j = b + 12 + c; j > b; --j)
      for (int k = 0; k < 5; k += c)
	if (((((i + 5) * 13 + (13 - j)) * 5 + k) % 17) == 6)
	  p = i * 10000 + j * 100 + k;

  #pragma omp parallel for schedule (nonmonotonic: static, 2) lastprivate (conditional: u2, x2)
  for (i = 15; i < 64; i++)
    {
      if ((a[i] % 5) == 3)
	u2 = i;
      if ((a[i] % 7) == 2)
	x2 = &a[i];
    }
  #pragma omp parallel for schedule (static, 3) lastprivate (conditional: v2) reduction (+:r2, s2)
  for (i = -3; i < 119; i += 2)
    {
      ++s2;
      if ((a[i + 4] % 11) == 9)
	v2 = i;
      else
	++r2;
    }
  #pragma omp parallel for lastprivate (conditional: w2) schedule (static, 1)
  for (j = b; j < b + 115 * c; j += (b & 3) + 7)
    if ((a[j] % 13) == 5)
      w2 = j * 2;
  #pragma omp parallel for schedule (static, 3) collapse (3) lastprivate (conditional: p2)
  for (i = -5; i < (int) (b + 5); i += 2)
    for (j = b + 12 + c; j > b; --j)
      for (int k = 0; k < 5; k += c)
	if (((((i + 5) * 13 + (13 - j)) * 5 + k) % 17) == 6)
	  p2 = i * 10000 + j * 100 + k;

  #pragma omp parallel for lastprivate (conditional: u3, x3) schedule (runtime)
  for (i = 15; i < 64; i++)
    {
      if ((a[i] % 5) == 3)
	u3 = i;
      if ((a[i] % 7) == 2)
	x3 = &a[i];
    }
  #pragma omp parallel for lastprivate (conditional: v3) reduction (+:r3, s3) schedule (nonmonotonic: dynamic)
  for (i = -3; i < 119; i += 2)
    {
      ++s3;
      if ((a[i + 4] % 11) == 9)
	v3 = i;
      else
	++r3;
    }
  #pragma omp parallel for schedule (monotonic: guided, 3) lastprivate (conditional: w3)
  for (j = b; j < b + 115 * c; j += (b & 3) + 7)
    if ((a[j] % 13) == 5)
      w3 = j * 2;
  #pragma omp parallel for schedule (dynamic, 4) lastprivate (conditional: p3) collapse(3)
  for (i = -5; i < (int) (b + 5); i += 2)
    for (j = b + 12 + c; j > b; --j)
      for (int k = 0; k < 5; k += c)
	if (((((i + 5) * 13 + (13 - j)) * 5 + k) % 17) == 6)
	  p3 = i * 10000 + j * 100 + k;

  /* Nasty testcase, verify that even a no-op assignment is accounted
     for in lastprivate(conditional:).  */
  #pragma omp parallel for schedule (monotonic: static, 2) firstprivate (z) \
			   lastprivate (conditional: z)
  for (int k = -2000; k < 8000; ++k)
    {
      if (k < 3000 && (k & 3) == 1)
	{
	  z = k;
	  thr1 = k;
	}
      else if (k == 7931)
	{
	  z = z;
	  thr2 = 1;
	}
    }

  if (thr2 && z != thr1)
    abort ();
}

int
main ()
{
  int a[128], i;
  volatile int j = 0;
  for (i = 0; i < 128; i++)
    a[i] = i;
  w = 1234;
  foo (a, j, j + 1);
  if (u != 63 || v != 115 || w != 140 || x != &a[58] || r != 55 || s != 61 || p != 30104)
    abort ();
  if (u2 != 63 || v2 != 115 || w2 != 140 || x2 != &a[58] || r2 != 55 || s2 != 61 || p2 != 30104)
    abort ();
  if (u3 != 63 || v3 != 115 || w3 != 140 || x3 != &a[58] || r3 != 55 || s3 != 61 || p3 != 30104)
    abort ();
  return 0;
}
