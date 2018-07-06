/* { dg-do run } */

/* Based on asyncwait-1.f90.  */

#include <stdlib.h>

#define N 64

int
main (void)
{
  int *a, *b, *c, *d, *e;

  a = (int*)malloc (N * sizeof (*a));
  b = (int*)malloc (N * sizeof (*b));
  c = (int*)malloc (N * sizeof (*c));
  d = (int*)malloc (N * sizeof (*d));
  e = (int*)malloc (N * sizeof (*e));

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {

#pragma acc parallel async
#pragma acc loop
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 3)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {
#pragma acc parallel async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2) abort ();
      if (b[i] != 2) abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N])
  {

#pragma acc parallel async (1)
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc parallel async (1)
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];


#pragma acc parallel async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 9)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
      e[i] = 0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N])
  {

#pragma acc parallel async (1)
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc parallel async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];

#pragma acc parallel async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];


#pragma acc parallel wait (1) async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      e[i] = a[i] + b[i] + c[i] + d[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2)
	abort ();
      if (b[i] != 4)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
      if (e[i] != 11)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {

#pragma acc kernels async
#pragma acc loop
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 3)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {
#pragma acc kernels async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2)
	abort ();
      if (b[i] != 2)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N])
  {
#pragma acc kernels async (1)
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc kernels async (1)
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];

#pragma acc kernels async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 9)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
      e[i] = 0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N])
  {
#pragma acc kernels async (1)
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc kernels async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];

#pragma acc kernels async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

#pragma acc kernels wait (1) async (1)
#pragma acc loop
    for (int i = 0; i < N; ++i)
      e[i] = a[i] + b[i] + c[i] + d[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2)
	abort ();
      if (b[i] != 4)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
      if (e[i] != 11)
	abort ();
    }

  free (a);
  free (b);
  free (c);
  free (d);
  free (e);

  return 0;
}
