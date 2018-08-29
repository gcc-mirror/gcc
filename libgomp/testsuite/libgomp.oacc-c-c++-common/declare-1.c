#include <openacc.h>
#include <stdlib.h>
#include <stdio.h>

#define N 8

void
subr2 (int *a)
{
  int i;
  int f[N];
#pragma acc declare copyout (f)

#pragma acc parallel copy (a[0:N])
  {
    for (i = 0; i < N; i++)
      {
	f[i] = a[i];
	a[i] = f[i] + f[i] + f[i];
      }
  }
}

void
subr1 (int *a)
{
  int f[N];
#pragma acc declare copy (f)

#pragma acc parallel copy (a[0:N])
  {
    int i;

    for (i = 0; i < N; i++)
      {
	f[i] = a[i];
	a[i] = f[i] + f[i];
      }
  }
}

int b[8];
#pragma acc declare create (b)

int d[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
#pragma acc declare copyin (d)

int
main (int argc, char **argv)
{
  int a[N];
  int e[N];
#pragma acc declare create (e)
  int i;

  for (i = 0; i < N; i++)
    a[i] = i + 1;

  if (!acc_is_present (&b, sizeof (b)))
    abort ();

  if (!acc_is_present (&d, sizeof (d)))
    abort ();

  if (!acc_is_present (&e, sizeof (e)))
    abort ();

#pragma acc parallel copyin (a[0:N])
  {
    for (i = 0; i < N; i++)
      {
	b[i] = a[i];
	a[i] = b[i];
      }
  }

  for (i = 0; i < N; i++)
    {
      if (a[i] != i + 1)
	abort ();
    }

#pragma acc parallel copy (a[0:N])
  {
    for (i = 0; i < N; i++)
      {
	e[i] = a[i] + d[i];
	a[i] = e[i];
      }
  }

  for (i = 0; i < N; i++)
    {
      if (a[i] != (i + 1) * 2)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 1234;
    }

  subr1 (&a[0]);

  for (i = 0; i < N; i++)
    {
      if (a[i] != 1234 * 2)
	abort ();
    }

  subr2 (&a[0]);

  for (i = 0; i < 1; i++)
    {
      if (a[i] != 1234 * 6)
	abort ();
    }

  return 0;
}
