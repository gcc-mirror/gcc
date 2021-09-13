/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <stdlib.h>
#include <stdio.h>

#define M 8
#define N 32

#pragma acc routine vector
void
vector (int *a)
{
  int i;

#pragma acc loop vector
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
  for (i = 0; i < N; i++)
    a[i] -= a[i]; 
}

#pragma acc routine worker
void
worker (int *b)
{
  int i, j;

#pragma acc loop worker
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
  for (i = 0; i < N; i++)
    {
#pragma acc loop vector
      /* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
      for (j = 0; j < M; j++)
        b[i * M + j] += b[i  * M + j]; 
    }
}

#pragma acc routine gang
void
gang (int *a)
{
  int i;

#pragma acc loop gang worker vector
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
  for (i = 0; i < N; i++)
    a[i] -= i; 
}

#pragma acc routine seq
void
seq (int *a)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] += 1;
}

int
main(int argc, char **argv)
{
  int i;
  int a[N];
  int b[M * N];

  i = 0;

  for (i = 0; i < N; i++)
    a[i] = 0;

#pragma acc parallel copy (a[0:N])
  {
#pragma acc loop seq
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    for (i = 0; i < N; i++)
      seq (&a[0]);
  }

  for (i = 0; i < N; i++)
    {
      if (a[i] != N)
	abort ();
    }

#pragma acc parallel copy (a[0:N])
  {
#pragma acc loop seq
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    for (i = 0; i < N; i++)
      gang (&a[0]);
  }

  for (i = 0; i < N; i++)
    {
      if (a[i] != N + (N * (-1 * i)))
	abort ();
    }

  for (i = 0; i < N; i++)
    a[i] = i;

#pragma acc parallel copy (b[0:M*N])
  {
    worker (&b[0]);
  }

  for (i = 0; i < N; i++)
    {
      if (a[i] != i)
	abort ();
    }

  for (i = 0; i < N; i++)
    a[i] = i;

#pragma acc parallel copy (a[0:N])
  {
#pragma acc loop
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    for (i = 0; i < N; i++)
      vector (&a[0]);
  }

  for (i = 0; i < N; i++)
    {
      if (a[i] != 0)
	abort ();
    }

  return 0;
}
