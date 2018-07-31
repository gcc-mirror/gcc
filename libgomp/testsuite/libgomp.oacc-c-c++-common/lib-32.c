/* acc_present_or_create, acc_present_or_copyin, etc.  */
/* See also Fortran variants in "../libgomp.oacc-fortran/lib-32*".  */

#include <stdbool.h>
#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  int *h, *d;
  const int N = 10000;
  const int S = N * sizeof *h;
  bool shared_mem;

  h = (int *) malloc (S);
  if (!h)
    abort ();
  for (int i = 0; i < N; ++i)
    h[i] = i + 0;

  shared_mem = acc_is_present (h, S);

  d = (int *) acc_present_or_create (h, S);
  if (!d)
    abort ();
  if (shared_mem)
    if (h != d)
      abort ();
  if (!acc_is_present (h, S))
    abort ();

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      d[i] = i + 1;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 1 : 0))
	abort ();
      h[i] = i + 2;
    }

  {
    int *d_ = (int *) acc_present_or_create (h, S);
    if (d_ != d)
      abort ();
  }

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 2 : 1))
	abort ();
      d[i] = i + 3;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 3 : 2))
	abort ();
      h[i] = i + 4;
    }

  {
    int *d_ = (int *) acc_pcreate (h, S);
    if (d_ != d)
      abort ();
  }

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 4 : 3))
	abort ();
      d[i] = i + 5;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 5 : 4))
	abort ();
      h[i] = i + 6;
    }

  {
    int *d_ = (int *) acc_present_or_copyin (h, S);
    if (d_ != d)
      abort ();
  }

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 6 : 5))
	abort ();
      d[i] = i + 7;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 7 : 6))
	abort ();
      h[i] = i + 8;
    }

  {
    int *d_ = (int *) acc_pcopyin (h, S);
    if (d_ != d)
      abort ();
  }

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 8 : 7))
	abort ();
      d[i] = i + 9;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 9 : 8))
	abort ();
      h[i] = i + 10;
    }

  acc_copyout_finalize (h, S);
  d = NULL;
  if (!shared_mem)
    if (acc_is_present (h, S))
      abort ();

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 10 : 9))
	abort ();
    }

  d = (int *) acc_pcopyin (h, S);
  if (!d)
    abort ();
  if (shared_mem)
    if (h != d)
      abort ();
  if (!acc_is_present (h, S))
    abort ();

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 10 : 9))
	abort ();
      d[i] = i + 11;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 11 : 9))
	abort ();
      h[i] = i + 12;
    }

  {
    int *d_ = (int *) acc_pcopyin (h, S);
    if (d_ != d)
      abort ();
  }

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 12 : 11))
	abort ();
      d[i] = i + 13;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 13 : 12))
	abort ();
      h[i] = i + 14;
    }

  {
    int *d_ = (int *) acc_pcreate (h, S);
    if (d_ != d)
      abort ();
  }

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 14 : 13))
	abort ();
      d[i] = i + 15;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 15 : 14))
	abort ();
      h[i] = i + 16;
    }

  {
    int *d_ = (int *) acc_pcreate (h, S);
    if (d_ != d)
      abort ();
  }

#pragma acc parallel loop deviceptr (d)
  for (int i = 0; i < N; ++i)
    {
      if (d[i] != i + (shared_mem ? 16 : 15))
	abort ();
      d[i] = i + 17;
    }

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 17 : 16))
	abort ();
      h[i] = i + 18;
    }

  acc_update_self (h, S);
  if (!acc_is_present (h, S))
    abort ();

  for (int i = 0; i < N; ++i)
    {
      if (h[i] != i + (shared_mem ? 18 : 17))
	abort ();
    }

  acc_delete_finalize (h, S);
  d = NULL;
  if (!shared_mem)
    if (acc_is_present (h, S))
      abort();

  free (h);

  return 0;
}
