/* { dg-additional-options "-fopenacc-dim=16:16" } */

#include <openacc.h>
#include <string.h>
#include <stdio.h>
#include <gomp-constants.h>

#pragma acc routine
static int __attribute__ ((noinline)) coord ()
{
  int res = 0;

  if (acc_on_device (acc_device_not_host))
    {
      int g, w, v;

      g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
      w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
      v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
      res = (1 << 24) | (g << 16) | (w << 8) | v;
    }
  return res;
}


int check (const int *ary, int size, int gp, int wp, int vp)
{
  int exit = 0;
  int ix;
  int *gangs = (int *)__builtin_alloca (gp * sizeof (int));
  int *workers = (int *)__builtin_alloca (wp * sizeof (int));
  int *vectors = (int *)__builtin_alloca (vp * sizeof (int));
  int offloaded = 0;
  
  memset (gangs, 0, gp * sizeof (int));
  memset (workers, 0, wp * sizeof (int));
  memset (vectors, 0, vp * sizeof (int));
  
  for (ix = 0; ix < size; ix++)
    {
      int g = (ary[ix] >> 16) & 0xff;
      int w = (ary[ix] >> 8) & 0xff;
      int v = (ary[ix] >> 0) & 0xff;

      if (g >= gp || w >= wp || v >= vp)
	{
	  printf ("unexpected cpu %#x used\n", ary[ix]);
	  exit = 1;
	}
      else
	{
	  vectors[v]++;
	  workers[w]++;
	  gangs[g]++;
	}
      offloaded += ary[ix] >> 24;
    }

  if (!offloaded)
    return 0;

  if (offloaded != size)
    {
      printf ("offloaded %d times,  expected %d\n", offloaded, size);
      return 1;
    }

  for (ix = 0; ix < gp; ix++)
    if (gangs[ix] != gangs[0])
      {
	printf ("gang %d not used %d times\n", ix, gangs[0]);
	exit = 1;
      }
  
  for (ix = 0; ix < wp; ix++)
    if (workers[ix] != workers[0])
      {
	printf ("worker %d not used %d times\n", ix, workers[0]);
	exit = 1;
      }
  
  for (ix = 0; ix < vp; ix++)
    if (vectors[ix] != vectors[0])
      {
	printf ("vector %d not used %d times\n", ix, vectors[0]);
	exit = 1;
      }
  
  return exit;
}

#define N (32 *32*32)

int test_1 (int gp, int wp, int vp)
{
  int ary[N];
  int exit = 0;
  
#pragma acc parallel copyout (ary)
  {
#pragma acc loop gang (static:1)
    for (int ix = 0; ix < N; ix++)
      ary[ix] = coord ();
  }

  exit |= check (ary, N, gp, 1, 1);

#pragma  acc parallel copyout (ary)
  {
#pragma acc loop worker
    for (int ix = 0; ix < N; ix++)
      ary[ix] = coord ();
  }

  exit |= check (ary, N, 1, wp, 1);

#pragma  acc parallel copyout (ary)
  {
#pragma acc loop vector
    for (int ix = 0; ix < N; ix++)
      ary[ix] = coord ();
  }

  exit |= check (ary, N, 1, 1, vp);

  return exit;
}

int main ()
{
  return test_1 (16, 16, 32);
}
