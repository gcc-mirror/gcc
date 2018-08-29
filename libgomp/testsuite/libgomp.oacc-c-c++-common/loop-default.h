#include <openacc.h>
#include <alloca.h>
#include <string.h>
#include <stdio.h>
#include <gomp-constants.h>

#pragma acc routine seq
static int __attribute__ ((noinline))
coord (void)
{
  int res = 0;

  if (acc_on_device (acc_device_nvidia))
    {
      int g = 0, w = 0, v = 0;
      g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
      w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
      v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

      res = (1 << 24) | (g << 16) | (w << 8) | v;
    }

  return res;
}

static int
check (const int *ary, int size, int gp, int wp, int vp)
{
  int exit = 0;
  int ix;
  int *gangs = (int *)alloca (gp * sizeof (int));
  int *workers = (int *)alloca (wp * sizeof (int));
  int *vectors = (int *)alloca (vp * sizeof (int));
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

#define N (32 * 32 * 32)
int ary[N];

static int
check_gang (int gp)
{
#pragma acc parallel copyout (ary)
  {
#pragma acc loop gang (static:1)
    for (int ix = 0; ix < N; ix++)
      ary[ix] = coord ();
  }

  return check (ary, N, gp, 1, 1);
}

static int
check_worker (int wp)
{
#pragma  acc parallel copyout (ary)
  {
#pragma acc loop worker
    for (int ix = 0; ix < N; ix++)
      ary[ix] = coord ();
  }

  return check (ary, N, 1, wp, 1);
}

static int
check_vector (int vp)
{
#pragma  acc parallel copyout (ary)
  {
#pragma acc loop vector
    for (int ix = 0; ix < N; ix++)
      ary[ix] = coord ();
  }

  return check (ary, N, 1, 1, vp);
}

static int
test_1 (int gp, int wp, int vp)
{
  int exit = 0;

  exit |= check_gang (gp);
  exit |= check_worker (wp);
  exit |= check_vector (vp);

  return exit;
}
