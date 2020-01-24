/* AMD GCN does not use 32-lane vectors.
   { dg-skip-if "unsuitable dimensions" { openacc_amdgcn_accel_selected } { "*" } { "" } } */

/* { dg-additional-options "-fopenacc-dim=32" } */

#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

int check (const int *ary, int size, int gp, int wp, int vp)
{
  int exit = 0;
  int ix;
  int gangs[32], workers[32], vectors[32];

  for (ix = 0; ix < 32; ix++)
    gangs[ix] = workers[ix] = vectors[ix] = 0;
  
  for (ix = 0; ix < size; ix++)
    {
      vectors[ary[ix] & 0xff]++;
      workers[(ary[ix] >> 8) & 0xff]++;
      gangs[(ary[ix] >> 16) & 0xff]++;
    }

  for (ix = 0; ix < 32; ix++)
    {
      if (gp)
	{
	  int expect = gangs[0];
	  if (gangs[ix] != expect)
	    {
	      exit = 1;
	      printf ("gang %d not used %d times\n", ix, expect);
	    }
	}
      else if (ix && gangs[ix])
	{
	  exit = 1;
	  printf ("gang %d unexpectedly used\n", ix);
	}

      if (wp)
	{
	  int expect = workers[0];
	  if (workers[ix] != expect)
	    {
	      exit = 1;
	      printf ("worker %d not used %d times\n", ix, expect);
	    }
	}
      else if (ix && workers[ix])
	{
	  exit = 1;
	  printf ("worker %d unexpectedly used\n", ix);
	}

      if (vp)
	{
	  int expect = vectors[0];
	  if (vectors[ix] != expect)
	    {
	      exit = 1;
	      printf ("vector %d not used %d times\n", ix, expect);
	    }
	}
      else if (ix && vectors[ix])
	{
	  exit = 1;
	  printf ("vector %d unexpectedly used\n", ix);
	}
      
    }
  return exit;
}

#pragma acc routine seq
static int __attribute__((noinline)) place ()
{
  int r = 0;

  int g = 0, w = 0, v = 0;
  g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
  w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
  v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
  r = (g << 16) | (w << 8) | v;

  return r;
}

static void clear (int *ary, int size)
{
  int ix;

  for (ix = 0; ix < size; ix++)
    ary[ix] = -1;
}

int vector_1 (int *ary, int size)
{
  clear (ary, size);
  
#pragma acc parallel num_workers (32) vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop gang
    for (int jx = 0; jx < 1; jx++)
#pragma acc loop auto
      for (int ix = 0; ix < size; ix++)
	ary[ix] = place ();
  }

  return check (ary, size, 0, 1, 1);
}

int vector_2 (int *ary, int size)
{
  clear (ary, size);
  
#pragma acc parallel num_workers (32) vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop worker
    for (int jx = 0; jx < size  / 64; jx++)
#pragma acc loop auto
      for (int ix = 0; ix < 64; ix++)
	ary[ix + jx * 64] = place ();
  }

  return check (ary, size, 0, 1, 1);
}

int worker_1 (int *ary, int size)
{
  clear (ary, size);
  
#pragma acc parallel num_workers (32) vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop gang
    for (int kx = 0; kx < 1; kx++)
#pragma acc loop auto
      for (int jx = 0; jx <  size  / 64; jx++)
#pragma acc loop vector
	for (int ix = 0; ix < 64; ix++)
	  ary[ix + jx * 64] = place ();
  }

  return check (ary, size, 0,  1, 1);
}

int gang_1 (int *ary, int size)
{
  clear (ary, size);
  
#pragma acc parallel num_gangs (32) num_workers (32) vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop auto
    for (int jx = 0; jx <  size  / 64; jx++)
#pragma acc loop worker
      for (int ix = 0; ix < 64; ix++)
	ary[ix + jx * 64] = place ();
  }

  return check (ary, size, 1, 1, 0);
}

int gang_2 (int *ary, int size)
{
  clear (ary, size);
  
#pragma acc parallel num_gangs (32) num_workers (32) vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop auto
    for (int kx = 0; kx < size / (32 * 32); kx++)
#pragma acc loop auto
      for (int jx = 0; jx <  32; jx++)
#pragma acc loop auto
	for (int ix = 0; ix < 32; ix++)
	  ary[ix + jx * 32 + kx * 32 * 32] = place ();
  }

  return check (ary, size, 1, 1, 1);
}

int gang_3 (int *ary, int size)
{
  clear (ary, size);
  
#pragma acc parallel num_workers (32) vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop auto
    for (int jx = 0; jx <  size  / 64; jx++)
#pragma acc loop auto
      for (int ix = 0; ix < 64; ix++)
	ary[ix + jx * 64] = place ();
  }

  return check (ary, size, 1, 1, 1);
}

int gang_4 (int *ary, int size)
{
  clear (ary, size);
  
#pragma acc parallel vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop auto
    for (int jx = 0; jx <  size; jx++)
      ary[jx] = place ();
  }

  return check (ary, size, 1, 0, 1);
}

#define N (32*32*32*2)
int main ()
{
  int ondev = 0;

#pragma acc parallel copy(ondev)
  {
    ondev = acc_on_device (acc_device_not_host);
  }
  if (!ondev)
    return 0;
  
  int ary[N];

  if (vector_1 (ary,  N))
    return 1;
  if (vector_2 (ary,  N))
    return 1;

  if (worker_1 (ary,  N))
    return 1;
  
  if (gang_1 (ary,  N))
    return 1;
  if (gang_2 (ary,  N))
    return 1;
  if (gang_3 (ary,  N))
    return 1;
  if (gang_4 (ary,  N))
    return 1;

  return 0;
}
