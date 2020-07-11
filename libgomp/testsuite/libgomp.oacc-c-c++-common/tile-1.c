/* AMD GCN does not use 32-lane vectors, so the expected use counts mismatch.
   { dg-skip-if "unsuitable dimensions" { openacc_radeon_accel_selected } { "*" } { "" } } */

/* { dg-additional-options "-fopenacc-dim=32" } */

#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

static int check (const int *ary, int size, int gp, int wp, int vp)
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

  if (acc_on_device (acc_device_not_host))
    {
      int g, w, v;

      g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
      w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
      v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
      r = (g << 16) | (w << 8) | v;
    }
  return r;
}

static void clear (int *ary, int size)
{
  int ix;

  for (ix = 0; ix < size; ix++)
    ary[ix] = -1;
}

int gang_vector_1 (int *ary, int size)
{
  clear (ary, size);
#pragma acc parallel vector_length(32) num_gangs (32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(128) gang vector
    for (int jx = 0; jx < size; jx++)
      ary[jx] = place ();
  }

  return check (ary, size, 1, 0, 1);
}

int gang_vector_2a (int *ary, int size)
{
  if (size % 256)
    return 1;
  
  clear (ary, size);
#pragma acc parallel vector_length(32) num_gangs (32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(64, 64) gang vector
    for (int jx = 0; jx < size / 256; jx++)
      for (int ix = 0; ix < 256; ix++)
	ary[jx * 256 + ix] = place ();
  }

  return check (ary, size, 1, 0, 1);
}

int gang_vector_2b (int *ary, int size)
{
  if (size % 256)
    return 1;
  
  clear (ary, size);
#pragma acc parallel vector_length(32) num_gangs (32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(64, 64) gang vector
    for (int jx = 0; jx < size; jx += 256)
      for (int ix = 0; ix < 256; ix++)
	ary[jx + ix] = place ();
  }

  return check (ary, size, 1, 0, 1);
}

int worker_vector_2a (int *ary, int size)
{
  if (size % 256)
    return 1;

  clear (ary, size);
#pragma acc parallel vector_length(32) num_workers (32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(64, 64) worker vector
    for (int jx = 0; jx < size / 256; jx++)
      for (int ix = 0; ix < 256; ix++)
	ary[jx * 256 + ix] = place ();
  }

  return check (ary, size, 0, 1, 1);
}

int worker_vector_2b (int *ary, int size)
{
  if (size % 256)
    return 1;

  clear (ary, size);
#pragma acc parallel vector_length(32) num_workers (32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(64, 64) worker vector
    for (int jx = 0; jx < size; jx += 256)
      for (int ix = 0; ix < 256; ix++)
	ary[jx + ix] = place ();
  }

  return check (ary, size, 0, 1, 1);
}

int gang_worker_vector_2a (int *ary, int size)
{
  if (size % 256)
    return 1;
  clear (ary, size);
#pragma acc parallel vector_length(32) num_workers (32) num_gangs(32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(32, 32)
    for (int jx = 0; jx < size / 256; jx++)
      for (int ix = 0; ix < 256; ix++)
	ary[jx * 256 + ix] = place ();
  }

  return check (ary, size, 1, 1, 1);
}

int gang_worker_vector_2b (int *ary, int size)
{
  if (size % 256)
    return 1;
  clear (ary, size);
#pragma acc parallel vector_length(32) num_workers (32) num_gangs(32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(32, 32)
    for (int jx = 0; jx < size; jx += 256)
      for (int ix = 0; ix < 256; ix++)
	ary[jx + ix] = place ();
  }

  return check (ary, size, 1, 1, 1);
}

int gang_worker_vector_star_2a (int *ary, int size)
{
  if (size % 256)
    return 1;

  clear (ary, size);
#pragma acc parallel vector_length(32) num_workers (32) num_gangs(32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(*, *)
    for (int jx = 0; jx < size / 256; jx++)
      for (int ix = 0; ix < 256; ix++)
	ary[jx * 256 + ix] = place ();
  }

  return check (ary, size, 1, 1, 1);
}

int gang_worker_vector_star_2b (int *ary, int size)
{
  if (size % 256)
    return 1;

  clear (ary, size);
#pragma acc parallel vector_length(32) num_workers (32) num_gangs(32) copy (ary[0:size]) firstprivate (size)
  {
#pragma acc loop tile(*, *)
    for (int jx = 0; jx < size; jx +=256)
      for (int ix = 0; ix < 256; ix++)
	ary[jx + ix] = place ();
  }

  return check (ary, size, 1, 1, 1);
}

#define N (32*32*32*8)
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
  if (gang_vector_1 (ary, N))
    return 1;
  if (gang_vector_2a (ary, N))
    return 1;
  if (worker_vector_2a (ary, N))
    return 1;
  if (gang_worker_vector_2a (ary, N))
    return 1;
  if (gang_worker_vector_star_2a (ary, N))
    return 1;
  if (gang_vector_2b (ary, N))
    return 1;
  if (worker_vector_2b (ary, N))
    return 1;
  if (gang_worker_vector_2b (ary, N))
    return 1;
  if (gang_worker_vector_star_2b (ary, N))
    return 1;
  return 0;
}
