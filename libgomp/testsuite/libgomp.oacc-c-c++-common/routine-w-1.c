/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N (32*32*32+17)

#pragma acc routine worker
void __attribute__ ((noinline)) worker (int ary[N])
/* { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-1 } */
{
#pragma acc loop worker
  for (unsigned ix = 0; ix < N; ix++)
    {
      if (acc_on_device (acc_device_not_host))
	{
	  int g, w, v;

	  g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
	  w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
	  v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
	  ary[ix] = (g << 16) | (w << 8) | v;
	}
      else
	ary[ix] = ix;
    }
}

int main ()
{
  int ary[N];
  int ix;
  int exit = 0;
  int ondev = 0;
  int workersize;

  for (ix = 0; ix < N;ix++)
    ary[ix] = -1;
  
#define NW 32
#define VL 32
#pragma acc parallel num_workers(NW) vector_length(VL) \
	    copy(ary) copy(ondev)
  {
    ondev = acc_on_device (acc_device_not_host);
    worker (ary);
  }
  workersize = NW;
#ifdef ACC_DEVICE_TYPE_radeon
  /* AMD GCN has an upper limit of 'num_workers(16)'.  */
  if (workersize > 16)
    workersize = 16;
#endif

  for (ix = 0; ix < N; ix++)
    {
      int expected = ix;
      if(ondev)
	{
	  int g = 0;
	  int w = ix % workersize;
	  int v = 0;

	  expected = (g << 16) | (w << 8) | v;
	}
      
      if (ary[ix] != expected)
	{
	  exit = 1;
	  printf ("ary[%d]=%x expected %x\n", ix, ary[ix], expected);
	}
    }
  
  return exit;
}
