#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N (32*32*32+17)
int main ()
{
  int ary[N];
  int ix;
  int exit = 0;
  int ondev = 0;
  int workersize, vectorsize;

  for (ix = 0; ix < N;ix++)
    ary[ix] = -1;
  
#pragma acc parallel num_workers(32) vector_length(32) copy(ary) copy(ondev) \
	    copyout(workersize, vectorsize)
  {
#pragma acc loop worker vector
    for (unsigned ix = 0; ix < N; ix++)
      {
	if (acc_on_device (acc_device_not_host))
	  {
	    int g, w, v;

	    g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
	    w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
	    v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
	    ary[ix] = (g << 16) | (w << 8) | v;
	    ondev = 1;
	  }
	else
	  ary[ix] = ix;
      }
    workersize = __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    vectorsize = __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);
  }

  for (ix = 0; ix < N; ix++)
    {
      int expected = ix;
      if(ondev)
	{
	  int g = 0;
#ifdef ACC_DEVICE_TYPE_radeon
#  ifdef __OPTIMIZE__
	  int use_vecsize = 64;
#  else
	  int use_vecsize = vectorsize;
#  endif
	  /* For Radeon, the loop is split into contiguous blocks of
	     chunk_size * vector_size, with chunk_size selected to cover the
	     whole iteration space.  Each block is then autovectorized where
	     possible.  */
	  int chunk_size = (N + workersize * use_vecsize - 1)
			   / (workersize * use_vecsize);
	  int w = ix / (chunk_size * use_vecsize);
	  int v = 0;
#else
	  int w = (ix / vectorsize) % workersize;
	  int v = ix % vectorsize;
#endif

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
