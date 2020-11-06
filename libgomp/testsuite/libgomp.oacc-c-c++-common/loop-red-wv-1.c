#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N (32*32*32+17)
int main ()
{
  int ix;
  int ondev = 0;
  int t = 0, h = 0;
  int workersize, vectorsize;
  
#pragma acc parallel num_workers(32) vector_length(32) copy(ondev) \
	    copyout(workersize, vectorsize)
  {
#pragma acc loop worker vector reduction (+:t)
    for (unsigned ix = 0; ix < N; ix++)
      {
	int val = ix;
	
	if (acc_on_device (acc_device_not_host))
	  {
	    int g, w, v;

	    g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
	    w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
	    v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
	    val = (g << 16) | (w << 8) | v;
	    ondev = 1;
	  }
	t += val;
      }
    workersize = __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    vectorsize = __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);
  }

  for (ix = 0; ix < N; ix++)
    {
      int val = ix;
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

	  val = (g << 16) | (w << 8) | v;
	}
      h += val;
    }
  if (t != h)
    {
      printf ("t=%x expected %x\n", t, h);
      return 1;
    }
  
  return 0;
}
