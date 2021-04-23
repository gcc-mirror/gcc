/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N (32*32*32+17)
int main ()
{
  int ix;
  int ondev = 0;
  int q = 0,  h = 0;
  int workersize;

#pragma acc parallel num_workers(32) vector_length(32) copy(q) copy(ondev) \
	    copyout(workersize)
  /* { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-2 } */
  {
    int t = q;
    
#pragma acc loop worker reduction(+:t)
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
    q = t;
    workersize = __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
  }

  for (ix = 0; ix < N; ix++)
    {
      int val = ix;
      if(ondev)
	{
	  int g = 0;
	  int w = ix % workersize;
	  int v = 0;

	  val = (g << 16) | (w << 8) | v;
	}
      h += val;
    }
  if (q != h)
    {
      printf ("t=%x expected %x\n", q, h);
      return 1;
    }
  
  return 0;
}
