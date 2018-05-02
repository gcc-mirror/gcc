#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N (32*32*32+17)
int main ()
{
  int ix;
  int ondev = 0;
  int t = 0, h = 0;
  
#pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) copy(ondev)
  {
#pragma acc loop gang worker vector reduction(+:t)
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
  }

  for (ix = 0; ix < N; ix++)
    {
      int val = ix;
      if(ondev)
	{
	  int chunk_size = (N + 32*32*32 - 1) / (32*32*32);
	  
	  int g = ix / (chunk_size * 32 * 32);
	  int w = ix / 32 % 32;
	  int v = ix % 32;

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
