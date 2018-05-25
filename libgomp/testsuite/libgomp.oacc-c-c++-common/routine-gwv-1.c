#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N (32*32*32+17)

#pragma acc routine gang
void __attribute__ ((noinline)) gang (int ary[N])
{
#pragma acc loop gang worker vector
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

  for (ix = 0; ix < N;ix++)
    ary[ix] = -1;
  
#pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) copy(ary) copy(ondev)
  {
    ondev = acc_on_device (acc_device_not_host);
    gang (ary);
  }

  for (ix = 0; ix < N; ix++)
    {
      int expected = ix;
      if(ondev)
	{
	  int chunk_size = (N + 32*32*32 - 1) / (32*32*32);
	  
	  int g = ix / (chunk_size * 32 * 32);
	  int w = ix / 32 % 32;
	  int v = ix % 32;

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
