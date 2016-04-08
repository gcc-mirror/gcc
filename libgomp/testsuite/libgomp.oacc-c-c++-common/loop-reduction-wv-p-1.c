#include <assert.h>

/* Test of reduction on loop directive (workers and vectors, private reduction
   variable).  */

int
main (int argc, char *argv[])
{
  int i, j, arr[1024], out[32], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyout(out)
  {
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
        res = 0;

	#pragma acc loop worker vector reduction(+:res)
	for (i = 0; i < 32; i++)
	  res += arr[j * 32 + i];

	out[j] = res;
      }
  }

  for (j = 0; j < 32; j++)
    {
      hres = 0;

      for (i = 0; i < 32; i++)
	hres += arr[j * 32 + i];

      assert (out[j] == hres);
    }

  return 0;
}
