#include <assert.h>

/* Test of reduction on loop directive (gangs, workers and vectors, non-private
   reduction variable: separate gang and worker/vector loops).  */

int
main (int argc, char *argv[])
{
  int i, j;
  double arr[32768], res = 0, hres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copyin(arr) copy(res)
  {
    #pragma acc loop gang reduction(+:res)
    for (j = 0; j < 32; j++)
      {
        #pragma acc loop worker vector reduction(+:res)
        for (i = 0; i < 1024; i++)
	  res += arr[j * 1024 + i];
      }
  }

  for (i = 0; i < 32768; i++)
    hres += arr[i];

  assert (res == hres);

  return 0;
}
