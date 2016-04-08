#include <assert.h>

/* Test of reduction on loop directive (workers and vectors, private reduction
   variable).  */

int
main (int argc, char *argv[])
{
  int i, j, arr[32768], out[32], res = 0, hres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyout(out)
  {
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
        res = j;

	#pragma acc loop worker reduction(+:res)
	for (i = 0; i < 1024; i++)
	  res += arr[j * 1024 + i];

	#pragma acc loop vector reduction(+:res)
	for (i = 1023; i >= 0; i--)
	  res += arr[j * 1024 + i];

	out[j] = res;
      }
  }

  for (j = 0; j < 32; j++)
    {
      hres = j;

      for (i = 0; i < 1024; i++)
	hres += arr[j * 1024 + i] * 2;

      assert (out[j] == hres);
    }

  return 0;
}
