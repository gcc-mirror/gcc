#include <assert.h>

/* Test of reduction on loop directive (gangs, workers and vectors, multiple
   non-private reduction variables, float type).  */

int
main (int argc, char *argv[])
{
  int i, j;
  float arr[32768];
  float res = 0, mres = 0, hres = 0, hmres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copy(res, mres)
  {
    #pragma acc loop gang reduction(+:res) reduction(max:mres)
    for (j = 0; j < 32; j++)
      {
	#pragma acc loop worker vector reduction(+:res) reduction(max:mres)
	for (i = 0; i < 1024; i++)
	  {
	    res += arr[j * 1024 + i];
	    if (arr[j * 1024 + i] > mres)
	      mres = arr[j * 1024 + i];
	  }

	#pragma acc loop worker vector reduction(+:res) reduction(max:mres)
	for (i = 0; i < 1024; i++)
	  {
	    res += arr[j * 1024 + (1023 - i)];
	    if (arr[j * 1024 + (1023 - i)] > mres)
	      mres = arr[j * 1024 + (1023 - i)];
	  }
      }
  }

  for (j = 0; j < 32; j++)
    for (i = 0; i < 1024; i++)
      {
        hres += arr[j * 1024 + i];
	hres += arr[j * 1024 + (1023 - i)];
	if (arr[j * 1024 + i] > hmres)
	  hmres = arr[j * 1024 + i];
	if (arr[j * 1024 + (1023 - i)] > hmres)
	  hmres = arr[j * 1024 + (1023 - i)];
      }

  assert (res == hres);
  assert (mres == hmres);

  return 0;
}
