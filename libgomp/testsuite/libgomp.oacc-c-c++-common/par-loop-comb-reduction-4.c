#include <assert.h>

/* Test of reduction on both parallel and loop directives (workers and vectors
   together in gang-partitioned mode, float type, multiple reductions).  */

int
main (int argc, char *argv[])
{
  int i, j;
  float arr[32768];
  float res = 0, mres = 0, hres = 0, hmres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i % (32768 / 64);

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
    reduction(+:res) reduction(max:mres) copy(res, mres)
  {
    #pragma acc loop gang
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

  assert (hres <= 16777216);
  assert (res == hres);

  assert (hmres <= 16777216);
  assert (mres == hmres);

  return 0;
}
