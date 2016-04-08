#include <assert.h>

/* Test of reduction on both parallel and loop directives (workers and vectors
   in gang-partitioned mode, int type with XOR).  */

int
main (int argc, char *argv[])
{
  int i, j, arr[32768], res = 0, hres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       reduction(^:res)
  {
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
	#pragma acc loop worker vector reduction(^:res)
	for (i = 0; i < 1024; i++)
	  res ^= arr[j * 1024 + i];

	#pragma acc loop worker vector reduction(^:res)
	for (i = 0; i < 1024; i++)
	  res ^= arr[j * 1024 + (1023 - i)];
      }
  }

  for (j = 0; j < 32; j++)
    for (i = 0; i < 1024; i++)
      {
        hres ^= arr[j * 1024 + i];
	hres ^= arr[j * 1024 + (1023 - i)];
      }

  assert (res == hres);

  return 0;
}
