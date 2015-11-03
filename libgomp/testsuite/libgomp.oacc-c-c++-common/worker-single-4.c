#include <assert.h>

/* Test worker-single/vector-partitioned mode.  */

int
main (int argc, char *argv[])
{
  int arr[32], i;

  for (i = 0; i < 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(1) num_workers(8) vector_length(32)
      {
	int k;
	#pragma acc loop vector
	for (k = 0; k < 32; k++)
	  {
	    #pragma acc atomic
	    arr[k]++;
	  }
      }

  for (i = 0; i < 32; i++)
    assert (arr[i] == i + 1);

  return 0;
}
