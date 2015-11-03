#include <assert.h>

/* Test worker-single/vector-single mode.  */

int
main (int argc, char *argv[])
{
  int arr[32], i;

  for (i = 0; i < 32; i++)
    arr[i] = 0;

  #pragma acc parallel copy(arr) num_gangs(8) num_workers(8) vector_length(32)
  {
    int j;
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
	#pragma acc atomic
	arr[j]++;
      }
  }

  for (i = 0; i < 32; i++)
    assert (arr[i] == 1);

  return 0;
}
