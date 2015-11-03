#include <assert.h>

#if defined(ACC_DEVICE_TYPE_host)
#define ACTUAL_GANGS 1
#else
#define ACTUAL_GANGS 8
#endif

/* Test worker-single, vector-partitioned, gang-redundant mode.  */

int
main (int argc, char *argv[])
{
  int n, arr[32], i;

  for (i = 0; i < 32; i++)
    arr[i] = 0;

  n = 0;

  #pragma acc parallel copy(n, arr) num_gangs(ACTUAL_GANGS) num_workers(8) \
	  vector_length(32)
  {
    int j;

    #pragma acc atomic
    n++;

    #pragma acc loop vector
    for (j = 0; j < 32; j++)
      {
	#pragma acc atomic
	arr[j] += 1;
      }

    #pragma acc atomic
    n++;
  }

  assert (n == ACTUAL_GANGS * 2);

  for (i = 0; i < 32; i++)
    assert (arr[i] == ACTUAL_GANGS);

  return 0;
}
