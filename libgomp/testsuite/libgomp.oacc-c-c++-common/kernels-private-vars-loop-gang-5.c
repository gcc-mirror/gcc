#include <assert.h>

/* Test of gang-private array variable declared on loop directive, with
   broadcasting to partitioned workers.  */

int
main (int argc, char* argv[])
{
  int x[8], i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  {
    #pragma acc loop gang(num:32) private(x)
    for (i = 0; i < 32; i++)
      {
        for (int j = 0; j < 8; j++)
	  x[j] = j * 2;

	#pragma acc loop worker(num:32)
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += x[j % 8];
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i % 8) * 2);

  return 0;
}
