#include <assert.h>

/* Test of vector-private variables declared on loop directive.  */

int
main (int argc, char* argv[])
{
  int x, i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  {
    int j;

    #pragma acc loop gang(num:32)
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker(num:32)
	for (j = 0; j < 32; j++)
	  {
	    int k;

	    #pragma acc loop vector(length:32) private(x)
	    for (k = 0; k < 32; k++)
	      {
		x = i ^ j * 3;
		arr[i * 1024 + j * 32 + k] += x * k;
	      }

	    #pragma acc loop vector(length:32) private(x)
	    for (k = 0; k < 32; k++)
	      {
		x = i | j * 5;
		arr[i * 1024 + j * 32 + k] += x * k;
	      }
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}

  return 0;
}
