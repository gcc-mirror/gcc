#include <assert.h>

/* Test of worker-private variables declared on a loop directive.  */

int
main (int argc, char* argv[])
{
  int x = 5, i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  {
    int j;

    #pragma acc loop gang(num:32)
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker(num:32) private(x)
	for (j = 0; j < 32; j++)
	  {
	    x = i ^ j * 3;
	    /* Try to ensure 'x' accesses doesn't get optimized into a
	       temporary.  */
	    __asm__ __volatile__ ("");
	    arr[i * 32 + j] += x;
	  }
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + ((i / 32) ^ (i % 32) * 3));

  return 0;
}
