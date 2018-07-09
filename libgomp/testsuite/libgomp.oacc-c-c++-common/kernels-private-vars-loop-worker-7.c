#include <assert.h>

/* Test of worker-private variables declared on loop directive, broadcasting
   to vector-partitioned mode.  Array worker variable.  */

int
main (int argc, char* argv[])
{
  int i, arr[32 * 32 * 32];
  int pt[2];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  /* "pt" is treated as "present_or_copy" on the kernels directive because it
     is an array variable.  */
  #pragma acc kernels copy(arr)
  {
    int j;

    #pragma acc loop gang(num:32)
    for (i = 0; i < 32; i++)
      {
        /* But here, it is made private per-worker.  */
        #pragma acc loop worker(num:32) private(pt)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    
	    pt[0] = i ^ j * 3;

	    #pragma acc loop vector(length:32)
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt[0] * k;

	    pt[1] = i | j * 5;
	    
	    #pragma acc loop vector(length:32)
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt[1] * k;
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
