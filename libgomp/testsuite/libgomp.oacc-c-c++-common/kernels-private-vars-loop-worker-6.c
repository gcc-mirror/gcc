#include <assert.h>

/* Test of worker-private variables declared on a loop directive, broadcasting
   to vector-partitioned mode.  Aggregate worker variable.  */

typedef struct
{
  int x, y;
} vec2;

int
main (int argc, char* argv[])
{
  int i, arr[32 * 32 * 32];
  vec2 pt;

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  {
    int j;

    #pragma acc loop gang(num:32)
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker(num:32) private(pt)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    
	    pt.x = i ^ j * 3;
	    pt.y = i | j * 5;

	    #pragma acc loop vector(length:32)
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt.x * k;
	    
	    #pragma acc loop vector(length:32)
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt.y * k;
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
