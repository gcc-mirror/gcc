#include <assert.h>

/* Test of reduction on loop directive (workers and vectors, private reduction
   variable: gang-redundant mode).  */

int
main (int argc, char *argv[])
{
  int i, arr[1024], out[32], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i ^ 33;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyin(arr) copyout(out)
  {
    /* Private variables aren't initialized by default in openacc.  */
    res = 0;

    /* "res" should be available at the end of the following loop (and should
       have the same value redundantly in each gang).  */
    #pragma acc loop worker vector reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];

    #pragma acc loop gang (static: 1)
    for (i = 0; i < 32; i++)
      out[i] = res;
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  for (i = 0; i < 32; i++)
    assert (out[i] == hres);

  return 0;
}
