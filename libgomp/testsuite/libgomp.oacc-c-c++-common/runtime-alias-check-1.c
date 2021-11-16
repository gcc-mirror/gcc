/* Test that a simple array copy does the right thing when the input and
   output data overlap.  The GPU kernel should automatically switch to
   a sequential operation mode in order to give the expected results.  */

#include <stdlib.h>
#include <openacc.h>

void f(int *data, int n, int to, int from, int count)
{
  /* We cannot use copyin for two overlapping arrays because we get an error
     that the memory is already present.  We also cannot do the pointer
     arithmetic inside the kernels region because it just ends up using
     host pointers (bug?).  Using enter data with a single array, and
     acc_deviceptr solves the problem.  */
#pragma acc enter data copyin(data[0:n])

  int *a = (int*)acc_deviceptr (data+to);
  int *b = (int*)acc_deviceptr (data+from);

#pragma acc kernels
  for (int i = 0; i < count; i++)
    a[i] = b[i];

#pragma acc exit data copyout(data[0:n])
}

#define N 2000

int data[N];

int
main ()
{
  for (int i=0; i < N; i++)
    data[i] = i;

  /* Baseline test; no aliasing. The high part of the data is copied to
     the lower part.  */
  int to = 0;
  int from = N/2;
  int count = N/2;
  f (data, N, to, from, count);
  for (int i=0; i < N; i++)
    if (data[i] != (i%count)+count)
      exit (1);

  /* Check various amounts of data overlap.  */
  int tests[] = {1, 10, N/4, N/2-10, N/2-1};
  for (int t = 0; t < sizeof (tests)/sizeof(tests[0]); t++)
    {
      for (int i=0; i < N; i++)
	data[i] = i;

      /* Output overlaps the latter part of input; expect the initial no-aliased
	 part of the input to repeat throughout the aliased portion.  */
      to = tests[t];
      from = 0;
      count = N-tests[t];
      f (data, N, to, from, count);
      for (int i=0; i < N; i++)
	if (data[i] != i%tests[t])
	exit (2);

      for (int i=0; i < N; i++)
	data[i] = i;

      /* Input overlaps the latter part of the output; expect the copy to work
	 in the obvious manner.  */
      to = 0;
      from = tests[t];
      count = N-tests[t];
      f (data, N, to, from, count);
      for (int i=0; i < count; i++)
	if (data[i+to] != i+tests[t])
	exit (3);
    }

  return 0;
}
