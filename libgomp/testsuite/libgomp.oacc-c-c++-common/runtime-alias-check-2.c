/* Test that a simple array copy does the right thing when the input and
   output data overlap.  The GPU kernel should automatically switch to
   a sequential operation mode in order to give the expected results.

   This test does not check the correctness of the output (there are other
   tests for that), but checks that the code really does select the faster
   path, when it can, by comparing the timing.  */

/* No optimization means no issue with aliasing.
   { dg-skip-if "" { *-*-* } { "-O0" } { "" } } 
   { dg-skip-if "" { *-*-* } { "-foffload=disable" } { "" } } */

#include <stdlib.h>
#include <sys/time.h>
#include <openacc.h>

void f(int *data, int n, int to, int from, int count)
{
  int *a = (int*)acc_deviceptr (data+to);
  int *b = (int*)acc_deviceptr (data+from);

#pragma acc kernels
  for (int i = 0; i < count; i++)
    a[i] = b[i];
}

#define N 1000000
int data[N];

int
main ()
{
  struct timeval start, stop, difference;
  long basetime, aliastime;

  for (int i=0; i < N; i++)
    data[i] = i;

  /* Ensure that the data copies are outside the timed zone.  */
#pragma acc enter data copyin(data[0:N])

  /* Baseline test; no aliasing. The high part of the data is copied to
     the lower part.  */
  int to = 0;
  int from = N/2;
  int count = N/2;
  gettimeofday (&start, NULL);
  f (data, N, to, from, count);
  gettimeofday (&stop, NULL);
  timersub (&stop, &start, &difference);
  basetime = difference.tv_sec * 1000000 + difference.tv_usec;

  /* Check various amounts of data overlap.  */
  int tests[] = {1, 10, N/4, N/2-10, N/2-1};
  for (int i = 0; i < sizeof (tests)/sizeof(tests[0]); i++)
    {
      to = 0;
      from = N/2 - tests[i];
      gettimeofday (&start, NULL);
      f (data, N, to, from, count);
      gettimeofday (&stop, NULL);
      timersub (&stop, &start, &difference);
      aliastime = difference.tv_sec * 1000000 + difference.tv_usec;

      /* If the aliased runtime is less than 200% of the non-aliased runtime
	 then the runtime alias check probably selected the wrong path.
	 (Actually we expect the difference to be far greater than that.)  */
      if (basetime*2 > aliastime)
	exit (1);
    }

  /* Repeat the baseline check just to make sure it didn't also get slower
     after the first run.  */
  to = 0;
  from = N/2;
  gettimeofday (&start, NULL);
  f (data, N, to, from, count);
  gettimeofday (&stop, NULL);
  timersub (&stop, &start, &difference);
  int controltime = difference.tv_sec * 1000000 + difference.tv_usec;

  /* The two times should be roughly the same, but we just check it wouldn't
     pass the aliastime test above.  */
  if (basetime*2 <= controltime)
    exit (2);

#pragma acc exit data copyout(data[0:N])

  return 0;
}
