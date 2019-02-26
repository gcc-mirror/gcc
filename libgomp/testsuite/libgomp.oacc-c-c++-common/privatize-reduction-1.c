#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char *argv[])
{
#define N 100
  int n = N;
  int i, j, tmp;
  int input[N*N], output[N], houtput[N];

  for (i = 0; i < n * n; i++)
    input[i] = i;

  for (i = 0; i < n; i++)
    {
      tmp = 0;
      for (j = 0; j < n; j++)
	tmp += input[i * n + j];
      houtput[i] = tmp;
    }

  #pragma acc parallel loop gang
  for (i = 0; i < n; i++)
    {
      tmp = 0;

      #pragma acc loop worker reduction(+:tmp)
      for (j = 0; j < n; j++)
	tmp += input[i * n + j];

      output[i] = tmp;
    }

  /* Test if every worker-level reduction had correct private result.  */
  for (i = 0; i < n; i++)
    if (houtput[i] != output[i])
      abort ();

  return 0;
}
