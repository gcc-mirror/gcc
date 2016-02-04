/* { dg-do run } */

#include <stdlib.h>

#define EPS 0.0001
#define N 1024*1024

void init (float B[], float C[], int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      B[i] = 0.1 * i;
      C[i] = 0.01 * i * i;
    }
}

float dotprod_ref (float B[], float C[], int n)
{
  int i;
  float sum = 0.0;

  for (i = 0; i < n; i++)
    sum += B[i] * C[i];

  return sum;
}

float dotprod (float B[], float C[], int n)
{
  int i;
  float sum = 0;

  #pragma omp target teams map(to: B[0:n], C[0:n]) \
			   map(tofrom: sum) reduction(+:sum)
    #pragma omp distribute parallel for reduction(+:sum)
      for (i = 0; i < n; i++)
	sum += B[i] * C[i];

  return sum;
}

void check (float a, float b)
{
  float err = (b == 0.0) ? a : (a - b) / b;
  if (((err > 0) ? err : -err) > EPS)
    abort ();
}

int main ()
{
  float *v1 = (float *) malloc (N * sizeof (float));
  float *v2 = (float *) malloc (N * sizeof (float));

  float p1, p2;

  init (v1, v2, N);

  p1 = dotprod_ref (v1, v2, N);
  p2 = dotprod (v1, v2, N);

  check (p1, p2);

  free (v1);
  free (v2);

  return 0;
}
