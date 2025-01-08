/* { dg-do run } */

#define N 100

void
f (int x[], int y[], int z[])
{
  int i;

  #pragma omp target map(to: x[0:N], y[0:N]) map(from: z[0:N])
    #pragma omp metadirective \
	when (device={arch("nvptx")}: teams loop) \
	default (parallel loop)
      for (i = 0; i < N; i++)
	z[i] = x[i] * y[i];
}

int
main (void)
{
  int x[N], y[N], z[N];
  int i;

  for (i = 0; i < N; i++)
    {
      x[i] = i;
      y[i] = -i;
    }

  f (x, y, z);

  for (i = 0; i < N; i++)
    if (z[i] != x[i] * y[i])
      return 1;

  return 0;
}
