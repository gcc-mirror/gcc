/* { dg-do run } */

#define N 100

int
f (int a[], int flag)
{
  int i;
  int res = 0;

  #pragma omp metadirective \
	when (user={condition(!flag)}: \
		target teams distribute parallel for \
		  map(from: a[0:N]) private(res)) \
	default (parallel for)
  for (i = 0; i < N; i++)
    {
      a[i] = i;
      res = 1;
    }

  return res;
}

int
main (void)
{
  int a[N];

  if (f (a, 0))
    return 1;
  if (!f (a, 1))
    return 1;

  return 0;
}
