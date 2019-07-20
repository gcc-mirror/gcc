extern
#ifdef __cplusplus
"C"
#endif
void abort (void);
#define N 256
int r;

void
foo (int *a)
{
  int i, j;
  #pragma omp loop bind(thread) order(concurrent) private (j) lastprivate (i) reduction(+:r) collapse(1)
  for (i = 0; i < N; i++)
    {
      j = i - 2;
      a[i] = j;
      r += j;
    }
}

void
bar (int *a)
{
  int i, j;
  #pragma omp loop bind(parallel) order(concurrent) private (j) lastprivate (i) reduction(+:r) collapse(1)
  for (i = 0; i < N; i++)
    {
      j = i;
      a[i] = j;
      r += j;
    }
}

void
baz (int *a)
{
  int i, j;
  #pragma omp loop bind(teams) order(concurrent) private (j) lastprivate (i) reduction(+:r)
  for (i = 0; i < N; i++)
    {
      j = i + 2;
      a[i] = j;
      r += j;
    }
}

int
main ()
{
  int a[N], i, j;
  foo (a);
  for (i = 0; i < N; ++i)
    if (a[i] != i - 2)
      abort ();
    else
      a[i] = -35;
  if (r != N * (N - 5) / 2)
    abort ();
  else
    r = 0;
  bar (a);
  for (i = 0; i < N; ++i)
    if (a[i] != i)
      abort ();
    else
      a[i] = -35;
  if (r != N * (N - 1) / 2)
    abort ();
  else
    r = 0;
  #pragma omp parallel loop private (j) lastprivate (i) reduction(+:r)
  for (i = 0; i < N; i++)
    {
      j = i + 4;
      a[i] = j;
      r += j;
    }
  if (i != N)
    abort ();
  for (i = 0; i < N; ++i)
    if (a[i] != i + 4)
      abort ();
    else
      a[i] = -35;
  if (r != N * (N + 7) / 2)
    abort ();
  else
    r = 0;
  #pragma omp parallel
  bar (a);
  for (i = 0; i < N; ++i)
    if (a[i] != i)
      abort ();
    else
      a[i] = -35;
  if (r != N * (N - 1) / 2)
    abort ();
  else
    r = 0;
  #pragma omp teams
  baz (a);
  for (i = 0; i < N; ++i)
    if (a[i] != i + 2)
      abort ();
    else
      a[i] = -35;
  if (r != N * (N + 3) / 2)
    abort ();
  else
    r = 0;
  #pragma omp teams loop order(concurrent) private (j) lastprivate (i) reduction(+:r) collapse(1)
  for (i = 0; i < N; i++)
    {
      j = i - 4;
      a[i] = j;
      r += j;
    }
  if (i != N)
    abort ();
  for (i = 0; i < N; ++i)
    if (a[i] != i - 4)
      abort ();
  if (r != N * (N - 9) / 2)
    abort ();
  return 0;
}
