// PR c/43893
// { dg-do run }

extern "C" void abort ();

template <typename T, T M, T N>
void
f1 ()
{
  int c;
  T i;
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = M; i < N; i++)
    c++;
  if (c != 1)
    abort ();
}

template <typename T, T M, T N>
void
f2 ()
{
  int c;
  T i;
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = M; i <= N; i++)
    c++;
  if (c != 1)
    abort ();
}

template <typename T, T M, T N>
void
f3 ()
{
  int c;
  T i;
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = M; i > N; i--)
    c++;
  if (c != 1)
    abort ();
}

template <typename T, T M, T N>
void
f4 ()
{
  int c;
  T i;
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = M; i >= N; i--)
    c++;
  if (c != 1)
    abort ();
}

int
main ()
{
  int c;
  unsigned int i;
  int j;
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 0; i < 1; i++)
    c++;
  if (c != 1)
    abort ();
  f1 <unsigned int, 0, 1> ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 0; i <= 0; i++)
    c++;
  if (c != 1)
    abort ();
  f2 <unsigned int, 0, 0> ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = - __INT_MAX__ - 1; j < - __INT_MAX__; j++)
    c++;
  if (c != 1)
    abort ();
  f1 <int, (- __INT_MAX__ - 1), (- __INT_MAX__)> ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = - __INT_MAX__ - 1; j <= - __INT_MAX__ - 1; j++)
    c++;
  if (c != 1)
    abort ();
  f2 <int, (- __INT_MAX__ - 1), (- __INT_MAX__ - 1)> ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 2U * __INT_MAX__ + 1; i > 2U * __INT_MAX__; i--)
    c++;
  if (c != 1)
    abort ();
  f3 <unsigned int, (2U * __INT_MAX__ + 1), (2U * __INT_MAX__)> ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 2U * __INT_MAX__ + 1; i >= 2U * __INT_MAX__ + 1; i--)
    c++;
  if (c != 1)
    abort ();
  f4 <unsigned int, (2U * __INT_MAX__ + 1), (2U * __INT_MAX__ + 1)> ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = __INT_MAX__; j > __INT_MAX__ - 1; j--)
    c++;
  if (c != 1)
    abort ();
  f3 <int, __INT_MAX__, (__INT_MAX__ - 1)> ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = __INT_MAX__; j >= __INT_MAX__; j--)
    c++;
  if (c != 1)
    abort ();
  f4 <int, __INT_MAX__, __INT_MAX__> ();
  return 0;
}
