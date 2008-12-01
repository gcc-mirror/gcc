// PR c++/
// { dg-do run }
// { dg-options "-std=c++0x -fopenmp" }

extern "C" void abort ();
int cnt;

template <typename T>
void
f0 (T, int)
{
  abort ();
}

template <>
void
f0<int> (int, int type)
{
  if (type != 0)
    abort ();
#pragma omp atomic
  cnt++;
}

template <>
void
f0<const char *> (const char *, int type)
{
  if (type != 1)
    abort ();
#pragma omp atomic
  cnt++;
}

template <typename T>
void
f1 ()
{
#pragma omp parallel for
  for (auto i = 0; i < 10; i++)
    f0 (i, 0);
}

template <typename T>
void
f2 ()
{
#pragma omp parallel for
  for (auto i = T (0); i < T (10); i += T (1))
    f0 (i, 0);
}

void
f3 ()
{
#pragma omp parallel for
  for (auto i = 0; i < 10; i++)
    f0 (i, 0);
}

const char *p = "abcdefghij";

template <typename T>
void
f4 ()
{
#pragma omp parallel for
  for (auto i = p; i < p + 10; i++)
    f0 (i, 1);
}

template <typename T>
void
f5 ()
{
#pragma omp parallel for
  for (auto i = T (p); i < T (p + 10); i++)
    f0 (i, 1);
}

void
f6 ()
{
#pragma omp parallel for
  for (auto i = p; i < p + 10; i++)
    f0 (i, 1);
}

int
main ()
{
  f1<int> ();
  if (cnt != 10)
    abort ();
  f2<int> ();
  if (cnt != 20)
    abort ();
  f3 ();
  if (cnt != 30)
    abort ();
  f4<int> ();
  if (cnt != 40)
    abort ();
  f5<const char *> ();
  if (cnt != 50)
    abort ();
  f6 ();
  if (cnt != 60)
    abort ();
}
