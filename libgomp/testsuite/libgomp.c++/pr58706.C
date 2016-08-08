// PR c++/58706
// { dg-do run }
// { dg-options "-std=c++11" }

template <typename T>
T
foo ()
{
  T n = T ();
#pragma omp parallel for reduction (+: n)
  for (T i = [](){ return 3; }(); i < 10; ++i)
    n++;
  return n;
}

template <typename T>
T
bar ()
{
  T n = T ();
#pragma omp parallel for reduction (+: n)
  for (T i = [](){ return 1; }() + [](){ return 4; }(); i < 10; ++i)
    n++;
  return n;
}

template <typename T>
T
baz ()
{
  T n = T ();
#pragma omp parallel for reduction (+: n)
  for (T i = T (); i < [](){ return 7; }() + [](){ return 11; }(); i += [](){ return 3; }() - [](){ return 1; }())
    n++;
  return n;
}

int
main ()
{
  if (foo <int> () != 7 || foo <long long> () != 7)
    __builtin_abort ();
  if (bar <int> () != 5 || bar <char> () != 5)
    __builtin_abort ();
  if (baz <int> () != 9 || baz <long long> () != 9)
    __builtin_abort ();
}
