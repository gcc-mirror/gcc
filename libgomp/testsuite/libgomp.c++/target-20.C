extern "C" void abort ();
struct S { int a, b, c, d; };

void
foo (S &s)
{
  int err;
  #pragma omp target map (s.b, s.d) map (from: err)
  {
    err = s.b != 21 || s.d != 24;
    s.b++; s.d++;
  }
  if (err || s.b != 22 || s.d != 25)
    abort ();
  #pragma omp target data map (s.b, s.d)
  {
    #pragma omp target map (alloc: s.b, s.d) map (from: err)
    {
      err = s.b != 22 || s.d != 25;
      s.b++; s.d++;
    }
  }
  if (err || s.b != 23 || s.d != 26)
    abort ();
  #pragma omp target data map (s)
  {
    #pragma omp target map (alloc: s.b, s.d) map (from: err)
    {
      err = s.b != 23 || s.d != 26;
      s.b++; s.d++;
    }
  }
  if (err || s.b != 24 || s.d != 27)
    abort ();
}

template <typename T, typename U>
void
bar (S &s, T &t, U u)
{
  int err;
  #pragma omp target map (s.b, s.d, t.b, t.d, u.b, u.d) map (from: err)
  {
    err = s.b != 21 || s.d != 24 || t.b != 73 || t.d != 82 || u.b != 31 || u.d != 37;
    s.b++; s.d++; t.b++; t.d++; u.b++; u.d++;
  }
  if (err || s.b != 22 || s.d != 25 || t.b != 74 || t.d != 83 || u.b != 32 || u.d != 38)
    abort ();
  #pragma omp target data map (s.b, s.d, t.b, t.d, u.b, u.d)
  {
    #pragma omp target map (alloc: s.b, s.d, t.b, t.d, u.b, u.d) map (from: err)
    {
      err = s.b != 22 || s.d != 25 || t.b != 74 || t.d != 83 || u.b != 32 || u.d != 38;
      s.b++; s.d++; t.b++; t.d++; u.b++; u.d++;
    }
  }
  if (err || s.b != 23 || s.d != 26 || t.b != 75 || t.d != 84 || u.b != 33 || u.d != 39)
    abort ();
  #pragma omp target data map (s, t, u)
  {
    #pragma omp target map (alloc: s.b, s.d, t.b, t.d, u.b, u.d) map (from: err)
    {
      err = s.b != 23 || s.d != 26 || t.b != 75 || t.d != 84 || u.b != 33 || u.d != 39;
      s.b++; s.d++; t.b++; t.d++; u.b++; u.d++;
    }
  }
  if (err || s.b != 24 || s.d != 27 || t.b != 76 || t.d != 85 || u.b != 34 || u.d != 40)
    abort ();
}

int
main ()
{
  S s = { 1, 21, 2, 24 };
  foo (s);
  S s2 = { 3, 21, 4, 24 };
  S t = { 5, 73, 6, 82 };
  S u = { 7, 31, 8, 37 };
  bar <S, S &> (s2, t, u);
}
