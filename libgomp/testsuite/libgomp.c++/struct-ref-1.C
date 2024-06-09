// { dg-do run }
// { dg-options "-fopenmp" }

#include <cassert>

struct S
{
  int x[10];
};

void
foo (S *s, int x)
{
  S *&r = s;
  for (int i = 0; i < x; i++)
    s[0].x[i] = s[1].x[i] = 0;
  #pragma omp target map (s, x)
    ;
  #pragma omp target map (s[0], x)
  for (int i = 0; i < x; i++)
    s[0].x[i] = i;
  #pragma omp target map (s[1], x)
  for (int i = 0; i < x; i++)
    s[1].x[i] = i * 2;
  for (int i = 0; i < x; i++)
    {
      assert (s[0].x[i] == i);
      assert (s[1].x[i] == i * 2);
      s[0].x[i] = 0;
      s[1].x[i] = 0;
    }
  #pragma omp target map (r, x)
    ;
  #pragma omp target map (r[0], x)
  for (int i = 0; i < x; i++)
    r[0].x[i] = i;
  #pragma omp target map (r[1], x)
  for (int i = 0; i < x; i++)
    r[1].x[i] = i * 2;
  for (int i = 0; i < x; i++)
    {
      assert (r[0].x[i] == i);
      assert (r[1].x[i] == i * 2);
    }
}

template <int N>
struct T
{
  int x[N];
};

template <int N>
void
bar (T<N> *t, int x)
{
  T<N> *&r = t;
  for (int i = 0; i < x; i++)
    t[0].x[i] = t[1].x[i] = 0;
  #pragma omp target map (t, x)
    ;
  #pragma omp target map (t[0], x)
  for (int i = 0; i < x; i++)
    t[0].x[i] = i;
  #pragma omp target map (t[1], x)
  for (int i = 0; i < x; i++)
    t[1].x[i] = i * 2;
  for (int i = 0; i < x; i++)
    {
      assert (t[0].x[i] == i);
      assert (t[1].x[i] == i * 2);
      t[0].x[i] = 0;
      t[1].x[i] = 0;
    }
  #pragma omp target map (r, x)
    ;
  #pragma omp target map (r[0], x)
  for (int i = 0; i < x; i++)
    r[0].x[i] = i;
  #pragma omp target map (r[1], x)
  for (int i = 0; i < x; i++)
    r[1].x[i] = i * 2;
  for (int i = 0; i < x; i++)
    {
      assert (r[0].x[i] == i);
      assert (r[1].x[i] == i * 2);
    }
}

int main (int argc, char *argv[])
{
  S s[2];
  foo (s, 10);
  T<10> t[2];
  bar (t, 10);
  return 0;
}
