// PR c++/81314
// { dg-do link }

template <int N>
struct S {
  S () { s = 0; }
  S (const S &x) { s = x.s; }
  ~S () {}
  int s;
};

void
foo (S<2> &x)
{
  #pragma omp taskloop
  for (int i = 0; i < 100; ++i)
    x.s++;
}

void
bar (S<3> &x)
{
  #pragma omp task
  x.s++;
}

int
main ()
{
  S<2> s;
  S<3> t;
  #pragma omp parallel
  #pragma omp master
  {
    foo (s);
    bar (t);
  }
}
