// { dg-do run }

extern "C" void abort ();

struct S
{
  void foo ()
  {
    S s;
    int j = 0;
    #pragma omp declare reduction (bar : int : omp_out += omp_in)
    #pragma omp parallel reduction (bar : s) reduction(S::operator+ : j)
    s.a = 4, j = 1;
    if (s.a != 4 * j) abort ();
  }
  #pragma omp declare reduction (bar : S : baz (omp_out, omp_in))
  static void baz (S &x, S &y) { x.a += y.a; }
  S () : a (0) {}
  int a;
};

template <int N>
struct T
{
  void foo ()
  {
    S s;
    T t;
    int j = 0;
    #pragma omp declare reduction (bar : int : omp_out += omp_in)
    #pragma omp parallel reduction (bar : t) reduction (S::bar : s) \
			 reduction(T<N>::operator+ : j)
    s.a = 4, t.a = 5, j = 1;
    if (s.a != 4 * j || t.a != 5 * j) abort ();
  }
  #pragma omp declare reduction (bar : T<N> : baz (omp_out, omp_in))
  static void baz (T &x, T &y) { x.a += y.a; }
  T () : a (N) {}
  int a;
};

int
main ()
{
  S s;
  s.foo ();
  T<0> t;
  t.foo ();
}
