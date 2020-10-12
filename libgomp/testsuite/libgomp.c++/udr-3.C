// { dg-do run }

extern "C" void abort ();

void
dblinit (double *p)
{
  *p = 2.0;
}

namespace NS
{
  template <int N>
  struct U
  {
    void foo (U &, bool);
    U ();
  };
  template <int N>
  struct S
  {
    int s;
    #pragma omp declare reduction (foo : U<0>, S : omp_out.foo (omp_in, false))
    #pragma omp declare reduction (foo : int : omp_out += omp_in) \
	initializer (omp_priv = N + 2)
    #pragma omp declare reduction (foo : double : omp_out += omp_in) \
	initializer (dblinit (&omp_priv))
    void baz (int v)
    {
      S s;
      int q = 0;
      if (s.s != 6 || v != 0) abort ();
      s.s = 20;
      double d = 4.0;
      #pragma omp parallel num_threads (4) reduction (foo : s, v, d) \
	reduction (::NS::U<N>::operator + : q)
      {
	if (s.s != 6 || q != 0 || v != N + 2 || d != 2.0) abort ();
	asm volatile ("" : "+m" (s.s), "+r" (q), "+r" (v));
	s.s++; q++; v++;
      }
      if (s.s != 20 + q * 7 || (N + 3) * q != v || d != 4.0 + 2.0 * q)
	abort ();
    }
    void foo (S &x) { s += x.s; }
    void foo (S &x, bool y) { s += x.s; if (y) abort (); }
    S (const S &x) { s = x.s + 1; }
    S (const S &x, bool y) { s = x.s + 2; if (y) abort (); }
    S () { s = 6; }
    S (int x) { s = x; }
    ~S ();
  };
  #pragma omp declare reduction (bar : S<1> : omp_out.foo (omp_in)) \
	initializer (omp_priv (8))
}

template <int N>
NS::S<N>::~S ()
{
  if (s < 6) abort ();
  s = -1;
  /* Ensure the above store is not DSEd.  */
  asm volatile ("" : : "r" (&s) : "memory");
}

template <int N>
struct T : public NS::S<N>
{
  void baz ()
  {
    NS::S<N> s;
    int q = 0;
    if (s.s != 6) abort ();
    #pragma omp parallel num_threads (4) reduction (foo:s) \
	reduction (+: q)
    {
      if (s.s != 6 || q != 0) abort ();
      asm volatile ("" : "+m" (s.s), "+r" (q));
      s.s += 2; q++;
    }
    if (s.s != 6 + q * 8) abort ();
  }
};

struct W
{
  int v;
  W () : v (6) {}
  W (int i) : v (i) {}
  ~W () {}
};

template <typename T, typename D>
struct V
{
  #pragma omp declare reduction (baz: T: omp_out.s += omp_in.s) \
	initializer (omp_priv (11))
  #pragma omp declare reduction (baz: D: omp_out += omp_in) \
	initializer (dblinit (&omp_priv))
  static void dblinit (D *x) { *x = 3.0; }
  void baz ()
  {
    T t;
    V v;
    int q = 0;
    D d = 4.0;
    if (t.s != 6 || v.v != 4) abort ();
    #pragma omp declare reduction (+ : V, W : omp_out.v -= omp_in.v) \
	initializer (omp_priv (12))
    {
      #pragma omp declare reduction (+ : W, V : omp_out.v += omp_in.v) \
	initializer (omp_priv (9))
      #pragma omp parallel num_threads (4) reduction (+: v, q) \
	reduction (baz: t, d)
      {
	if (t.s != 11 || v.v != 9 || q != 0 || d != 3.0) abort ();
	asm volatile ("" : "+m" (t.s), "+m" (v.v), "+r" (q));
	t.s += 2; v.v += 3; q++;
      }
      if (t.s != 6 + 13 * q || v.v != 4 + 12 * q || d != 4.0 + 3.0 * q)
	abort ();
    }
  }
  int v;
  V () : v (4) {}
  V (int x) : v (x) {}
  ~V () {}
};

int
main ()
{
  NS::S<0> u;
  u.baz (0);
  T<2> t;
  t.baz ();
  NS::S<1> s;
  int q = 0;
  if (s.s != 6) abort ();
  // Test ADL
  #pragma omp parallel num_threads (4) reduction (bar:s) reduction (+:q)
  {
    if (s.s != 8 || q != 0) abort ();
    asm volatile ("" : "+m" (s.s), "+r" (q));
    s.s += 4; q++;
  }
  if (s.s != 6 + q * 12) abort ();
  V <NS::S <0>, double> v;
  v.baz ();
}
