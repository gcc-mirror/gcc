// { dg-do run }

extern "C" void abort ();

namespace NS
{
  struct U
  {
    void foo (U &, bool);
    U ();
  };
  struct S
  {
    int s;
    #pragma omp declare reduction (foo : U, S : omp_out.foo (omp_in, false))
    #pragma omp declare reduction (foo : int : omp_out += omp_in) \
	initializer (omp_priv = int ())
    void baz (int v)
    {
      S s;
      int q = 0;
      if (s.s != 6 || v != 0) abort ();
      s.s = 20;
      #pragma omp parallel num_threads (4) reduction (foo : s, v) \
	reduction (::NS::U::operator + : q)
      {
	if (s.s != 6 || q != 0 || v != 0) abort ();
	asm volatile ("" : "+m" (s.s), "+r" (q), "+r" (v));
	s.s++; q++; v++;
      }
      if (s.s != 20 + q * 7 || q != v) abort ();
    }
    void foo (S &x) { s += x.s; }
    void foo (S &x, bool y) { s += x.s; if (y) abort (); }
    S (const S &x) { s = x.s + 1; }
    S (const S &x, bool y) { s = x.s + 2; if (y) abort (); }
    S () { s = 6; }
    S (int x) { s = x; }
    ~S ();
  };
  #pragma omp declare reduction (bar : S : omp_out.foo (omp_in)) \
	initializer (omp_priv (8))
}

NS::S::~S ()
{
  if (s < 6) abort ();
  s = -1;
  /* Ensure the above store is not DSEd.  */
  asm volatile ("" : : "r" (&s) : "memory");
}

struct T : public NS::S
{
  void baz ()
  {
    S s;
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

int
main ()
{
  NS::S s;
  s.baz (0);
  T t;
  t.baz ();
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
}
