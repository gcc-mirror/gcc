// { dg-do run }

extern "C" void abort ();

struct S
{
  int s;
  void foo (S &x) { s += x.s; }
  void foo (S &x, bool y) { s += x.s; if (y) abort (); }
  S (const S &x) { s = x.s + 1; }
  S (const S &x, bool y) { s = x.s + 2; if (y) abort (); }
  S () { s = 6; }
  ~S ();
};

S::~S ()
{
  if (s < 6) abort ();
  s = -1;
  /* Ensure the above store is not DSEd.  */
  asm volatile ("" : : "r" (&s) : "memory");
}

void
bar (S &x)
{
  if (x.s != 6) abort ();
  x.s = 15;
}

#pragma omp declare reduction (foo: S: omp_out.foo (omp_in)) \
	initializer (omp_priv (omp_orig, false))
#pragma omp declare reduction (foo: char, int, short: omp_out += omp_in - 4) \
	initializer (omp_priv (4))
#pragma omp declare reduction (+: S: omp_out.foo (omp_in, false)) \
	initializer (omp_priv (omp_orig))

namespace N
{
  #pragma omp declare reduction (foo: S: omp_out.foo (omp_in)) \
	initializer (::bar (omp_priv))
  namespace M {}
}

int
main ()
{
  S a, b, c, s, t, u;
  if (a.s != 6 || b.s != 6 || c.s != 6
      || s.s != 6 || t.s != 6 || u.s != 6) abort ();
  s.s = 9; t.s = 10; u.s = 11;
  int d = 0, e = 0, f = 0, g = 0, h = 30, v = 2, q = 0;
  #pragma omp declare reduction (foo: S: omp_out.foo (omp_in, true)) \
	initializer (omp_priv = omp_orig)
  {
    #pragma omp declare reduction (foo: S: omp_out.foo (omp_in, false)) \
	initializer (omp_priv = omp_orig)
    #pragma omp parallel num_threads (4) reduction (N::operator +: q) \
	reduction (operator +: a, d) reduction (::operator +: b, e) \
	reduction (+: c, f) reduction (::N::M::operator +: g) \
	reduction (::N::min: h) reduction (foo: s) reduction (N::foo: t) \
	reduction (::foo: u) reduction (::foo: v)
    {
      if (a.s != 7 || b.s != 7 || c.s != 7
	  || s.s != 10 || t.s != 15 || u.s != 13
	  || v != 4 || d || e || f || g || h != __INT_MAX__) abort ();
      asm volatile ("" : "+m" (a.s), "+m" (b.s));
      asm volatile ("" : "+m" (c.s), "+r" (d));
      asm volatile ("" : "+r" (e), "+r" (f));
      asm volatile ("" : "+r" (g), "+r" (h));
      asm volatile ("" : "+m" (s.s), "+m" (t.s));
      asm volatile ("" : "+m" (u.s), "+r" (v));
      a.s++; b.s++; c.s++; d++; e++; f++; g++; h = t.s;
      s.s++; t.s++; u.s++; v++; q++;
    }
  }
  if (a.s != 6 + q * 8 || b.s != 6 + q * 8 || c.s != 6 + q * 8
      || d != q || e != q || f != q || g != q || h != 15
      || s.s != 9 + q * 11 || t.s != 10 + q * 16 || u.s != 11 + q * 14
      || v != 2 + q)
    abort ();
}
