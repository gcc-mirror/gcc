/* { dg-do run } */

extern
#ifdef __cplusplus
"C"
#endif
void abort ();

struct S { int s; struct S *t; };

void
foo (struct S *out, struct S *in)
{
  out->s += in->s;
}

void
bar (struct S *x)
{
  if (x->s != 6) abort ();
  x->s = 15;
}

void
baz (struct S *x, struct S *y)
{
  x->s = 6;
  x->t = x;
  (void) y;
}

#pragma omp declare reduction (foo: struct S: foo (&omp_out, &omp_in)) \
	initializer (omp_priv = { 8, &omp_priv })
#pragma omp declare reduction (foo: char, int, short: omp_out += omp_in - 4) \
	initializer (omp_priv = 4)
#pragma omp declare reduction (+: struct S: foo (&omp_out, &omp_in)) \
	initializer (baz (&omp_priv, &omp_orig))

void
test (struct S s, struct S t)
{
  int q = 0;
  #pragma omp parallel num_threads (4) reduction (+: s, q) reduction (foo: t)
  {
    if (s.s != 6 || s.t != &s || t.s != 8 || t.t != &t)
      abort ();
    s.s = 2;
    t.s = 3;
    q = 1;
  }
  if (s.s != 12 + 2 * q || t.s != 14 + 3 * q)
    abort ();
}

int
main ()
{
  struct S s, t;
  s.s = 9; t.s = 10;
  int h = 30, v = 2, q = 0;
  #pragma omp declare reduction (foo: struct S: omp_out.s *= omp_in.s) \
	initializer (omp_priv = omp_orig)
  {
    #pragma omp declare reduction (foo: struct S: omp_out.s += omp_in.s) \
	initializer (omp_priv = omp_orig)
    #pragma omp parallel num_threads (4) reduction (+: t, q) \
	reduction (min: h) reduction (foo: s, v)
    {
      if (s.s != 9 || t.s != 6 || v != 4 || h != __INT_MAX__) abort ();
      asm volatile ("" : "+m" (s.s), "+m" (t.s));
      asm volatile ("" : "+r" (h), "+r" (v));
      h = t.s; s.s++; t.s++; v++; q++;
    }
  }
  if (h != 6 || s.s != 9 + q * 10 || t.s != 10 + q * 7 || v != 2 + q)
    abort ();
  s.s = 12;
  t.s = 14;
  test (s, t);
  return 0;
}
