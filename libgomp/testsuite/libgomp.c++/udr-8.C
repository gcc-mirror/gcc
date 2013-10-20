// { dg-do run }

extern "C" void abort ();

struct S;
void foo (S *, S *);
void bar (S &, S &);
#pragma omp declare reduction (+:S:foo (&omp_out, &omp_in))
#pragma omp declare reduction (*:S:bar (omp_out, omp_in))
struct S { int s; S () : s (0) {} };

void
foo (S *x, S *y)
{
  x->s += y->s;
}

void
bar (S &x, S &y)
{
  x.s += y.s;
}

int
main ()
{
  S s, t;
  int i = 0;
  #pragma omp parallel reduction (+:s, i) reduction (*:t)
  {
    if (s.s != 0 || t.s != 0)
      abort ();
    s.s = 2;
    t.s = 3;
    i = 1;
  }
  if (s.s != 2 * i || t.s != 3 * i)
    abort ();
}
