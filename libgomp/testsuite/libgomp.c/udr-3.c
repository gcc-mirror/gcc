/* { dg-do run } */

extern void abort ();

struct S;
void foo (struct S *, struct S *);
#pragma omp declare reduction (+:struct S:foo (&omp_out, &omp_in))
struct S { int s; };

void
foo (struct S *x, struct S *y)
{
  x->s += y->s;
}

int
main ()
{
  struct S s;
  int i = 0;
  s.s = 0;
  #pragma omp parallel reduction (+:s, i)
  {
    if (s.s != 0)
      abort ();
    s.s = 2;
    i = 1;
  }
  if (s.s != 2 * i)
    abort ();
  return 0;
}
