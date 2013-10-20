/* { dg-do run } */

extern void abort ();

struct S { int s; };

#pragma omp declare reduction (+:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:int:omp_out += omp_in)

int
main ()
{
  int u = 0, q = 0;
  struct S s, t;
  s.s = 0; t.s = 0;
  #pragma omp parallel reduction(+:s, q) reduction(foo:t, u)
  {
    if (s.s != 0 || t.s != 0 || u != 0 || q != 0) abort ();
    s.s = 6;
    t.s = 8;
    u = 9;
    q++;
  }
  if (s.s != 6 * q || t.s != 8 * q || u != 9 * q) abort ();
  return 0;
}
