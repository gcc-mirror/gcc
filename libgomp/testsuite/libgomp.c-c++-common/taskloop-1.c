/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-std=c99" { target c } } */

int q, r, e;

__attribute__((noinline, noclone)) void
foo (long a, long b)
{
  #pragma omp taskloop lastprivate (q) nogroup
    for (long d = a; d < b; d += 2)
      {
	q = d;
	if (d < 2 || d > 6 || (d & 1))
	  #pragma omp atomic
	    e |= 1;
      }
}

__attribute__((noinline, noclone)) int
bar (int a, int b)
{
  int q = 7;
  #pragma omp taskloop lastprivate (q)
    for (int d = a; d < b; d++)
      {
	if (d < 12 || d > 17)
	  #pragma omp atomic
	    e |= 1;
	q = d;
      }
  return q;
}

int
main ()
{
  #pragma omp parallel
    #pragma omp single
      {
	foo (2, 7);
	r = bar (12, 18);
      }
  if (q != 6 || r != 17 || e)
    __builtin_abort ();
  return 0;
}
