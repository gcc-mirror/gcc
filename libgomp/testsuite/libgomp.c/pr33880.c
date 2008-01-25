/* PR middle-end/33880 */
/* { dg-do run } */

extern void abort (void);

void
test1 (void)
{
  int i = 0, j = 0;
  void bar (void)
  {
    i++;
    j++;
  }
  bar ();
  #pragma omp parallel for num_threads(4)
    for (i = 0; i < 100; i++)
      #pragma omp atomic
	j += 1;
  if (j != 101)
    abort ();
  #pragma omp parallel for lastprivate(i) num_threads(2)
    for (i = 0; i < 100; i++)
      #pragma omp atomic
	j += 1;
  if (i != 100)
    abort ();
  i = 3;
  bar ();
  if (j != 202)
    abort ();
  if (i != 4)
    abort ();
}

void
test2 (void)
{
  int i = -1, j = 99, k, l = 9, m = 0;
  void bar (void)
  {
    i++;
    j++;
    l++;
    m++;
  }
  bar ();
  #pragma omp parallel for num_threads(4)
    for (k = i; k < j; k += l)
      #pragma omp atomic
	m += 1;
  bar ();
  if (i != 1 || j != 101 || l != 11 || m != 12)
    abort ();
}

void
test3 (void)
{
  int i, j, k, l, m;
  void bar (void)
  {
  #pragma omp parallel for num_threads(4)
    for (i = j; i < k; i += l)
      #pragma omp atomic
        m += 1;
  }
  void baz (void)
  {
  #pragma omp parallel for num_threads(2) lastprivate(i)
    for (i = j; i < k * 2; i += l / 2)
      #pragma omp atomic
        m += 1;
  }
  i = 7;
  j = 0;
  k = 100;
  l = 2;
  m = 0;
  bar ();
  if (j != 0 || k != 100 || l != 2 || m != 50)
    abort ();
  baz ();
  if (i != 200 || j != 0 || k != 100 || l != 2 || m != 250)
    abort ();
}

void
test4 (void)
{
  int i, j, k, l, m = 0;
  int foo (void)
  {
    return j;
  }
  int bar (void)
  {
    return k;
  }
  int baz (void)
  {
    return l;
  }
  j = 0;
  k = 1000;
  l = 2;
  #pragma omp parallel for num_threads(8) lastprivate(i)
  for (i = foo (); i < bar (); i += baz ())
    #pragma omp atomic
      m += 1;
  if (i != 1000 || m != 500 || j != 0 || k != 1000 || l != 2)
    abort ();
}

int
main (void)
{
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  return 0;
}
