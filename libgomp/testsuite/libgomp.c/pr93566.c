/* PR middle-end/93566 */
/* { dg-additional-options "-std=c99" } */

extern void abort (void);

void
foo (int *x)
{
  void nest (void) {
    #pragma omp parallel for reduction(+:x[:10])
    for (int i = 0; i < 1024; i++)
      for (int j = 0; j < 10; j++)
	x[j] += j * i;
  }
  nest ();
  for (int i = 0; i < 10; i++)
    if (x[i] != 1023 * 1024 / 2 * i)
      abort ();
}

void
bar (void)
{
  int x[10] = {};
  void nest (void) {
    #pragma omp parallel for reduction(+:x[:10])
    for (int i = 0; i < 1024; i++)
      for (int j = 0; j < 10; j++)
	x[j] += j * i;
  }
  nest ();
  for (int i = 0; i < 10; i++)
    if (x[i] != 1023 * 1024 / 2 * i)
      abort ();
}

void
baz (void)
{
  int x[10] = {};
  void nest (void) {
    #pragma omp parallel for reduction(+:x[2:5])
    for (int i = 0; i < 1024; i++)
      for (int j = 2; j < 7; j++)
	x[j] += j * i;
  }
  nest ();
  for (int i = 2; i < 7; i++)
    if (x[i] != 1023 * 1024 / 2 * i)
      abort ();
}

void
qux (int *x)
{
  void nest (void) { x++; }
  nest ();
  #pragma omp parallel for reduction(+:x[:9])
  for (int i = 0; i < 1024; i++)
    for (int j = 0; j < 9; j++)
      x[j] += j * i;
  nest ();
  for (int i = 0; i < 9; i++)
    if (x[i - 1] != 1023 * 1024 / 2 * i)
      abort ();
}

void
quux (void)
{
  int x[10];
  void nest (void) { for (int i = 0; i < 10; i++) x[i] = 0; }
  int nest2 (int i) { return x[i]; }
  nest ();
  #pragma omp parallel for reduction(+:x[:7])
  for (int i = 0; i < 1024; i++)
    for (int j = 0; j < 7; j++)
      x[j] += j * i;
  for (int i = 0; i < 7; i++)
    if (nest2 (i) != 1023 * 1024 / 2 * i)
      abort ();
}

void
corge (void)
{
  int x[10];
  void nest (void) { for (int i = 0; i < 10; i++) x[i] = 0; }
  int nest2 (int i) { return x[i]; }
  nest ();
  #pragma omp parallel for reduction(+:x[2:4])
  for (int i = 0; i < 1024; i++)
    for (int j = 2; j < 6; j++)
      x[j] += j * i;
  for (int i = 2; i < 6; i++)
    if (nest2 (i) != 1023 * 1024 / 2 * i)
      abort ();
}

int
main ()
{
  int a[10] = {};
  foo (a);
  bar ();
  baz ();
  for (int i = 0; i < 10; i++)
    a[i] = 0;
  qux (a);
  quux ();
  corge ();
  return 0;
}
