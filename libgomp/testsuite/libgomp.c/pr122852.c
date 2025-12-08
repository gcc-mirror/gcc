/* { dg-do compile } */
/* { dg-require-effective-target fopenmp } */
/* { dg-options "-O2 -fopenmp" } */

int a, b, c;

static void
foo (int g)
{
  int f = c ? c : 2;
  if (c)
    b = 3;
  if (!g)
    for (int d = 0; d < f; ++d)
#pragma omp parallel
      while (a)
	;
}

void
bar (void)
{
  foo (1);
}
