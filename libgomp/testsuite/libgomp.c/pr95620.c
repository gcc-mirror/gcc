// { dg-do link { target { { i?86-*-* x86_64-*-* } && lp64 } } }
/* { dg-require-effective-target lto } */
/* { dg-additional-options "-flto -mcmodel=medium" } */

double a[353783808];
int b, c, d;

int
main()
{
  for (; b;)
#pragma omp parallel
    a[c] = 1;
  for (;; b++)
    if (a[c])
      d++;
  return 0;
}
