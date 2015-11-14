/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

extern void abort (void);
int a[1024], b = -1;

int
main ()
{
  int i;
  #pragma omp parallel for simd ordered
  for (i = 0; i < 1024; i++)
    {
      a[i] = i;
      #pragma omp ordered threads simd
      {
	if (b + 1 != i)
	  abort ();
	b = i;
      }
      a[i] += 3;
    }
  if (b != 1023)
    abort ();
  for (i = 0; i < 1024; i++)
    if (a[i] != i + 3)
      abort ();
  return 0;
}
