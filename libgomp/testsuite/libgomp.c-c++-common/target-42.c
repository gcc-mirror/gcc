/* PR libgomp/100573 */

int
foo (int a)
{
  #pragma omp target firstprivate(a)
  if (a == 0)
    {
      a++;
      #pragma omp target map(tofrom:a)		/* { dg-warning "'target' construct inside of 'target' region" } */
      a = foo (a);
      #pragma omp target data map(tofrom:a)	/* { dg-warning "'target data' construct inside of 'target' region" } */
      a++;
      #pragma omp target enter data map(to:a)	/* { dg-warning "'target enter data' construct inside of 'target' region" } */
      #pragma omp target exit data map(from:a)	/* { dg-warning "'target exit data' construct inside of 'target' region" } */
    }
  return a + 1;
}

int
main ()
{
  if (foo (1) != 2)
    __builtin_abort ();
  return 0;
}
