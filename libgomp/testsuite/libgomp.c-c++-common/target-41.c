/* PR libgomp/100573 */

int
foo (int a)
{
  if (a == 0)
    {
      int c;
      a++;
      #pragma omp target map(tofrom:a)
      a = foo (a);
      #pragma omp target data map(tofrom:a)
      c = a != 2;
      if (c)
	return -1;
      #pragma omp target enter data map(to:a)
      #pragma omp target exit data map(from:a)
    }
  return a + 1;
}

int
main ()
{
  if (foo (0) != 3)
    __builtin_abort ();
  return 0;
}
