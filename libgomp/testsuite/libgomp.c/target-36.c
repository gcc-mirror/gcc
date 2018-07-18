int
main ()
{
  int ah, bh, n = 1024;
#pragma omp target map(from: ah, bh)
  {
    int a, b;
#pragma omp simd lastprivate(b)
    for (a = 0; a < n; a++)
      {
	b = a + n + 1;
	asm volatile ("" : "+r"(b));
      }
    ah = a, bh = b;
  }
  if (ah != n || bh != 2 * n)
    __builtin_abort ();
}
