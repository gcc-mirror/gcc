/* PR target/90811 */

int
main ()
{
  long long a[100], b[100];
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = i;
      b[i] = i % 10;
    }
  #pragma omp target teams distribute parallel for simd map(tofrom: a[:100], b[:100])
  for (i = 0; i < 100; i++)
    {
      long long c = 0;
      const long long d[] = { 1, 3, 5, 7, 9 };
      for (int j = 4; j >= 0; j--)
         c = d[j] + b[i] * c;
      a[i] += c;
    }
  for (i = 0; i < 100; i++)
    {
      const long long r[] = { 1, 26, 229, 976, 2849, 6646, 13381, 24284, 40801, 64594 };
      if (a[i] != r[i % 10] + (i / 10 * 10))
	__builtin_abort ();
    }
  return 0;
}
