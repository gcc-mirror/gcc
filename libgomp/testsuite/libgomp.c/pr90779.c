/* PR middle-end/90779 */

extern void abort (void);

int
main ()
{
  int i, j;
  for (i = 0; i < 2; ++i)
    #pragma omp target map(from: j)
    {
      static int k = 5;
      j = ++k;
    }
  if (j != 7)
    abort ();
  return 0;
}
