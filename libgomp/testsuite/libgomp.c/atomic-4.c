/* PR middle-end/35611 */

extern void abort (void);

int
main (void)
{
  long double d = .0L;
  int i;
  #pragma omp parallel for shared (d)
    for (i = 0; i < 1000; i++)
      #pragma omp atomic
	d += 1.0L;
  if (d != 1000.0L)
    abort ();
  return 0;
}
