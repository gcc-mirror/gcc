/* PR middle-end/100471 */

extern void abort (void);

int c;

int
main ()
{
#pragma omp parallel
#pragma omp single
  {
    int r = 0, i;
    #pragma omp taskloop reduction(+:r)
    for (i = 0; i < c; i++)
      r++;
    if (r != 0)
      abort ();
  }
  return 0;
}
