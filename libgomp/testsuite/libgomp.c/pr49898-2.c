/* PR middle-end/49898 */
/* { dg-do run } */

extern void abort (void);

int
main ()
{
  int i, j, sum = 0;
#pragma omp parallel for reduction(+:sum)
  for (i = 0; i < 10; i++)
    #pragma omp parallel for reduction(+:sum)
    for (j = 0; j < 10; j++)
      sum += j;
  if (sum != 450)
    abort ();
  return 0;
}
