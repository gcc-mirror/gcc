/* PR other/39591 */
/* { dg-do run } */

extern void abort (void);

int e, a[40];

void __attribute__((noinline))
foo (int *array)
{
#pragma omp task
  {
    int j;
    for (j = 0; j < 40; j++)
      if (array[j] != 0x55555555)
#pragma omp atomic
	e++;
  }
}

int
main (void)
{
  int k;
  for (k = 0; k < sizeof a / sizeof a[0]; k++)
    a[k] = 0x55555555;

#pragma omp parallel
  {
    int i;

#pragma omp for schedule (dynamic)
    for (i = 0; i < 50; i++)
      foo (a);
  }
  if (e)
    abort ();
  return 0;
}
