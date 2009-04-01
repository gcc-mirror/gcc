/* PR other/39591 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int err, a[40];

void __attribute__((noinline))
foo (int *array)
{
#pragma omp task
  {
    int j;
    for (j = 0; j < sizeof array / sizeof array[0]; j++)
      if (array[j] != 0x55555555)
#pragma omp atomic
	err++;
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
  if (err)
    abort ();
  return 0;
}
