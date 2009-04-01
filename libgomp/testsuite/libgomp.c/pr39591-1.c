/* PR other/39591 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int err;

int
main (void)
{
#pragma omp parallel
  {
    int array[40];
    int i;
    for (i = 0; i < sizeof array / sizeof array[0]; i++)
      array[i] = 0x55555555;

#pragma omp for schedule(dynamic)
    for (i = 0; i < 50; i++)
#pragma omp task shared(array)
      {
	int j;
	for (j = 0; j < sizeof array / sizeof array[0]; j++)
	  if (array[j] != 0x55555555)
#pragma omp atomic
	    err++;
      }
  }
  if (err)
    abort ();
  return 0;
}
