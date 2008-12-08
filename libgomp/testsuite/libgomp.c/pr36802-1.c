/* PR middle-end/36802 */

extern void abort (void);

int
foo (int k)
{
  int i = 0;
#pragma omp parallel
  #pragma omp single
    {
      if (!k)
	{
	  int j;
	  for (j = 0; j < 10; j++)
	  #pragma omp task
	    if (j == 4)  
	      i++;
	}
      else
	i++;
    }
  return i;
}

int
main (void)
{
  if (foo (0) != 1)
    abort ();
  if (foo (1) != 1)
    abort ();
  return 0;
}
