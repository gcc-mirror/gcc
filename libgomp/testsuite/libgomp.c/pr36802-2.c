/* PR middle-end/36802 */

extern void abort (void);
extern int omp_set_dynamic (int);
extern void omp_set_nested (int);
extern int omp_get_num_threads (void);

int q;

int
foo (int k)
{
  int i = 6, n = 0;
  omp_set_dynamic (0);
  omp_set_nested (1);
#pragma omp parallel shared (i) num_threads (3)
  {
    int l;

    if (omp_get_num_threads () != 3)
    #pragma omp atomic
      n += 1;
    else
    #pragma omp for
      for (l = 0; l < 3; l++)
	if (k)
	#pragma omp atomic
	  q += i;
	else
	#pragma omp parallel shared (i) num_threads (4)
	  {
	    if (omp_get_num_threads () != 4)
	    #pragma omp atomic
	      n += 1;
	    #pragma omp critical
	      i += 1;
	  }
  }
  if (n == 0 && i != 6 + 3 * 4)
    abort ();
  return 0;
}

int
main (void)
{
  foo (0);
  return 0;
}
