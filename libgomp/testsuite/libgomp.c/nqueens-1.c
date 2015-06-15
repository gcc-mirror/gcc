/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */

#include <omp.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int cnt;
#pragma omp threadprivate (cnt)

void
nqueens (char *a, int n, int pos)
{
  /* b[i] = j means the queen in i-th row is in column j.  */
  char b[pos + 1];
  int i, j;
  memcpy (b, a, pos);
  for (i = 0; i < n; i++)
    {
      for (j = 0; j < pos; j++)
	if (b[j] == i || b[j] == i + pos - j || i == b[j] + pos - j)
	  break;
      if (j < pos)
	continue;
      if (pos == n - 1)
	/* Found a solution.  Could output it here.  */
	++cnt;
      else
	{
	  b[pos] = i;
	  #pragma omp task
	    nqueens (b, n, pos + 1);
	}
    }
}

int
main (int argc, char **argv)
{
  int n = 8;
  if (argc >= 2)
    n = strtoul (argv[1], NULL, 0);
  if (n < 1 || n > 127)
    {
      fprintf (stderr, "invalid count %d\n", n);
      return 1;
    }
  cnt = 0;
  double stime = omp_get_wtime ();
  nqueens ("", n, 0);
  printf ("serial   N %d solutions # %d time %f\n", n, cnt, omp_get_wtime () - stime);
  #pragma omp parallel
    cnt = 0;
  stime = omp_get_wtime ();
  int tempcnt = 0;
  #pragma omp parallel reduction (+:tempcnt)
    {
      #pragma omp single
	nqueens ("", n, 0);
      tempcnt = cnt;
    }
  cnt = tempcnt;
  printf ("parallel N %d solutions # %d time %f\n", n, cnt, omp_get_wtime () - stime);
  return 0;
}
