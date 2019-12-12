/* { dg-do run } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* Exercise the kernel launch argument mapping.  */

int
main (int argc, char **argv)
{
  int a[256], b[256], c[256], d[256], e[256], f[256];
  int i;
  int n;

  /* 48 is the size of the mappings for the first parallel construct.  */
  n = sysconf (_SC_PAGESIZE) / 48 - 1;

  i = 0;

  for (i = 0; i < n; i++)
    {
      #pragma acc parallel copy (a, b, c, d)
	{
	  int j;

	  for (j = 0; j < 256; j++)
	    {
	      a[j] = j;
	      b[j] = j;
	      c[j] = j;
	      d[j] = j;
	    }
	}
    }

#pragma acc parallel copy (a, b, c, d, e, f)
  {
    int j;

    for (j = 0; j < 256; j++)
      {
	a[j] = j;
	b[j] = j;
	c[j] = j;
	d[j] = j;
	e[j] = j;
	f[j] = j;
      }
  }

  for (i = 0; i < 256; i++)
   {
     if (a[i] != i) abort();
     if (b[i] != i) abort();
     if (c[i] != i) abort();
     if (d[i] != i) abort();
     if (e[i] != i) abort();
     if (f[i] != i) abort();
   }

  exit (0);
}
