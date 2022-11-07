#include <stdlib.h>

/* Test multiple struct dereferences on one directive, and slices starting at
   non-zero.  */

typedef struct {
  int *a;
  int *b;
  int *c;
} mystruct;

int main(int argc, char* argv[])
{
  const int N = 1024;
  mystruct *m = (mystruct *) malloc (sizeof (*m));
  int i;

  m->a = (int *) malloc (N * sizeof (int));
  m->b = (int *) malloc (N * sizeof (int));
  m->c = (int *) malloc (N * sizeof (int));

  for (i = 0; i < N; i++)
    {
      m->a[i] = 0;
      m->b[i] = 0;
      m->c[i] = 0;
    }

  for (int i = 0; i < 99; i++)
    {
      int j;
#pragma acc parallel loop copy(m->a[0:N])
      for (j = 0; j < N; j++)
	m->a[j]++;
#pragma acc parallel loop copy(m->b[0:N], m->c[5:N-10])
      for (j = 0; j < N; j++)
	{
	  m->b[j]++;
	  if (j > 5 && j < N - 5)
	    m->c[j]++;
	}
    }

  for (i = 0; i < N; i++)
    {
      if (m->a[i] != 99)
	abort ();
      if (m->b[i] != 99)
	abort ();
      if (i > 5 && i < N-5)
	{
	  if (m->c[i] != 99)
	    abort ();
	}
      else
	{
	  if (m->c[i] != 0)
	    abort ();
	}
    }

  free (m->a);
  free (m->b);
  free (m->c);
  free (m);

  return 0;
}
