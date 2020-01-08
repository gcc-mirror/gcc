#include <stdlib.h>

/* Test attach/detach with dereferences of reference to pointer to struct.  */

typedef struct {
  int *a;
  int *b;
  int *c;
} mystruct;

int main(int argc, char* argv[])
{
  const int N = 1024;
  mystruct *m = (mystruct *) malloc (sizeof (*m));
  mystruct *&mref = m;
  int i;

  mref->a = (int *) malloc (N * sizeof (int));
  m->b = (int *) malloc (N * sizeof (int));
  m->c = (int *) malloc (N * sizeof (int));

  for (i = 0; i < N; i++)
    {
      mref->a[i] = 0;
      m->b[i] = 0;
      m->c[i] = 0;
    }

#pragma acc enter data copyin(m[0:1])

  for (int i = 0; i < 99; i++)
    {
      int j;
#pragma acc parallel loop copy(mref->a[0:N])
      for (j = 0; j < N; j++)
	mref->a[j]++;
#pragma acc parallel loop copy(mref->b[0:N], m->c[5:N-10])
      for (j = 0; j < N; j++)
	{
	  mref->b[j]++;
	  if (j > 5 && j < N - 5)
	    m->c[j]++;
	}
    }

#pragma acc exit data copyout(m[0:1])

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
