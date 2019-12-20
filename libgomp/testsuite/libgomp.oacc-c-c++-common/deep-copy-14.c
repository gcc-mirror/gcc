#include <openacc.h>
#include <stdlib.h>

/* Test attach/detach operation with chained dereferences.  */

typedef struct mystruct {
  int *a;
  struct mystruct *next;
} mystruct;

int
main (int argc, char* argv[])
{
  const int N = 1024;
  mystruct *m = (mystruct *) malloc (sizeof (*m));
  int i;

  m->a = (int *) malloc (N * sizeof (int));
  m->next = (mystruct *) malloc (sizeof (*m));
  m->next->a = (int *) malloc (N * sizeof (int));
  m->next->next = NULL;

  for (i = 0; i < N; i++)
    {
      m->a[i] = 0;
      m->next->a[i] = 0;
    }

#pragma acc enter data copyin(m[0:1])
  acc_copyin (m->next, sizeof (*m));

  for (int i = 0; i < 99; i++)
    {
      int j;
      acc_copyin (m->next->a, N * sizeof (int));
      acc_attach ((void **) &m->next);
      /* This will attach only the innermost pointer, i.e. "a[0:N]".  That's
	 why we have to attach the "m->next" pointer manually above.  */
#pragma acc parallel loop copy(m->next->a[0:N])
      for (j = 0; j < N; j++)
	m->next->a[j]++;
      acc_detach ((void **) &m->next);
      acc_copyout (m->next->a, N * sizeof (int));
    }

  acc_copyout (m->next, sizeof (*m));
#pragma acc exit data copyout(m[0:1])

  for (i = 0; i < N; i++)
    {
      if (m->a[i] != 0)
	abort ();
      if (m->next->a[i] != 99)
	abort ();
    }

  free (m->next->a);
  free (m->next);
  free (m->a);
  free (m);

  return 0;
}
