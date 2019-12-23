#include <stdlib.h>

/* Test asyncronous attach and detach operation.  */

typedef struct {
  int *a;
  int *b;
} mystruct;

int
main (int argc, char* argv[])
{
  const int N = 1024;
  mystruct m;
  int i;

  m.a = (int *) malloc (N * sizeof (int));
  m.b = (int *) malloc (N * sizeof (int));

  for (i = 0; i < N; i++)
    {
      m.a[i] = 0;
      m.b[i] = 0;
    }

#pragma acc enter data copyin(m)

  for (int i = 0; i < 99; i++)
    {
      int j;
#pragma acc parallel loop copy(m.a[0:N]) async(i % 2)
      for (j = 0; j < N; j++)
	m.a[j]++;
#pragma acc parallel loop copy(m.b[0:N]) async((i + 1) % 2)
      for (j = 0; j < N; j++)
	m.b[j]++;
    }

#pragma acc exit data copyout(m) wait(0, 1)

  for (i = 0; i < N; i++)
    {
      if (m.a[i] != 99)
	abort ();
      if (m.b[i] != 99)
	abort ();
    }

  free (m.a);
  free (m.b);

  return 0;
}
