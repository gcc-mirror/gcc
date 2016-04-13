#include <stdlib.h>

#define N (1024 * 512)
#define COUNTERTYPE unsigned int

int
main (void)
{
  unsigned int i;

  unsigned int *__restrict c;

  c = (unsigned int *__restrict)malloc (N * sizeof (unsigned int));

  for (COUNTERTYPE i = 0; i < N; i++)
    c[i] = i * 2;

#pragma acc kernels copy (c[0:N])
  {
    for (COUNTERTYPE ii = 0; ii < N; ii++)
      c[ii] = c[ii] + ii + 1;
  }

  for (COUNTERTYPE i = 0; i < N; i++)
    if (c[i] != i * 2 + i + 1)
      abort ();

  free (c);

  return 0;
}
