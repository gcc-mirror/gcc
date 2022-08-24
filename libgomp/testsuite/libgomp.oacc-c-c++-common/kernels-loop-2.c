#include <stdlib.h>

#define N (1024 * 512)
#define COUNTERTYPE unsigned int

int
main (void)
{
  unsigned int *__restrict a;
  unsigned int *__restrict b;
  unsigned int *__restrict c;

  a = (unsigned int *__restrict)malloc (N * sizeof (unsigned int));
  b = (unsigned int *__restrict)malloc (N * sizeof (unsigned int));
  c = (unsigned int *__restrict)malloc (N * sizeof (unsigned int));

  /* Parallelism dimensions: compiler/runtime decides.  */
#pragma acc kernels copyout (a[0:N])
  {
    for (COUNTERTYPE i = 0; i < N; i++)
      a[i] = i * 2;
  }

  /* Parallelism dimensions: variable.  */
#pragma acc kernels copyout (b[0:N]) \
  num_gangs (3 + a[3]) num_workers (5 + a[5]) vector_length (7 + a[7])
  /* { dg-prune-output "using .vector_length \\(32\\)., ignoring runtime setting" } */
  {
    for (COUNTERTYPE i = 0; i < N; i++)
      b[i] = i * 4;
  }

  /* Parallelism dimensions: literal.  */
#pragma acc kernels copyin (a[0:N], b[0:N]) copyout (c[0:N]) \
  num_gangs (3) num_workers (5) vector_length (7)
  /* { dg-prune-output "using .vector_length \\(32\\)., ignoring 7" } */
  {
    for (COUNTERTYPE ii = 0; ii < N; ii++)
      c[ii] = a[ii] + b[ii];
  }

  for (COUNTERTYPE i = 0; i < N; i++)
    {
      if (a[i] != i * 2)
	abort ();
      if (b[i] != i * 4)
	abort ();
      if (c[i] != a[i] + b[i])
	abort ();
    }

  free (a);
  free (b);
  free (c);

  return 0;
}
