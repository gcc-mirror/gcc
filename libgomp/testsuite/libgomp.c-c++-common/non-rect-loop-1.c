/* PR middle-end/111017  */

#include <omp.h>

#define DIM 32
#define N (DIM*DIM)

int
main ()
{
  int a[N], b[N], c[N];
  int dim = DIM;

  for (int i = 0; i < N; i++)
    {
      a[i] = 3*i;
      b[i] = 7*i;
      c[i] = 42;
    }

  #pragma omp parallel for collapse(2)
  for (int i = 0; i < DIM; i++)
    for (int j = (i*DIM); j < (i*DIM + DIM); j++)
      c[j] = a[j] + b[j];

  for (int i = 0; i < DIM; i++)
    for (int j = (i*DIM); j < (i*DIM + DIM); j++)
      if (c[j] != a[j] + b[j] || c[j] != 3*j +7*j)
	__builtin_abort ();
  for (int i = 0; i < N; i++)
    c[i] = 42;

  #pragma omp parallel for collapse(2)
  for (int i = 0; i < dim; i++)
    for (int j = (i*dim); j < (i*dim + dim); j++)
      c[j] = a[j] + b[j];

  for (int i = 0; i < DIM; i++)
    for (int j = (i*DIM); j < (i*DIM + DIM); j++)
      if (c[j] != a[j] + b[j] || c[j] != 3*j +7*j)
	__builtin_abort ();
  for (int i = 0; i < N; i++)
    c[i] = 42;

  for (int dev = 0; dev <= omp_get_num_devices(); dev++)
    {
      #pragma omp target teams loop device(dev) map(to:a,b) map(from:c)
      for (int i = 0; i < DIM; i++)
	for (int j = (i*DIM); j < (i*DIM + DIM); j++)
	  c[j] = a[j] + b[j];

      for (int i = 0; i < DIM; i++)
	for (int j = (i*DIM); j < (i*DIM + DIM); j++)
	  if (c[j] != a[j] + b[j] || c[j] != 3*j +7*j)
	    __builtin_abort ();
      for (int i = 0; i < N; i++)
	c[i] = 42;

      #pragma omp target teams loop device(dev) map(to:a,b) map(from:c)
      for (int i = 0; i < dim; i++)
	for (int j = (i*dim); j < (i*dim + dim); j++)
	  c[j] = a[j] + b[j];

      for (int i = 0; i < DIM; i++)
	for (int j = (i*DIM); j < (i*DIM + DIM); j++)
	  if (c[j] != a[j] + b[j] || c[j] != 3*j +7*j)
	    __builtin_abort ();
      for (int i = 0; i < N; i++)
	c[i] = 42;
    }
  return 0;
}
