/* PR c/43893 */
/* { dg-do run } */

extern void abort (void);

int
main ()
{
  int c;
  unsigned int i;
  int j;
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 0; i < 1; i++)
    c++;
  if (c != 1)
    abort ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 0; i <= 0; i++)
    c++;
  if (c != 1)
    abort ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = - __INT_MAX__ - 1; j < - __INT_MAX__; j++)
    c++;
  if (c != 1)
    abort ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = - __INT_MAX__ - 1; j <= - __INT_MAX__ - 1; j++)
    c++;
  if (c != 1)
    abort ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 2U * __INT_MAX__ + 1; i > 2U * __INT_MAX__; i--)
    c++;
  if (c != 1)
    abort ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (i = 2U * __INT_MAX__ + 1; i >= 2U * __INT_MAX__ + 1; i--)
    c++;
  if (c != 1)
    abort ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = __INT_MAX__; j > __INT_MAX__ - 1; j--)
    c++;
  if (c != 1)
    abort ();
  c = 0;
#pragma omp parallel for reduction(+:c)
  for (j = __INT_MAX__; j >= __INT_MAX__; j--)
    c++;
  if (c != 1)
    abort ();
  return 0;
}
