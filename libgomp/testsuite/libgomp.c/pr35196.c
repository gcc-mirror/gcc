/* PR middle-end/35196 */
/* { dg-do run } */

extern void abort (void);
extern void omp_set_dynamic (int);

int
main (void)
{
  int i, j;
  omp_set_dynamic (0);
#pragma omp parallel for lastprivate (i, j) num_threads (8) schedule (static)
  for (i = 0; i < 5; i++)
    j = i;
  if (i != 5 || j != 4)
    abort ();
#pragma omp parallel for lastprivate (i, j) num_threads (8) schedule (static, 2)
  for (i = 0; i < 5; i++)
    j = i;
  if (i != 5 || j != 4)
    abort ();
#pragma omp parallel for lastprivate (i, j) num_threads (8) schedule (dynamic)
  for (i = 0; i < 5; i++)
    j = i;
  if (i != 5 || j != 4)
    abort ();
#pragma omp parallel for lastprivate (i, j) num_threads (8) schedule (static)
  for (i = -12; i < 21; i += 3)
    j = i;
  if (i != 21 || j != 18)
    abort ();
#pragma omp parallel for lastprivate (i, j) num_threads (8) schedule (static, 2)
  for (i = -12; i < 21; i += 3)
    j = i;
  if (i != 21 || j != 18)
    abort ();
#pragma omp parallel for lastprivate (i, j) num_threads (8) schedule (dynamic, 3)
  for (i = -12; i < 21; i += 3)
    j = i;
  if (i != 21 || j != 18)
    abort ();
  return 0;
}
