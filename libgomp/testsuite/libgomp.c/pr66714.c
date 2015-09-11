/* { dg-do "compile" } */
/* { dg-additional-options "--param ggc-min-expand=0" } */
/* { dg-additional-options "--param ggc-min-heapsize=0" } */
/* { dg-additional-options "-g" } */

/* Minimized from on target-2.c.  */

void
fn3 (int x)
{
  double b[3 * x];
  int i;
#pragma omp target
#pragma omp parallel for
  for (i = 0; i < x; i++)
    b[i] += 1;
}
