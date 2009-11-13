/* PR middle-end/42029 */
/* { dg-do run } */

extern void abort (void);

int
main ()
{
  int i;
  _Complex int c = 0;

#pragma omp parallel for private(i) reduction(+:c)
  for (i = 0; i < 8; ++i)
    c += 1;

  if (c != 8)
    abort ();
  return 0;
}
