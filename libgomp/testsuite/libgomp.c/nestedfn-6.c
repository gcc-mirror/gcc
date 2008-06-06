extern void abort (void);

int j;

int
main (void)
{
  int i;
  void nested (void) { i = 0; }
#pragma omp parallel for lastprivate (i)
  for (i = 0; i < 50; i += 3)
    ;
  if (i != 51)
    abort ();
#pragma omp parallel for lastprivate (j)
  for (j = -50; j < 70; j += 7)
    ;
  if (j != 76)
    abort ();
  return 0;
}
