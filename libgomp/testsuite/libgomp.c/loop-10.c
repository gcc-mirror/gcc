extern void abort (void);

int i = 8;

int main (void)
{
  int j = 7, k = 0;
  #pragma omp for
  for (i = 0; i < 10; i++)
    ;
  #pragma omp for
  for (j = 0; j < 10; j++)
    ;
  /* OpenMP 3.0 newly guarantees that the original list items can't
     be shared with the privatized omp for iterators, even when
     the original list items are already private.  */
  if (i != 8 || j != 7)
    abort ();
  #pragma omp parallel private (i) reduction (+:k)
  {
    i = 6;
    #pragma omp for
    for (i = 0; i < 10; i++)
      ;
    k = (i != 6);
  }
  if (k)
    abort ();
  return 0;
}
