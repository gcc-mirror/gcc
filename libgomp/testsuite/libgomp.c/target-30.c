extern void abort (void);

#pragma omp declare target
int v = 6;
#pragma omp end declare target

int
main ()
{
  #pragma omp target /* predetermined map(tofrom: v) */
  v++;
  #pragma omp target update from (v)
  if (v != 7)
    abort ();
  #pragma omp parallel private (v) num_threads (1)
  {
    #pragma omp target /* predetermined firstprivate(v) */
    v++;
  }
  #pragma omp target update from (v)
  if (v != 7)
    abort ();
  return 0;
}
