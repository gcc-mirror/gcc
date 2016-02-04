/* PR middle-end/68960 */
/* { dg-do run } */

int
main ()
{
  int temp[257] __attribute__ ((aligned (256))) = { 0 };
  #pragma omp parallel private (temp) num_threads (2)
  {
    int *p = &temp[0];
    asm volatile ("" : "+g" (p));
    if (((__UINTPTR_TYPE__) p) & 255)
      __builtin_abort ();
  }
  #pragma omp parallel num_threads (2)
  #pragma omp single
  #pragma omp task firstprivate (temp)
  {
    int *p = &temp[0];
    asm volatile ("" : "+g" (p));
    if (((__UINTPTR_TYPE__) p) & 255)
      __builtin_abort ();
  }
  return 0;
}
