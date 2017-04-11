/* PR libgomp/80394 */

int
main ()
{
  int x = 0;
  #pragma omp parallel shared(x)
  #pragma omp single
  {
    #pragma omp task depend(inout: x)
    {
      for (int i = 0; i < 100000; i++)
        asm volatile ("" : : : "memory");
      x += 5;
    }
    #pragma omp task if (0) depend(inout: x)
    ;
    if (x != 5)
      __builtin_abort ();
  }
  return 0;
}
