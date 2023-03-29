/* { dg-do run } */

int
main ()
{
  int i = 0;
  #pragma omp task
  {
    #pragma omp target nowait private (i)
    i = 1;
    #pragma omp taskwait
  }
  return 0;
}
