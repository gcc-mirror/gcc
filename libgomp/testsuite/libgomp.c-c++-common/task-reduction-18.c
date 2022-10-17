/* { dg-do run } */

int a;

int
main ()
{
  #pragma omp task
  {
    #pragma omp taskgroup task_reduction (+: a)
    {
      #pragma omp task in_reduction (+: a)
      ++a;
    }
  }
  return 0;
}
