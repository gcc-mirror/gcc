/* PR libgomp/104385 */

#include <unistd.h>

int
main ()
{
  int j = 0;
  #pragma omp parallel shared(j) num_threads(2)
  {
    #pragma omp barrier
    #pragma omp master
    #pragma omp task shared(j)
    {
      #pragma omp task depend(out: j) shared(j)
      {
        usleep (10000);
        j = 1;
      }

      #pragma omp task depend(inout: j) shared(j)
      j += 1;
    }
  }
  return j - 2;
}
