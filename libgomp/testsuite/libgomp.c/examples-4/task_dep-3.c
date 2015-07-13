/* { dg-do run } */

#include <stdlib.h>

int main()
{
   int x = 0;
   #pragma omp parallel
   #pragma omp single
   {
      #pragma omp task shared(x) depend(out: x)
        x = 1;
      #pragma omp task shared(x) depend(out: x)
        x = 2;
      #pragma omp taskwait
        if (x != 1 && x != 2)
          abort ();
   }
   return 0;
}
