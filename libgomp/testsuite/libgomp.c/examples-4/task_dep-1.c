/* { dg-do run } */

#include <stdlib.h>
int main()
{
   int x = 1;
   #pragma omp parallel
   #pragma omp single
   {
      #pragma omp task shared(x) depend(out: x)
         x = 2;
      #pragma omp task shared(x) depend(in: x)
        if (x != 2)
          abort ();
   }
   return 0;
}
