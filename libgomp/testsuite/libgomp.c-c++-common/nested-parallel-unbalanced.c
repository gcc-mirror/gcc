/* Ensure that nested parallel regions work even when the number of loop
   iterations is not divisible by the number of threads.  */

#include <stdlib.h>

int main() {
  int A[30][40], B[30][40];
  size_t n = 30;

  for (size_t i = 0; i < 30; ++i)
    for (size_t j = 0; j < 40; ++j)
    A[i][j] = 42;

#pragma omp target map(A[0:30][0:40], B[0:30][0:40])
  {
#pragma omp parallel for num_threads(8)
    for (size_t i = 0; i < n; ++i)
      {
#pragma omp parallel for
	for (size_t j = 0; j < n; ++j)
	  {
	    B[i][j] = A[i][j];
	  }
      }
  }

for (size_t i = 0; i < n; ++i)
  for (size_t j = 0; j < n; ++j)
    if (B[i][j] != 42)
      abort ();
}
