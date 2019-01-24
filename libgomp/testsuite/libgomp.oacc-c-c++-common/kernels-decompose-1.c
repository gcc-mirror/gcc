/* { dg-additional-options "-fopenacc-kernels=split" } */
/* { dg-additional-options "-fopt-info-optimized-omp" } */

#undef NDEBUG
#include <assert.h>

int main()
{
  int a = 0;
#define N 123
  int b[N] = { 0 };

#pragma acc kernels
  {
    int c = 234; /* { dg-warning "note: beginning .gang-single. region in OpenACC .kernels. construct" } */

#pragma acc loop independent gang /* { dg-warning "note: assigned OpenACC gang loop parallelism" } */
    /* { dg-warning "note: parallelized loop nest in OpenACC .kernels. construct" "" { target *-*-* } 17 } */
    for (int i = 0; i < N; ++i)
      b[i] = c;

    a = c; /* { dg-warning "note: beginning .gang-single. region in OpenACC .kernels. construct" } */
  }

  for (int i = 0; i < N; ++i)
    assert (b[i] == 234);
  assert (a == 234);

  return 0;
}
