/* Test that GOMP_task is special cased when cpyfn is NULL.  */

/* { dg-do run } */
/* { dg-options "-O3 -fopenmp -std=gnu99 -fdump-ipa-cp-details" } */
/* { dg-require-effective-target fopenmp } */

static inline void __attribute__((always_inline)) test(int c) {
  for (int i = 0; i < c; i++)
    if (!__builtin_constant_p(c))
      __builtin_abort();
}

int foo(int c) {
#pragma omp task
  test(c);
  return 0;
}

int main() {
  foo(7);
  return 0;
}

/* { dg-final { scan-ipa-dump "Creating a specialized node of foo._omp_fn" "cp" } } */
