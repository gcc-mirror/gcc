/* Check that GOMP_task doesn't produce callback edges when cpyfn is not
   NULL.  */

/* { dg-do run } */
/* { dg-options "-O3 -fopenmp -flto -std=gnu99 -fdump-ipa-cp-details" } */
/* { dg-require-effective-target fopenmp } */
/* { dg-require-effective-target lto } */

void test(int *a) {
  for (int i = 0; i < 100; i++) {
    a[i] = i;
  }
}
int main() {
  int a[100];
  __builtin_memset (a, 0, sizeof (a));
  #pragma omp task
  test (a);
}

/* { dg-final { scan-ipa-dump-not "Created callback edge" "cp" } } */
