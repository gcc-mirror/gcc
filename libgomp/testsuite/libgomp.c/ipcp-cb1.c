/* Test that we can propagate constants into outlined OpenMP kernels.
   This tests the underlying callback attribute and its related edges.  */

/* { dg-do run } */
/* { dg-options "-O3 -fopenmp -flto -std=gnu99 -fdump-ipa-cp-details" } */
/* { dg-require-effective-target fopenmp } */
/* { dg-require-effective-target lto } */

int a[100];
void test(int c) {
#pragma omp parallel for
  for (int i = 0; i < c; i++) {
    if (!__builtin_constant_p(c)) {
      __builtin_abort();
    }
    a[i] = i;
  }
}
int main() {
  test(100);
  return a[5] - 5;
}

/* { dg-final { scan-wpa-ipa-dump "Creating a specialized node of test._omp_fn" "cp" } } */
/* { dg-final { scan-wpa-ipa-dump "Aggregate replacements: 0\\\[0]=100\\(by_ref\\)" "cp" } } */
