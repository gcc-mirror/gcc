// { dg-do run { target c++26 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>
#include <testsuite_hooks.h>

void test01() {
  long arr[10] = {};

  const auto mo = std::memory_order_relaxed;
  std::atomic<long *> a(arr);

  auto v = atomic_fetch_max(&a, arr + 5);
  VERIFY(v == arr);
  VERIFY(a == arr + 5);
  v = atomic_fetch_max_explicit(&a, arr + 2, mo);
  VERIFY(v == arr + 5);
  VERIFY(a == arr + 5);

  v = atomic_fetch_min(&a, arr + 3);
  VERIFY(v == arr + 5);
  VERIFY(a == arr + 3);
  v = atomic_fetch_min_explicit(&a, arr + 5, mo);
  VERIFY(v == arr + 3);
  VERIFY(a == arr + 3);
}

void test02() {
  char arr[10] = {};

  const auto mo = std::memory_order_relaxed;
  std::atomic<char *> a(arr);

  auto v = atomic_fetch_max(&a, arr + 5);
  VERIFY(v == arr);
  VERIFY(a == arr + 5);
  v = atomic_fetch_max_explicit(&a, arr + 2, mo);
  VERIFY(v == arr + 5);
  VERIFY(a == arr + 5);

  v = atomic_fetch_min(&a, arr + 3);
  VERIFY(v == arr + 5);
  VERIFY(a == arr + 3);
  v = atomic_fetch_min_explicit(&a, arr + 5, mo);
  VERIFY(v == arr + 3);
  VERIFY(a == arr + 3);
}

int main() {
  test01();
  test02();
}
