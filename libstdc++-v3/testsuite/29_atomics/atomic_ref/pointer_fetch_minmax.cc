// { dg-do run { target c++26 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>
#include <testsuite_hooks.h>

void test01() {
  long arr[10] = {};
  long *value;

  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<long *> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<long *>::is_always_lock_free)
      VERIFY(ok);
    a = arr;

    auto v = a.fetch_max(arr + 5);
    VERIFY(v == arr);
    VERIFY(a == arr + 5);
    v = a.fetch_max(arr + 2, mo);
    VERIFY(v == arr + 5);
    VERIFY(a == arr + 5);

    v = a.fetch_min(arr + 3);
    VERIFY(v == arr + 5);
    VERIFY(a == arr + 3);
    v = a.fetch_min(arr + 5, mo);
    VERIFY(v == arr + 3);
    VERIFY(a == arr + 3);
  }

  VERIFY(value == arr + 3);
}

void test02() {
  char arr[10] = {};
  char *value;

  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<char *> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<char *>::is_always_lock_free)
      VERIFY(ok);
    a = arr;

    auto v = a.fetch_max(arr + 5);
    VERIFY(v == arr);
    VERIFY(a == arr + 5);
    v = a.fetch_max(arr + 2, mo);
    VERIFY(v == arr + 5);
    VERIFY(a == arr + 5);

    v = a.fetch_min(arr + 3);
    VERIFY(v == arr + 5);
    VERIFY(a == arr + 3);
    v = a.fetch_min(arr + 5, mo);
    VERIFY(v == arr + 3);
    VERIFY(a == arr + 3);
  }

  VERIFY(value == arr + 3);
}

int main() {
  test01();
  test02();
}
