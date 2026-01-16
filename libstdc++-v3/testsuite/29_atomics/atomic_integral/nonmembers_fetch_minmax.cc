// { dg-do compile { target c++26 } }
// { dg-require-atomic-builtins "" }

#include <atomic>

void
test01()
{
  volatile std::atomic<int> v;
  std::atomic<long> a;
  const std::memory_order mo = std::memory_order_seq_cst;
  int i = 0;
  long l = 0;

  auto r1 = atomic_fetch_min(&v, i);
  static_assert( std::is_same<decltype(r1), int>::value, "" );
  auto r2 = atomic_fetch_min(&a, l);
  static_assert( std::is_same<decltype(r2), long>::value, "" );
  auto r3 = atomic_fetch_min_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r3), int>::value, "" );
  auto r4 = atomic_fetch_min_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r4), long>::value, "" );

  auto r5 = atomic_fetch_max(&v, i);
  static_assert( std::is_same<decltype(r5), int>::value, "" );
  auto r6 = atomic_fetch_max(&a, l);
  static_assert( std::is_same<decltype(r6), long>::value, "" );
  auto r7 = atomic_fetch_max_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r7), int>::value, "" );
  auto r8 = atomic_fetch_max_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r8), long>::value, "" );
}

void
test02()
{
  volatile std::atomic<long> v;
  std::atomic<long> a;
  std::memory_order mo = std::memory_order_seq_cst;
  const int i = 0;

  atomic_fetch_min(&v, i);
  atomic_fetch_min(&a, i);
  atomic_fetch_min_explicit(&v, i, mo);
  atomic_fetch_min_explicit(&a, i, mo);
  atomic_fetch_max(&v, i);
  atomic_fetch_max(&a, i);
  atomic_fetch_max_explicit(&v, i, mo);
  atomic_fetch_max_explicit(&a, i, mo);
}
