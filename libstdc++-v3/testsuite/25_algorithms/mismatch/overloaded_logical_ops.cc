// The predicate's result type does not model boolean-testable (it has an
// ADL-reachable operator&& and operator!), so this is undefined behaviour.
// libstdc++ supports it as a QoI extension: std::mismatch does not evaluate
// the predicate on, or dereference, the past-the-end iterator.

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;

int truth = 0;

struct Logic
{
  Logic operator!() const { return Logic(); }
  operator bool() const { return truth != 0; }
};

struct Value { };

bool operator&&(bool, Logic) { return false; }

struct Eq
{
  Logic operator()(Value, Value) const { return Logic(); }
};

void
test01()
{
  Value arr[1] = { };

  test_container<Value, forward_iterator_wrapper> empty(arr, arr);
  test_container<Value, forward_iterator_wrapper> other(arr, arr + 1);
  forward_iterator_wrapper<Value> r
    = std::mismatch(empty.begin(), empty.end(), other.begin(), Eq()).first;
  VERIFY( r.ptr == arr );

#ifdef __cpp_lib_robust_nonmodifying_seq_ops  // C++ >= 14
  test_container<Value, forward_iterator_wrapper> e2(arr, arr);
  test_container<Value, forward_iterator_wrapper> o2(arr, arr + 1);
  auto r2 = std::mismatch(e2.begin(), e2.end(), o2.begin(), o2.end(), Eq());
  VERIFY( r2.first.ptr == arr );
#endif
}

int
main()
{
  test01();
  return 0;
}
