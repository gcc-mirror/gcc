// The predicate's result type does not model boolean-testable (it has an
// ADL-reachable operator&& and operator!), so this is undefined behaviour.
// libstdc++ supports it as a QoI extension: std::find_if does not evaluate
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

struct Value
{
  Logic operator>(Value) const { return Logic(); }
};

bool operator&&(bool, Logic) { return false; }

struct Pred
{
  Logic operator()(Value v) const { return v > v; }
};

void
test01()
{
  Value arr[1] = { };

  test_container<Value, forward_iterator_wrapper> empty(arr, arr);
  VERIFY( std::find_if(empty.begin(), empty.end(), Pred()).ptr == arr );

  test_container<Value, forward_iterator_wrapper> con(arr, arr + 1);
  VERIFY( std::find_if(con.begin(), con.end(), Pred()).ptr == arr + 1 );
}

int
main()
{
  test01();
  return 0;
}
