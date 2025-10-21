// { dg-do run { target c++11 } }
// { dg-add-options no_pch }

// Undefine these if present in runtest flags.
#undef _GLIBCXX_ASSERTIONS
#undef _GLIBCXX_DEBUG

// Prevent assertions from being automatically enabled at -O0
#define _GLIBCXX_NO_ASSERTIONS

#include <iterator>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

template<typename Container>
void
test_advance()
{
  int a[] = { 1, 2, 3 };
  Container c(a);
  auto iter = c.begin();

  // This call violates the precondition for std::advance,
  // but with assertions disabled we do not diagnose it.
  std::advance(iter, -1);

  // However we do guarantee that erroneously decrementing
  // an input iterator is a no-op and does no harm.
  VERIFY( *iter == 1 );

  ++iter;
  std::advance(iter, -999);
  VERIFY( *iter == 2 );

  std::advance(iter, 0);
  VERIFY( *iter == 2 );
  std::advance(iter, 1);
  VERIFY( *iter == 3 );
}

template<typename Container>
void
test_prev()
{
  int a[] = { 1, 2, 3 };
  Container c(a);
  auto iter = c.begin();

  // This calls std::advance(iter, -1), which violates the precondition.
  iter = std::prev(iter);

  // As above, we turn the std::prev call into a no-op.
  VERIFY( *iter == 1 );

  ++iter;
  iter = std::prev(iter, 999);
  VERIFY( *iter == 2 );

  iter = std::prev(iter, 0);
  VERIFY( *iter == 2 );
  iter = std::prev(iter, -1);
  VERIFY( *iter == 3 );
}

template<typename Container>
void
test_next()
{
  int a[] = { 1, 2, 3 };
  Container c(a);
  auto iter = c.begin();

  // This calls std::advance(iter, -1), which violates the precondition.
  iter = std::next(iter, -1);

  // As above, we turn the std::prev call into a no-op.
  VERIFY( *iter == 1 );

  ++iter;
  iter = std::next(iter, -999);
  VERIFY( *iter == 2 );

  iter = std::next(iter, 0);
  VERIFY( *iter == 2 );
  iter = std::next(iter);
  VERIFY( *iter == 3 );
}

int main()
{
  using InputContainer = __gnu_test::input_container<int>;
  test_advance<InputContainer>();
  test_prev<InputContainer>();
  test_next<InputContainer>();

  using ForwardContainer = __gnu_test::forward_container<int>;
  test_advance<ForwardContainer>();
  test_prev<ForwardContainer>();
  test_next<ForwardContainer>();
}
