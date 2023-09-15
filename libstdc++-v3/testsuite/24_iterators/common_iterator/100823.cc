// { dg-options "-D_GLIBCXX_ASSERTIONS" }
// { dg-do run { target c++20 } }
#include <iterator>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

void
test_triviality()
{
  using I = std::common_iterator<int*, const int*>;

  // Cannot be trivial, because it has to initialize members.
  static_assert( ! std::is_trivially_default_constructible_v<I> );

  static_assert( std::is_trivially_destructible_v<I> );
  static_assert( std::is_trivially_copy_constructible_v<I> );
  static_assert( std::is_trivially_copy_assignable_v<I> );
  static_assert( std::is_trivially_move_constructible_v<I> );
  static_assert( std::is_trivially_move_assignable_v<I> );
}

void
test_valueless_assignment()
{
  int x[1] { };
  __gnu_test::test_forward_range<int> r(x);
  using Iter = decltype(r.begin());
  using Sent = decltype(r.end());

  std::common_iterator<Iter, Sent> i;
  const std::common_iterator<Iter, Sent> j(r.begin());
  try
  {
    struct Bomb
    {
      bool operator==(Iter) const { return true; }
      operator Sent() const { throw 1; }
    };
    std::common_iterator<Iter, Bomb> b{Bomb{}};
    i = b; // Throws, leaving i valueless-by-exception.
    VERIFY(false);
  }
  catch (int)
  {
    std::common_iterator<Iter, Sent> k(i);

    // PR libstdc++/100823
    k = i; // Valid even though both operands are valueless.

    i = j; // No longer valueless.
  }
  VERIFY( i == j );
}

int main()
{
  test_valueless_assignment();
}
