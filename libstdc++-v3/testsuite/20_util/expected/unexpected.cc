// { dg-do run { target c++23 } }

#include <expected>
#include <testsuite_hooks.h>

static_assert( sizeof(std::unexpected<char>) == 1 );

constexpr bool
test()
{
  std::unexpected<int> u1(1);
  VERIFY( u1.error() == 1 );

  std::unexpected<int> u2(std::in_place, 2);
  VERIFY( u2.error() == 2 );

  struct X
  {
    constexpr X(int i, int j) : n(i+j) { }
    constexpr X(std::initializer_list<int> l, void*) : n(l.size()) { }

    constexpr X(const X&) = default;
    constexpr X(X&& x) :n(x.n) { x.n = -1; }

    constexpr X& operator=(const X&) = default;
    constexpr X& operator=(X&& x) { n = x.n; x.n = -1; return *this; }

    constexpr bool operator==(const X&) const = default;
    constexpr bool operator==(int i) const { return n == i; }

    int n;
  };

  std::unexpected<X> u3(std::in_place, 2, 1);
  VERIFY( u3.error() == 3 );

  std::unexpected<X> u4(std::in_place, {1,2,3,4}, nullptr);
  VERIFY( u4.error() == 4 );

  std::unexpected<X> u5(u4);
  VERIFY( u5.error() == 4 );
  VERIFY( u4.error() == 4 );

  std::unexpected<X> u6(std::move(u4));
  VERIFY( u6.error() == 4 );
  VERIFY( u4.error() == -1 );

  u6 = u3;
  VERIFY( u6.error() == 3 );
  VERIFY( u3.error() == 3 );

  u5 = std::move(u3);
  VERIFY( u5.error() == 3 );
  VERIFY( u3.error() == -1 );

  u5.swap(u3);
  VERIFY( u3.error() == 3 );
  VERIFY( u5.error() == -1 );

  swap(u5, u3);
  VERIFY( u5.error() == 3 );
  VERIFY( u3.error() == -1 );

  VERIFY( u1 == u1 );
  VERIFY( u1 != u2 );
  VERIFY( u3 == u4 );

  // CTAD
  std::unexpected u7(1L);
  static_assert( std::is_same_v<decltype(u7), std::unexpected<long>> );

  return true;
}

static_assert( std::is_swappable_v<std::unexpected<int>> );
struct A { A& operator=(A&&) = delete; };
static_assert( ! std::is_swappable_v<std::unexpected<A>> );

int main()
{
  static_assert( test() );
  test();
}
