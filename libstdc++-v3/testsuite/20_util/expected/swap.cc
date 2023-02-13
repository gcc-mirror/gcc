// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <expected>
#include <testsuite_hooks.h>

struct NonTrivial
{
  constexpr NonTrivial(int i) : i(i) { }
  constexpr NonTrivial(const NonTrivial& x) noexcept(false): i(x.i) { }
  constexpr ~NonTrivial() { }
  int i;

  constexpr bool operator==(const NonTrivial&) const = default;
};

constexpr bool
test_swap_obj()
{
  std::expected<int, int> e1(1), e2(2);
  std::expected<int, int> e3(std::unexpect, 3), e4(std::unexpect, 4);

  swap(e1, e2);
  VERIFY( e1.value() == 2 );
  VERIFY( e2.value() == 1 );
  swap(e1, e3);
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == 3 );
  VERIFY( e3.value() == 2 );
  swap(e1, e3);
  VERIFY( ! e3.has_value() );
  VERIFY( e1.value() == 2 );
  VERIFY( e3.error() == 3 );
  swap(e3, e4);
  VERIFY( ! e3.has_value() );
  VERIFY( ! e4.has_value() );
  VERIFY( e3.error() == 4 );
  VERIFY( e4.error() == 3 );

  std::expected<int, NonTrivial> e5(1), e6(2);
  std::expected<int, NonTrivial> e7(std::unexpect, 3), e8(std::unexpect, 4);

  swap(e5, e6);
  VERIFY( e5.value() == 2 );
  VERIFY( e6.value() == 1 );
  swap(e5, e7);
  VERIFY( ! e5.has_value() );
  VERIFY( e5.error() == 3 );
  VERIFY( e7.value() == 2 );
  swap(e5, e7);
  VERIFY( ! e7.has_value() );
  VERIFY( e5.value() == 2 );
  VERIFY( e7.error() == 3 );
  swap(e7, e8);
  VERIFY( ! e7.has_value() );
  VERIFY( ! e8.has_value() );
  VERIFY( e7.error() == 4 );
  VERIFY( e8.error() == 3 );

  std::expected<NonTrivial, int> e9(1), e10(2);
  std::expected<NonTrivial, int> e11(std::unexpect, 3), e12(std::unexpect, 4);

  swap(e9, e10);
  VERIFY( e9.value() == 2 );
  VERIFY( e10.value() == 1 );
  swap(e9, e11);
  VERIFY( ! e9.has_value() );
  VERIFY( e9.error() == 3 );
  VERIFY( e11.value() == 2 );
  swap(e9, e11);
  VERIFY( ! e11.has_value() );
  VERIFY( e9.value() == 2 );
  VERIFY( e11.error() == 3 );
  swap(e11, e12);
  VERIFY( ! e11.has_value() );
  VERIFY( ! e12.has_value() );
  VERIFY( e11.error() == 4 );
  VERIFY( e12.error() == 3 );

  return true;
}

constexpr bool
test_swap_void()
{
  std::expected<void, int> v1, v2;
  std::expected<void, int> v3(std::unexpect, 3), v4(std::unexpect, 4);

  swap(v1, v2);
  VERIFY( v1.has_value() );
  VERIFY( v2.has_value() );
  swap(v1, v3);
  VERIFY( ! v1.has_value() );
  VERIFY( v1.error() == 3 );
  VERIFY( v3.has_value() );
  swap(v1, v3);
  VERIFY( ! v3.has_value() );
  VERIFY( v1.has_value() );
  VERIFY( v3.error() == 3 );
  swap(v3, v4);
  VERIFY( ! v3.has_value() );
  VERIFY( ! v4.has_value() );
  VERIFY( v3.error() == 4 );
  VERIFY( v4.error() == 3 );

  std::expected<void, NonTrivial> v5, v6;
  std::expected<void, NonTrivial> v7(std::unexpect, 3), v8(std::unexpect, 4);

  swap(v5, v6);
  VERIFY( v5.has_value() );
  VERIFY( v6.has_value() );
  swap(v5, v7);
  VERIFY( ! v5.has_value() );
  VERIFY( v5.error() == 3 );
  VERIFY( v7.has_value() );
  swap(v5, v7);
  VERIFY( ! v7.has_value() );
  VERIFY( v5.has_value() );
  VERIFY( v7.error() == 3 );
  swap(v7, v8);
  VERIFY( ! v7.has_value() );
  VERIFY( ! v8.has_value() );
  VERIFY( v7.error() == 4 );
  VERIFY( v8.error() == 3 );

  return true;
}

static_assert( std::is_swappable_v<std::expected<int, int>> );
static_assert( std::is_swappable_v<std::expected<void, int>> );

struct A { A& operator=(A&&) = delete; };
static_assert( ! std::is_swappable_v<std::expected<A, int>> );
static_assert( ! std::is_swappable_v<std::expected<int, A>> );
static_assert( ! std::is_swappable_v<std::expected<void, A>> );

int main()
{
  static_assert( test_swap_obj() );
  test_swap_obj();
  static_assert( test_swap_void() );
  test_swap_void();
}
