// { dg-do run { target c++23 } }

#include <expected>
#include <testsuite_hooks.h>

constexpr bool
test_default()
{
  std::expected<int, int> e;
  VERIFY( e.has_value() );
  VERIFY( *e == 0 );

  std::expected<void, int> ev;
  VERIFY( ev.has_value() );
  VERIFY( (ev.value(), true) );

  return true;
}

constexpr bool
test_val()
{
  std::expected<int, int> e1(1);
  VERIFY( e1.has_value() );
  VERIFY( *e1 == 1 );

  std::expected<int, int> e2(std::in_place, 2);
  VERIFY( e2.has_value() );
  VERIFY( *e2 == 2 );

  struct X
  {
    constexpr X(std::initializer_list<int> l, void*) : n(l.size()) { }
    int n;
  };

  std::expected<X, int> e3(X{{1, 2, 3}, nullptr});
  VERIFY( e3.has_value() );
  VERIFY( e3->n == 3 );

  std::expected<X, int> e4(std::in_place, {1, 2, 3, 4}, nullptr);
  VERIFY( e4.has_value() );
  VERIFY( e4->n == 4 );

  std::expected<void, int> ev(std::in_place);
  VERIFY( ev.has_value() );
  VERIFY( (ev.value(), true) );

  return true;
}

constexpr bool
test_err()
{
  std::expected<int, int> e1(std::unexpected<int>(1));
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == 1 );

  const std::unexpected<int> u2(2);
  std::expected<int, int> e2(u2);
  VERIFY( ! e2.has_value() );
  VERIFY( e2.error() == 2 );

  std::expected<int, int> e3(std::unexpect, 3);
  VERIFY( ! e3.has_value() );
  VERIFY( e3.error() == 3 );

  struct X
  {
    constexpr X(int i, int j) : n(i+j) { }
    constexpr X(std::initializer_list<int> l, void*) : n(l.size()) { }
    int n;
  };

  std::expected<int, X> e4(std::unexpect, 1, 3);
  VERIFY( ! e4.has_value() );
  VERIFY( e4.error().n == 4 );

  std::expected<int, X> e5(std::unexpect, {1, 2, 3, 4, 5}, nullptr);
  VERIFY( ! e5.has_value() );
  VERIFY( e5.error().n == 5 );

  std::expected<const void, int> ev1(std::unexpected<int>(1));
  VERIFY( ! ev1.has_value() );
  VERIFY( ev1.error() == 1 );

  std::expected<volatile void, int> ev2(u2);
  VERIFY( ! ev2.has_value() );
  VERIFY( ev2.error() == 2 );

  std::expected<const volatile void, int> ev3(std::unexpect, 3);
  VERIFY( ! ev3.has_value() );
  VERIFY( ev3.error() == 3 );

  std::expected<void, X> ev4(std::unexpect, 1, 3);
  VERIFY( ! ev4.has_value() );
  VERIFY( ev4.error().n == 4 );

  std::expected<void, X> ev5(std::unexpect, {1, 2, 3, 4, 5}, nullptr);
  VERIFY( ! ev5.has_value() );
  VERIFY( ev5.error().n == 5 );

  return true;
}

constexpr bool
test_copy()
{
  std::expected<int, int> e1(1);
  std::expected<int, int> e2(e1);
  VERIFY( e2.value() == 1 );
  std::expected<int, int> e3(std::move(e2));
  VERIFY( e2.value() == 1 );
  VERIFY( e3.value() == 1 );
  std::expected<short, short> e4(e1);
  VERIFY( e4.value() == 1 );
  std::expected<short, short> e5(std::move(e4));
  VERIFY( e4.value() == 1 );
  VERIFY( e5.value() == 1 );

  std::expected<int, int> u1(std::unexpect, 2);
  std::expected<int, int> u2(u1);
  VERIFY( ! u2.has_value() );
  VERIFY( u2.error() == 2 );
  std::expected<int, int> u3(std::move(u2));
  VERIFY( ! u3.has_value() );
  VERIFY( u3.error() == 2 );
  std::expected<short, short> u4(u1);
  VERIFY( ! u4.has_value() );
  VERIFY( u4.error() == 2 );
  std::expected<short, short> u5(std::move(u4));
  VERIFY( ! u5.has_value() );
  VERIFY( u5.error() == 2 );

  std::expected<void, int> ev1;
  std::expected<void, int> ev2(ev1);
  VERIFY( ev2.has_value() );
  std::expected<void, int> ev3(std::move(ev2));
  VERIFY( ev2.has_value() );
  VERIFY( ev3.has_value() );
  std::expected<volatile void, short> ev4(ev1);
  VERIFY( ev4.has_value() );
  std::expected<const void, short> ev5(std::move(ev4));
  VERIFY( ev4.has_value() );
  VERIFY( ev5.has_value() );

  std::expected<void, int> uv1(std::unexpect, 2);
  std::expected<void, int> uv2(uv1);
  VERIFY( ! uv2.has_value() );
  VERIFY( uv2.error() == 2 );
  std::expected<void, int> uv3(std::move(uv2));
  VERIFY( ! uv3.has_value() );
  VERIFY( uv3.error() == 2 );
  std::expected<const void, short> uv4(uv1);
  VERIFY( ! uv4.has_value() );
  VERIFY( uv4.error() == 2 );
  std::expected<volatile void, short> uv5(std::move(uv4));
  VERIFY( ! uv5.has_value() );
  VERIFY( uv5.error() == 2 );

  return true;
}

constexpr bool
test_pr105153()
{
  struct E {
    E(int&&) = delete;
    E(const int&);
  };

  std::expected<void, E> e(std::expected<void, int>{});

  static_assert( ! std::is_constructible_v<std::expected<void, int>,
					   std::expected<int, int>> );

  return true;
}

int main()
{
  test_default();
  static_assert( test_default() );
  test_val();
  static_assert( test_val() );
  test_err();
  static_assert( test_err() );
  test_copy();
  static_assert( test_copy() );
  test_pr105153();
  static_assert( test_pr105153() );
}
