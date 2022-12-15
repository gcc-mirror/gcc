// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <expected>
#include <expected>
#include <testsuite_hooks.h>

struct X
{
  constexpr int f() & { return 1; }
  constexpr int f() const & { return 2; }
  constexpr int f() && { return 3; }
  constexpr int f() const && { return 4; }
};

constexpr bool
test_arrow()
{
  std::expected<X, int> e1;
  VERIFY( e1->f() == 1 );
  const auto& e2 = e1;
  VERIFY( e2->f() == 2 );

  return true;
}

constexpr bool
test_star()
{
  std::expected<X, int> e1;
  VERIFY( (*e1).f() == 1 );
  VERIFY( std::move(*e1).f() == 3 );
  const auto& e2 = e1;
  VERIFY( (*e2).f() == 2 );
  VERIFY( std::move(*e2).f() == 4 );

  std::expected<void, int> v;
  *v;

  return true;
}

constexpr bool
test_has_value()
{
  std::expected<int, int> e;
  VERIFY( e.has_value() );
  VERIFY( e );
  e = std::unexpected(1);
  VERIFY( ! e.has_value() );
  VERIFY( ! e );

  std::expected<void, int> v;
  VERIFY( v.has_value() );
  VERIFY( v );
  v = std::unexpected(1);
  VERIFY( ! v.has_value() );
  VERIFY( ! v );

  return true;
}

constexpr bool
test_value()
{
  std::expected<X, int> e1;

  VERIFY( e1.value().f() == 1 );
  VERIFY( std::move(e1).value().f() == 3 );
  const auto& e2 = e1;
  VERIFY( e2.value().f() == 2 );
  VERIFY( std::move(e2).value().f() == 4 );

  std::expected<void, int> v1;
  v1.value();
  std::move(v1).value();

  return true;
}

void
test_value_throw()
{
  std::expected<int, int> e1 = std::unexpected(9);

  try {
    e1.value();
    VERIFY( false );
  } catch (const std::bad_expected_access<int>& e) {
    VERIFY( e.error() == 9 );
  }
  try {
    std::move(e1).value();
    VERIFY( false );
  } catch (const std::bad_expected_access<int>& e) {
    VERIFY( e.error() == 9 );
  }

  const auto& e2 = e1;
  try {
    e2.value();
    VERIFY( false );
  } catch (const std::bad_expected_access<int>& e) {
    VERIFY( e.error() == 9 );
  }
  try {
    std::move(e2).value();
    VERIFY( false );
  } catch (const std::bad_expected_access<int>& e) {
    VERIFY( e.error() == 9 );
  }

  std::expected<void, int> v1 = std::unexpected(8);
  try {
    v1.value();
    VERIFY( false );
  } catch (const std::bad_expected_access<int>& e) {
    VERIFY( e.error() == 8 );
  }
  try {
    std::move(v1).value();
    VERIFY( false );
  } catch (const std::bad_expected_access<int>& e) {
    VERIFY( e.error() == 8 );
  }
}

constexpr bool
test_error()
{
  std::expected<int, X> e1(std::unexpect);

  VERIFY( e1.error().f() == 1 );
  VERIFY( std::move(e1).error().f() == 3 );
  const auto& e2 = e1;
  VERIFY( e2.error().f() == 2 );
  VERIFY( std::move(e2).error().f() == 4 );

  std::expected<void, X> v1(std::unexpect);

  VERIFY( v1.error().f() == 1 );
  VERIFY( std::move(v1).error().f() == 3 );
  const auto& v2 = v1;
  VERIFY( v2.error().f() == 2 );
  VERIFY( std::move(v2).error().f() == 4 );

  return true;
}

constexpr bool
test_value_or()
{
  struct Movable
  {
    constexpr Movable(int i) : x(i) { }
    constexpr Movable(const Movable&) = default;
    constexpr Movable(Movable&& m) : x(m.x) { m.x = -1; }
    int x;

    constexpr bool operator==(int i) const { return x == i; }
  };

  std::expected<Movable, int> e1(1);

  Movable m2(2);
  VERIFY( e1.value_or(2) == 1 );
  VERIFY( e1.value_or(m2) == 1 );
  VERIFY( e1.value_or(std::move(m2)) == 1 );
  VERIFY( m2 == 2 );

  VERIFY( std::move(e1).value_or(m2) == 1 );
  VERIFY( *e1 == -1 ); // moved
  VERIFY( m2 == 2 );

  e1 = std::unexpected(3);
  VERIFY( e1.value_or(m2) == 2 );
  VERIFY( m2 == 2 );
  VERIFY( std::move(e1).value_or(m2) == 2 );
  VERIFY( m2 == 2 );

  VERIFY( e1.value_or(std::move(m2)) == 2 );
  VERIFY( m2 == -1 );

  m2.x = 4;
  VERIFY( std::move(e1).value_or(std::move(m2)) == 4 );
  VERIFY( m2 == -1 );

  VERIFY( e1.value_or(5) == 5 );
  VERIFY( std::move(e1).value_or(6) == 6 );

  return true;
}

constexpr bool
test_error_or()
{
  std::expected<int, int> e1(1), e2(std::unexpect, 3);
  VERIFY( e1.error_or(2) == 2 );
  VERIFY( std::move(e1).error_or(2) == 2 );
  VERIFY( e2.error_or(2) == 3 );
  VERIFY( std::move(e2).error_or(2) == 3 );

  std::expected<void, int> e3, e4(std::unexpect, 3);
  VERIFY( e3.error_or(2) == 2 );
  VERIFY( std::move(e3).error_or(2) == 2 );
  VERIFY( e4.error_or(2) == 3 );
  VERIFY( std::move(e4).error_or(2) == 3 );

  return true;
}

int main()
{
  static_assert( test_arrow() );
  test_arrow();
  static_assert( test_star() );
  test_star();
  static_assert( test_has_value() );
  test_has_value();
  static_assert( test_value() );
  test_value();
  test_value_throw();
  static_assert( test_error() );
  test_error();
  static_assert( test_value_or() );
  test_value_or();
  static_assert( test_error_or() );
  test_error_or();
}
