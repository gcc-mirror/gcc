// { dg-do run { target c++23 } }

#include <expected>
#include <type_traits>
#include <testsuite_hooks.h>

int dtor_count;
constexpr void reset_dtor_count()
{
  if (!std::is_constant_evaluated())
    dtor_count = 0;
}
constexpr void inc_dtor_count()
{
  if (!std::is_constant_evaluated())
    ++dtor_count;
}
constexpr bool check_dtor_count(int c)
{
  if (std::is_constant_evaluated())
    return true;
  return dtor_count == c;
}

struct X
{
  constexpr X(int i, int j = 0) noexcept : n(i+j) { }
  constexpr X(std::initializer_list<int> l, void*) noexcept : n(l.size()) { }

  constexpr X(const X&) = default;
  constexpr X(X&& x) noexcept : n(x.n) { x.n = -1; }

  constexpr X& operator=(const X&) = default;
  constexpr X& operator=(X&& x) noexcept { n = x.n; x.n = -1; return *this; }

  constexpr ~X()
  {
    inc_dtor_count();
  }

  constexpr bool operator==(const X&) const = default;
  constexpr bool operator==(int i) const { return n == i; }

  int n;
};

constexpr bool
test_copy(bool = true)
{
  reset_dtor_count();

  std::expected<int, int> e1(1), e2(2), e3(std::unexpect, 3);

  e1 = e1;
  e1 = e2; // T = T
  VERIFY( e1.value() == e2.value() );
  e1 = e3; // T = E
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == e3.error() );
  e1 = e3; // E = E
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == e3.error() );
  e1 = e2; // E = T
  VERIFY( e1.value() == e2.value() );

  e1 = std::move(e1);
  e1 = std::move(e2); // T = T
  VERIFY( e1.value() == e2.value() );
  e1 = std::move(e3); // T = E
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == e3.error() );
  e1 = std::move(e3); // E = E
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == e3.error() );
  e1 = std::move(e2); // E = T
  VERIFY( e1.value() == e2.value() );

  std::expected<X, X> x1(1), x2(2), x3(std::unexpect, 3);

  x1 = x1;

  x1 = x2; // T = T
  VERIFY( check_dtor_count(0) );
  VERIFY( x1.value() == x2.value() );
  x1 = x3; // T = E
  VERIFY( check_dtor_count(1) );
  VERIFY( ! x1.has_value() );
  x1 = x3; // E = E
  VERIFY( check_dtor_count(1) );
  VERIFY( ! x1.has_value() );
  x1 = x2; // E = T
  VERIFY( check_dtor_count(2) );
  VERIFY( x1.value() == x2.value() );

  reset_dtor_count();

  x1 = std::move(x1);
  VERIFY( x1.value() == -1 );

  x1 = std::move(x2); // T = T
  VERIFY( check_dtor_count(0) );
  VERIFY( x1.value() == 2 );
  VERIFY( x2.value() == -1 );
  x1 = std::move(x3); // T = E
  VERIFY( check_dtor_count(1) );
  VERIFY( ! x1.has_value() );
  VERIFY( x1.error() == 3 );
  VERIFY( x3.error() == -1 );
  x3.error().n = 33;
  x1 = std::move(x3); // E = E
  VERIFY( check_dtor_count(1) );
  VERIFY( ! x1.has_value() );
  VERIFY( x1.error() == 33 );
  VERIFY( x3.error() == -1 );
  x2.value().n = 22;
  x1 = std::move(x2); // E = T
  VERIFY( check_dtor_count(2) );
  VERIFY( x1.value() == 22 );
  VERIFY( x2.value() == -1 );

  std::expected<void, int> ev1, ev2, ev3(std::unexpect, 3);

  ev1 = ev2; // T = T
  VERIFY( ev1.has_value() );
  ev1 = ev3; // T = E
  VERIFY( ! ev1.has_value() );
  VERIFY( ev1.error() == ev3.error() );
  ev1 = ev3; // E = E
  VERIFY( ! ev1.has_value() );
  VERIFY( ev1.error() == ev3.error() );
  ev1 = ev2; // E = T
  VERIFY( ev1.has_value() );

  reset_dtor_count();
  std::expected<void, X> xv1, xv2, xv3(std::unexpect, 3);

  xv1 = std::move(xv2); // T = T
  VERIFY( check_dtor_count(0) );
  VERIFY( xv1.has_value() );
  xv1 = std::move(xv3); // T = E
  VERIFY( check_dtor_count(0) );
  VERIFY( ! xv1.has_value() );
  VERIFY( xv1.error() == 3 );
  VERIFY( xv3.error() == -1 );
  xv3.error().n = 33;
  xv1 = std::move(xv3); // E = E
  VERIFY( check_dtor_count(0) );
  VERIFY( xv1.error() == 33 );
  VERIFY( xv3.error() == -1 );
  xv1 = std::move(xv2); // E = T
  VERIFY( check_dtor_count(1) );
  VERIFY( xv1.has_value() );

  return true;
}

constexpr bool
test_converting(bool = true)
{
  std::expected<int, int> e1(1);
  std::expected<unsigned, long> e2(2U), e3(std::unexpect, 3L);
  e1 = e2;
  VERIFY( e1.value() == e2.value() );
  e1 = e3;
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == e3.error() );
  e1 = e2;
  VERIFY( e1.value() == e2.value() );

  e1 = std::move(e3);
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == e3.error() );
  e1 = std::move(e2);
  VERIFY( e1.value() == e2.value() );

  std::expected<void, int> ev4;
  std::expected<const void, long> ev5(std::unexpect, 5);
  ev4 = ev5;
  VERIFY( ! ev4.has_value() );
  VERIFY( ev4.error() == 5 );
  ev4 = std::expected<volatile void, unsigned>();
  VERIFY( ev4.has_value() );
  ev4 = std::move(ev5);
  VERIFY( ! ev4.has_value() );
  VERIFY( ev4.error() == 5 );

  return true;
}

constexpr bool
test_unexpected(bool = true)
{
  reset_dtor_count();

  std::expected<X, int> e1(0);

  e1 = std::unexpected<int>(5);
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == 5 );
  VERIFY( check_dtor_count(1) );

  e1 = std::unexpected<int>(6);
  VERIFY( check_dtor_count(1) );

  std::expected<int, X> e2;

  std::unexpected<X> x(std::in_place, 1, 2);
  e2 = x;
  VERIFY( check_dtor_count(1) );

  e2 = 1;
  VERIFY( e2.value() == 1 );
  VERIFY( check_dtor_count(2) );

  return true;
}

constexpr bool
test_emplace(bool = true)
{
  reset_dtor_count();

  std::expected<int, int> e1(1);
  e1.emplace(2);
  VERIFY( e1.value() == 2 );

  std::expected<void, int> ev2;
  ev2.emplace();
  VERIFY( ev2.has_value() );

  std::expected<X, int> e3(std::in_place, 0, 0);

  e3.emplace({1,2,3}, nullptr);
  VERIFY( e3.value() == 3 );
  VERIFY( check_dtor_count(1) );

  e3.emplace(2, 2);
  VERIFY( e3.value() == 4 );
  VERIFY( check_dtor_count(2) );

  std::expected<int, X> ev4(std::unexpect, 4);

  ev4.emplace(5);
  VERIFY( ev4.value() == 5 );
  VERIFY( check_dtor_count(3) );

  ev4.emplace(6);
  VERIFY( ev4.value() == 6 );
  VERIFY( check_dtor_count(3) );

  return true;
}

void
test_exception_safety()
{
  struct CopyThrows
  {
    CopyThrows(int i) noexcept : x(i) { }
    CopyThrows(const CopyThrows&) { throw 1; }
    CopyThrows(CopyThrows&&) = default;
    CopyThrows& operator=(const CopyThrows&) = default;
    CopyThrows& operator=(CopyThrows&&) = default;
    int x;

    bool operator==(int i) const { return x == i; }
  };

  struct MoveThrows
  {
    MoveThrows(int i) noexcept : x(i) { }
    MoveThrows(const MoveThrows&) = default;
    MoveThrows(MoveThrows&&) { throw 1L; }
    MoveThrows& operator=(const MoveThrows&) = default;
    MoveThrows& operator=(MoveThrows&&) = default;
    int x;

    bool operator==(int i) const { return x == i; }
  };

  std::expected<CopyThrows, MoveThrows> c(std::unexpect, 1);

  // operator=(U&&)
  try {
    CopyThrows x(2);
    c = x;
    VERIFY( false );
  } catch (int) {
    VERIFY( ! c.has_value() );
    VERIFY( c.error() == 1 );
  }

  c = CopyThrows(2);

  try {
    c = std::unexpected<MoveThrows>(3);
    VERIFY( false );
  } catch (long) {
    VERIFY( c.value() == 2 );
  }
}

int main(int argc, char**)
{
  bool non_constant = argc == 1; // force non-constant evaluation

  static_assert( test_copy() );
  test_copy(non_constant);
  static_assert( test_converting() );
  test_converting(non_constant);
  static_assert( test_unexpected() );
  test_unexpected(non_constant);
  static_assert( test_emplace() );
  test_emplace(non_constant);

  test_exception_safety();

  // Ensure the non-constexpr tests actually ran:
  VERIFY( dtor_count != 0 );
}
