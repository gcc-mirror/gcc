// { dg-do run { target c++26 }  }

#include <optional>
#include <type_traits>
#include <testsuite_hooks.h>

struct NonMovable
{
  constexpr NonMovable() {}
  NonMovable(NonMovable&&) = delete;
};

template<typename T>
constexpr
void test_value(T& t)
{
  std::optional<T&> o1(t);
  const std::optional<T&> co1(t);

  static_assert( std::is_same_v<decltype(o1.value()), T&> );
  VERIFY( &o1.value() == &t );

  static_assert( std::is_same_v<decltype(co1.value()), T&> );
  VERIFY( &co1.value() == &t );

  static_assert( std::is_same_v<decltype(std::move(o1).value()), T&> );
  VERIFY( &std::move(o1).value() == &t );

  static_assert( std::is_same_v<decltype(std::move(co1).value()), T&> );
  VERIFY( &std::move(co1).value() == &t );

  std::optional<const T&> o2(t);
  static_assert( std::is_same_v<decltype(o2.value()), const T&> );
  VERIFY( &o2.value() == &t );
}

struct Tracker
{
  int copy = 0;
  int move = 0;

  constexpr Tracker(int v) : copy(v), move(v) {}

  constexpr Tracker(Tracker const& o)
  : copy(o.copy+1), move(o.move)
  {}

  constexpr Tracker(Tracker&& o)
  : copy(o.copy), move(o.move+1)
  {}

  Tracker& operator=(Tracker) = delete;
};

constexpr
void test_value_or()
{
  Tracker t(100), u(200);
  std::optional<Tracker&> o(t);

  Tracker r1 = o.value_or(u);
  VERIFY( r1.copy == 101 );
  VERIFY( r1.move == 100 );
  Tracker r2 = o.value_or(std::move(u));
  VERIFY( r2.copy == 101 );
  VERIFY( r2.move == 100 );
  Tracker r3 = std::move(o).value_or(u);
  VERIFY( r3.copy == 101 );
  VERIFY( r3.move == 100 );
  Tracker r4 = std::move(o).value_or(std::move(u));
  VERIFY( r4.copy == 101 );
  VERIFY( r4.move == 100 );

  o.reset();
  Tracker r5 = o.value_or(u);
  VERIFY( r5.copy == 201 );
  VERIFY( r5.move == 200 );
  Tracker r6 = o.value_or(std::move(u));
  VERIFY( r6.copy == 200 );
  VERIFY( r6.move == 201 );
  Tracker r7 = std::move(o).value_or(u);
  VERIFY( r7.copy == 201 );
  VERIFY( r7.move == 200 );
  Tracker r8 = std::move(o).value_or(std::move(u));
  VERIFY( r8.copy == 200 );
  VERIFY( r8.move == 201 );
}

template<typename T>
concept has_value_or_for = requires(std::optional<T&> t, T& u)
{ t.value_or(u); };

static_assert(  has_value_or_for<int> );
static_assert(  has_value_or_for<NonMovable> );
static_assert( !has_value_or_for<int[2]> );
static_assert(  has_value_or_for<int(*)[2]> );
static_assert( !has_value_or_for<int()> );
static_assert(  has_value_or_for<int(*)()> );

int i;
NonMovable nm;
int arr[2];
int foo();

int main()
{
  auto test_all = [] {
    test_value<int>(i);
    test_value<NonMovable>(nm);
    test_value<int[2]>(arr);
    test_value<int()>(foo);

    test_value_or();
    return true;
  };

  test_all();
  static_assert(test_all());
}
