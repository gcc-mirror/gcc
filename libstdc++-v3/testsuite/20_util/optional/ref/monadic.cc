// { dg-do run { target c++26 }  }

#include <optional>
#include <type_traits>
#include <testsuite_hooks.h>
#include <utility>

struct NonMovable
{
  constexpr NonMovable() {}
  NonMovable(NonMovable&&) = delete;
};

struct Tracker
{
  int copy = 0;
  int move = 0;

  Tracker() = default;

  constexpr Tracker(Tracker const& o)
  : copy(o.copy+1), move(o.move)
  {}

  constexpr Tracker(Tracker&& o)
  : copy(o.copy), move(o.move+1)
  {}

  Tracker& operator=(Tracker) = delete;

  void reset() {
    copy = move = 0;
  }
};

template<typename T>
auto identity_of = []<typename U>(U&& t) -> std::optional<T>
{
  static_assert( std::is_same_v<T, U&&> );
  VERIFY( t.copy == 0 );
  VERIFY( t.move == 0 );
  return std::optional<T>(t);
};

constexpr void
test_and_then()
{
  std::optional<Tracker> t(std::in_place);
  std::optional<Tracker&> rt(t);
  std::optional<const Tracker&> rct(t);

  auto r1 = t.and_then(identity_of<Tracker&>);
  VERIFY( r1.has_value() );
  VERIFY( &r1.value() == &t.value() );

  auto r2 = rt.and_then(identity_of<Tracker&>);
  VERIFY( r2.has_value() );
  VERIFY( &r2.value() == &t.value() );

  std::as_const(rt).and_then(identity_of<Tracker&>);
  std::move(rt).and_then(identity_of<Tracker&>);

  auto r4 = rct.and_then(identity_of<const Tracker&>);
  VERIFY( r4.has_value() );
  VERIFY( &r4.value() == &t.value() );

  std::as_const(rct).and_then(identity_of<const Tracker&>);
  std::move(rct).and_then(identity_of<const Tracker&>);

  auto r5 = rt.and_then([](Tracker&) { return std::optional<int>(42); });
  static_assert( std::is_same_v<decltype(r5), std::optional<int>> );
  VERIFY( r5.has_value() );
  VERIFY( r5.value() == 42 );

  auto r6 = rct.and_then([](const Tracker&) { return std::optional<int>(); });
  static_assert( std::is_same_v<decltype(r6), std::optional<int>> );
  VERIFY( !r6.has_value() );

  rct.reset();
  auto r7 = rct.and_then([](const Tracker&) { VERIFY(false); return std::optional<int>(42); });
  static_assert( std::is_same_v<decltype(r7), std::optional<int>> );
  VERIFY( !r7.has_value() );

  rt.reset();
  auto r8 = rt.and_then([](Tracker&) { VERIFY(false); return std::optional<int>(); });
  static_assert( std::is_same_v<decltype(r8), std::optional<int>> );
  VERIFY( !r8.has_value() );
}

template<typename T>
constexpr void
test_or_else()
{
  T t, u;

  std::optional<T&> ot(t);
  auto r1 = ot.or_else([&] { VERIFY(false); return std::optional<T&>(u); });
  VERIFY( &ot.value() == &t );
  VERIFY( r1.has_value() );
  VERIFY( &r1.value() == &t );
  auto r2 = std::move(ot).or_else([&] { VERIFY(false); return std::optional<T&>(); });
  VERIFY( &ot.value() == &t );
  VERIFY( r2.has_value() );
  VERIFY( &r2.value() == &t );

  ot.reset();
  auto r3 = ot.or_else([&] { return std::optional<T&>(u); });
  VERIFY( !ot.has_value() );
  VERIFY( r3.has_value() );
  VERIFY( &r3.value() == &u );
  auto r4 = std::move(ot).or_else([] { return std::optional<T&>(); });
  VERIFY( !ot.has_value() );
  VERIFY( !r4.has_value() );
}

constexpr void
test_transform()
{
  std::optional<Tracker> t(std::in_place);

  auto r1 = t.transform(&Tracker::copy);
  static_assert( std::is_same_v<decltype(r1), std::optional<int&>> );
  VERIFY( r1.has_value() );
  VERIFY( &r1.value() == &t->copy );
  auto r2 = std::as_const(t).transform(&Tracker::move);
  static_assert( std::is_same_v<decltype(r2), std::optional<const int&>> );
  VERIFY( r2.has_value() );
  VERIFY( &r2.value() == &t->move );

  std::optional<Tracker&> rt(t);
  auto r3 = rt.transform(&Tracker::copy);
  static_assert( std::is_same_v<decltype(r3), std::optional<int&>> );
  VERIFY( r3.has_value() );
  VERIFY( &r3.value() == &t->copy );
  auto r4 = std::as_const(rt).transform(&Tracker::copy);
  static_assert( std::is_same_v<decltype(r4), std::optional<int&>> );
  VERIFY( r4.has_value() );
  VERIFY( &r4.value() == &t->copy );
  auto r5 = std::move(rt).transform(&Tracker::copy);
  static_assert( std::is_same_v<decltype(r5), std::optional<int&>> );
  VERIFY( r5.has_value() );
  VERIFY( &r5.value() == &t->copy );

  auto r6 = rt.transform([] (Tracker& t) { return 10; });
  static_assert( std::is_same_v<decltype(r6), std::optional<int>> );
  VERIFY( r6.has_value() );
  VERIFY( &r6.value() != &t->copy );
  VERIFY( r6.value() == 10 );

  auto r7 = rt.transform([] (Tracker& t) { return NonMovable(); });
  static_assert( std::is_same_v<decltype(r7), std::optional<NonMovable>> );
  VERIFY( r7.has_value() );

  rt.reset();
  auto r8 = rt.transform([] (Tracker& t) { VERIFY(false); return 42; });
  static_assert( std::is_same_v<decltype(r8), std::optional<int>> );
  VERIFY( !r8.has_value() );

  std::optional<const Tracker&> crt(t);
  auto r9 = crt.transform(&Tracker::copy);
  static_assert( std::is_same_v<decltype(r9), std::optional<const int&>> );
  VERIFY( r9.has_value() );
  VERIFY( &r9.value() == &t->copy );
  auto r10 = std::as_const(crt).transform(&Tracker::copy);
  static_assert( std::is_same_v<decltype(r10), std::optional<const int&>> );
  VERIFY( r10.has_value() );
  VERIFY( &r10.value() == &t->copy );
  auto r11 = std::move(crt).transform(&Tracker::copy);
  static_assert( std::is_same_v<decltype(r11), std::optional<const int&>> );
  VERIFY( r11.has_value() );
  VERIFY( &r11.value() == &t->copy );

  crt.reset();
  auto r12 = rt.transform([] (Tracker& t) { VERIFY(false); return 42; });
  static_assert( std::is_same_v<decltype(r12), std::optional<int>> );
  VERIFY( !r12.has_value() );
}

int main()
{
  auto test_all = [] {
    test_and_then();
    test_transform();
    test_or_else<Tracker>();
    test_or_else<const Tracker>();
    test_or_else<NonMovable>();
    return true;
  };

  test_all();
  static_assert(test_all());
}
