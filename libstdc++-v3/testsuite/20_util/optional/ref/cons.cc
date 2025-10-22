// { dg-do run { target c++26 }  }

#include <optional>
#include <type_traits>
#include <testsuite_hooks.h>

struct NonTrivial
{
  constexpr NonTrivial() {}
  constexpr NonTrivial(NonTrivial const&) {};
  constexpr ~NonTrivial() {};
};

struct NonMovable
{
  constexpr NonMovable() {}
  NonMovable(NonMovable&&) = delete;
};

template<typename T>
struct Conv
{
  T t;

  constexpr operator T() const noexcept
  { return t; }
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
};

template<typename T>
void
test_trivial()
{
  static_assert(std::is_trivially_copyable_v<std::optional<T&>>);
  static_assert(std::is_copy_constructible_v<std::optional<T&>>);
  static_assert(std::is_move_constructible_v<std::optional<T&>>);
  static_assert(std::is_destructible_v<std::optional<T&>>);
}

constexpr void
test_trivial_all()
{
  test_trivial<int>();
  test_trivial<NonTrivial>();
  test_trivial<NonMovable>();
  test_trivial<std::optional<int&>>();
}

constexpr void
test_copy()
{
  Tracker t;
  std::optional<Tracker&> o1(t);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &t );
  VERIFY( t.copy == 0 );
  VERIFY( t.move == 0 );

  std::optional<Tracker&> o2(o1);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &t );
  VERIFY( o2.has_value() );
  VERIFY( &o2.value() == &t );
  VERIFY( t.copy == 0 );
  VERIFY( t.move == 0 );

  std::optional<Tracker&> o3(std::move(o1));
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &t );
  VERIFY( o3.has_value() );
  VERIFY( &o3.value() == &t );
  VERIFY( t.copy == 0 );
  VERIFY( t.move == 0 );

  std::optional<Tracker&> e;
  VERIFY( !e.has_value() );

  std::optional<Tracker&> o4(e);
  VERIFY( !e.has_value() );
  VERIFY( !o4.has_value() );

  std::optional<Tracker&> o5(std::move(e));
  VERIFY( !e.has_value() );
  VERIFY( !o5.has_value() );
}


constexpr void
test_from_value()
{
  NonTrivial v;
  const NonTrivial& cv = v;

  std::optional<NonTrivial&> o1(v);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const NonTrivial&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  NonTrivial> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const NonTrivial> );

  std::optional<NonTrivial&> o2(std::in_place, v);
  VERIFY( o2.has_value() );
  VERIFY( &o2.value() == &v );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::in_place_t, const NonTrivial&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::in_place_t, NonTrivial> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::in_place_t, const NonTrivial> );

  std::optional<const NonTrivial&> co1(v);
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v );
  std::optional<const NonTrivial&> co2(cv);
  VERIFY( co2.has_value() );
  VERIFY( &co2.value() == &v );
  // No binding to rvalue
  static_assert( !std::is_constructible_v<std::optional<const NonTrivial&>,
					  NonTrivial> );
  static_assert( !std::is_constructible_v<std::optional<const NonTrivial&>,
					  const NonTrivial> );

  std::optional<const NonTrivial&> co3(std::in_place, v);
  VERIFY( co3.has_value() );
  VERIFY( &co3.value() == &v );
  std::optional<const NonTrivial&> co4(std::in_place, cv);
  VERIFY( co4.has_value() );
  VERIFY( &co4.value() == &v );
  // No binding to rvalue
  static_assert( !std::is_constructible_v<std::optional<const NonTrivial&>,
					  std::in_place_t, NonTrivial> );
  static_assert( !std::is_constructible_v<std::optional<const NonTrivial&>,
					  std::in_place_t, const NonTrivial> );

  // Conversion create a pr-value that would bind to temporary
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  Conv<NonTrivial>&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const Conv<NonTrivial>&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  Conv<NonTrivial>> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const Conv<NonTrivial>> );

  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::in_place_t, Conv<NonTrivial>&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::in_place_t, const Conv<NonTrivial>&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::in_place_t, Conv<NonTrivial>> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::in_place_t, const Conv<NonTrivial>> );

  Conv<NonTrivial&> rw(v);
  const Conv<NonTrivial&> crw(v);

  std::optional<NonTrivial&> ro1(rw);
  VERIFY( ro1.has_value() );
  VERIFY( &ro1.value() == &v );
  std::optional<NonTrivial&> ro2(crw);
  VERIFY( ro2.has_value() );
  VERIFY( &ro2.value() == &v );
  std::optional<NonTrivial&> ro3(std::move(rw));
  VERIFY( ro3.has_value() );
  VERIFY( &ro3.value() == &v );
  std::optional<NonTrivial&> ro4(std::move(crw));
  VERIFY( ro4.has_value() );
  VERIFY( &ro4.value() == &v );

  std::optional<NonTrivial&> ro5(std::in_place, rw);
  VERIFY( ro5.has_value() );
  VERIFY( &ro5.value() == &v );
  std::optional<NonTrivial&> ro6(std::in_place, crw);
  VERIFY( ro6.has_value() );
  VERIFY( &ro6.value() == &v );
  std::optional<NonTrivial&> ro7(std::in_place, std::move(rw));
  VERIFY( ro7.has_value() );
  VERIFY( &ro7.value() == &v );
  std::optional<NonTrivial&> ro8(std::in_place, std::move(crw));
  VERIFY( ro8.has_value() );
  VERIFY( &ro8.value() == &v );
}

constexpr void
test_from_opt_value()
{
  std::optional<NonTrivial> v(std::in_place);
  const std::optional<NonTrivial>& cv = v;

  std::optional<NonTrivial&> o1(v);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v.value() );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const std::optional<NonTrivial>&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::optional<NonTrivial>> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const std::optional<NonTrivial>> );

  std::optional<const NonTrivial&> co1(v);
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v.value() );
  std::optional<const NonTrivial&> co2(cv);
  VERIFY( co2.has_value() );
  VERIFY( &co2.value() == &v.value() );
  // No binding to rvalue
  static_assert( !std::is_constructible_v<std::optional<const NonTrivial&>,
					  std::optional<NonTrivial>> );
  static_assert( !std::is_constructible_v<std::optional<const NonTrivial&>,
					  const std::optional<NonTrivial>> );

  // Conversion create a pr-value that would bind to temporary
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::optional<Conv<NonTrivial>>&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const std::optional<Conv<NonTrivial>>&> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  std::optional<Conv<NonTrivial>>> );
  static_assert( !std::is_constructible_v<std::optional<NonTrivial&>,
					  const std::optional<Conv<NonTrivial>>> );

  std::optional<Conv<NonTrivial&>> rw(*v);
  std::optional<const Conv<NonTrivial&>> crw(*v);

  std::optional<NonTrivial&> ro1(rw);
  VERIFY( ro1.has_value() );
  VERIFY( &ro1.value() == &v.value() );
  std::optional<NonTrivial&> ro2(crw);
  VERIFY( ro2.has_value() );
  VERIFY( &ro2.value() == &v.value() );
  std::optional<NonTrivial&> ro3(std::move(rw));
  VERIFY( ro3.has_value() );
  VERIFY( &ro3.value() == &v.value() );
  std::optional<NonTrivial&> ro4(std::move(crw));
  VERIFY( ro4.has_value() );
  VERIFY( &ro4.value() == &v.value() );
}

constexpr void
test_to_opt_value()
{
  Tracker t;
  std::optional<Tracker&> r(t);
  const std::optional<Tracker&> cr(t);

  std::optional<Tracker> o1(r);
  VERIFY( o1.has_value() );
  VERIFY( o1->copy == 1 );
  VERIFY( o1->move == 0 );

  std::optional<Tracker> o2(cr);
  VERIFY( o2.has_value() );
  VERIFY( o2->copy == 1 );
  VERIFY( o2->move == 0 );

  std::optional<Tracker> o3(std::move(r));
  VERIFY( o3.has_value() );
  VERIFY( o3->copy == 1 );
  VERIFY( o3->move == 0 );

  std::optional<Tracker> o4(std::move(cr));
  VERIFY( o4.has_value() );
  VERIFY( o4->copy == 1 );
  VERIFY( o4->move == 0 );

  std::optional<Tracker&> er;
  const std::optional<Tracker&> cer;

  std::optional<Tracker> e1(er);
  VERIFY( !e1.has_value() );

  std::optional<Tracker> e2(cer);
  VERIFY( !e2.has_value() );

  std::optional<Tracker> e3(std::move(er));
  VERIFY( !e3.has_value() );

  std::optional<Tracker> e4(std::move(cer));
  VERIFY( !e4.has_value() );
}

constexpr void
test_opt_opt()
{
  std::optional<int> s(43);

  std::optional<std::optional<int>&> o1(s);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &s );

  std::optional<std::optional<int>&> o2(std::in_place, s);
  VERIFY( o2.has_value() );
  VERIFY( &o2.value() == &s );

  std::optional<std::optional<int>> o3(o1);
  VERIFY( o2.has_value() );
  VERIFY( o2.value().has_value() );
  VERIFY( o2.value() == 43 );

  s.reset();
  std::optional<std::optional<int>&> o4(s);
  VERIFY( o4.has_value() );
  VERIFY( &o4.value() == &s );

  std::optional<std::optional<int>&> o5(std::in_place, s);
  VERIFY( o5.has_value() );
  VERIFY( &o5.value() == &s );

  std::optional<std::optional<int>> o6(o1);
  VERIFY( o6.has_value() );
  VERIFY( !o6.value().has_value() );

  std::optional<std::optional<int>> s2(std::in_place);
  std::optional<std::optional<int>&> oo1(s2);
  VERIFY( oo1.has_value() );
  VERIFY( &oo1.value() == &s2.value() );

  s2.reset();
  std::optional<std::optional<int>&> oo2(s2);
  VERIFY( !oo2.has_value() );
}

int main()
{
  auto test_all = [] {
    test_copy();
    test_from_value();
    test_from_opt_value();
    test_to_opt_value();
    test_opt_opt();
    return true;
  };

  test_all();
  static_assert(test_all());
}
