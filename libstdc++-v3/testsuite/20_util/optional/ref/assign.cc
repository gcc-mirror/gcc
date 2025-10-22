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
  NonMovable() = default;
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
  struct Counter
  {
    int copy;
    int move;
  };

  Counter ctor{0,0};
  Counter assign{0,0};

  Tracker() = default;

  constexpr Tracker(Tracker const& o)
  : ctor(o.ctor), assign(o.assign)
  {
    ctor.copy += 1;
  }

  constexpr Tracker(Tracker&& o)
  : ctor(o.ctor), assign(o.assign)
  {
    ctor.move += 1;
  }

  constexpr Tracker& operator=(const Tracker& o)
  {
    assign.copy += 1;
    return *this;
  }

  constexpr Tracker& operator=(Tracker&& o)
  {
    assign.move += 1;
    return *this;
  }

};

template<typename T>
void
test_trivial()
{
  static_assert(std::is_copy_assignable_v<std::optional<T&>>);
  static_assert(std::is_move_assignable_v<std::optional<T&>>);
}

constexpr void
test_trivial_all()
{
  test_trivial<int>();
  test_trivial<NonTrivial>();
  test_trivial<std::optional<int&>>();
}

constexpr void
test_copy()
{
  Tracker t, u;
  std::optional<Tracker&> e;
  std::optional<Tracker&> o1(t);
  std::optional<Tracker&> o2(u);

  o2 = o1;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &t );
  VERIFY( o2.has_value() );
  VERIFY( &o2.value() == &t );
  VERIFY( t.ctor.copy == 0 );
  VERIFY( t.ctor.move == 0 );
  VERIFY( t.assign.copy == 0 );
  VERIFY( t.assign.move == 0 );

  o2 = e;
  VERIFY( !o2.has_value() );

  o2 = std::move(o1);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &t );
  VERIFY( o2.has_value() );
  VERIFY( &o2.value() == &t );
  VERIFY( t.ctor.copy == 0 );
  VERIFY( t.ctor.move == 0 );
  VERIFY( t.assign.copy == 0 );
  VERIFY( t.assign.move == 0 );

  o2 = std::move(e);
  VERIFY( !o2.has_value() );
}

template<typename T, typename U>
concept can_emplace = requires (T t, U&& u)
{ t.emplace(std::forward<U>(u)); };

constexpr void
test_from_value()
{
  NonTrivial v, u;
  const NonTrivial& cv = v;
  const std::optional<NonTrivial&> s(u);
  std::optional<NonTrivial&> o1;
  std::optional<const NonTrivial&> co1;

  o1 = s;
  o1 = v;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );

  o1.reset();
  VERIFY( !o1.has_value() );

  o1 = v;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const NonTrivial&> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       NonTrivial> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const NonTrivial> );

  o1 = s;
  o1.emplace(v);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );

  o1 = std::nullopt;
  VERIFY( !o1.has_value() );

  o1.emplace(v);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );

  static_assert( !can_emplace<std::optional<NonTrivial&>, const NonTrivial&> );
  static_assert( !can_emplace<std::optional<NonTrivial&>, NonTrivial> );
  static_assert( !can_emplace<std::optional<NonTrivial&>, const NonTrivial> );

  co1 = s;
  co1 = v;
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v );

  co1 = std::nullopt;
  co1 = cv;
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v );
  // No binding to rvalue
  static_assert( !std::is_assignable_v<std::optional<const NonTrivial&>,
				       NonTrivial> );
  static_assert( !std::is_assignable_v<std::optional<const NonTrivial&>,
				       const NonTrivial> );

  co1 = std::nullopt;
  co1.emplace(v);
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v );

  co1 = s;
  co1.emplace(cv);
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v );
  // No binding to rvalue
  static_assert( !can_emplace<std::optional<const NonTrivial&>, const NonTrivial> );
  static_assert( !can_emplace<std::optional<const NonTrivial&>, NonTrivial> );


  // Conversion create a pr-value that would bind to temporary
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       Conv<NonTrivial>&> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const Conv<NonTrivial>&> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       Conv<NonTrivial>> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const Conv<NonTrivial>> );

  static_assert( !can_emplace<std::optional<NonTrivial&>, Conv<NonTrivial>&> );
  static_assert( !can_emplace<std::optional<NonTrivial&>, const Conv<NonTrivial>&> );
  static_assert( !can_emplace<std::optional<NonTrivial&>, Conv<NonTrivial>> );
  static_assert( !can_emplace<std::optional<NonTrivial&>, const Conv<NonTrivial>> );

  Conv<NonTrivial&> rw(v);
  const Conv<NonTrivial&> crw(v);

  o1 = std::nullopt;
  o1 = rw;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  o1 = s;
  o1 = crw;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  o1 = s;
  o1 = std::move(rw);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  o1 = std::nullopt;
  o1 = std::move(crw);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );

  o1 = s;
  o1.emplace(rw);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  o1 = std::nullopt;
  o1.emplace(crw);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  o1 = std::nullopt;
  o1.emplace(std::move(rw));
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
  o1 = s;
  o1.emplace(std::move(crw));
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v );
}

constexpr void
test_from_opt_value()
{
  NonTrivial u;
  std::optional<NonTrivial> v(std::in_place);
  const std::optional<NonTrivial>& cv = v;

  const std::optional<NonTrivial&> s(u);
  std::optional<NonTrivial&> o1;
  std::optional<const NonTrivial&> co1;

  o1 = s;
  o1 = v;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v.value() );
  o1 = std::nullopt;
  o1 = v;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v.value() );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const std::optional<NonTrivial>&> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       std::optional<NonTrivial>> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const std::optional<NonTrivial>> );

  co1 = s;
  co1 = v;
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v.value() );
  co1 = std::nullopt;
  co1 = cv;
  VERIFY( co1.has_value() );
  VERIFY( &co1.value() == &v.value() );
  // No binding to rvalue
  static_assert( !std::is_assignable_v<std::optional<const NonTrivial&>,
				       std::optional<NonTrivial>> );
  static_assert( !std::is_assignable_v<std::optional<const NonTrivial&>,
				       const std::optional<NonTrivial>> );

  // Conversion create a pr-value that would bind to temporary
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       std::optional<Conv<NonTrivial>>&> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const std::optional<Conv<NonTrivial>>&> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       std::optional<Conv<NonTrivial>>> );
  static_assert( !std::is_assignable_v<std::optional<NonTrivial&>,
				       const std::optional<Conv<NonTrivial>>> );

  std::optional<Conv<NonTrivial&>> rw(*v);
  std::optional<const Conv<NonTrivial&>> crw(*v);

  o1 = std::nullopt;
  o1 = rw;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v.value() );
  o1 = s;
  o1 = crw;
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v.value() );
  o1 = s;
  o1 = std::move(rw);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v.value() );
  o1 = std::nullopt;
  o1 = std::move(crw);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &v.value() );
}

constexpr void
test_to_opt_value()
{
  Tracker t;
  std::optional<Tracker&> er;
  std::optional<Tracker&> r(t);
  const std::optional<Tracker&> cr(t);

  std::optional<Tracker> o1;
  o1 = r;
  VERIFY( o1.has_value() );
  VERIFY( o1->ctor.copy == 1 );
  VERIFY( o1->ctor.move == 0 );
  VERIFY( o1->assign.copy == 0 );
  VERIFY( o1->assign.move == 0 );

  o1 = r;
  VERIFY( o1.has_value() );
  VERIFY( o1->ctor.copy == 1 );
  VERIFY( o1->ctor.move == 0 );
  VERIFY( o1->assign.copy == 1 );
  VERIFY( o1->assign.move == 0 );

  o1 = er;
  VERIFY( !o1.has_value() );

  o1 = cr;
  VERIFY( o1.has_value() );
  VERIFY( o1->ctor.copy == 1 );
  VERIFY( o1->ctor.move == 0 );
  VERIFY( o1->assign.copy == 0 );
  VERIFY( o1->assign.move == 0 );

  o1 = cr;
  VERIFY( o1.has_value() );
  VERIFY( o1->ctor.copy == 1 );
  VERIFY( o1->ctor.move == 0 );
  VERIFY( o1->assign.copy == 1 );
  VERIFY( o1->assign.move == 0 );

  o1 = std::move(er);

  o1 = std::move(r);
  VERIFY( o1.has_value() );
  VERIFY( o1->ctor.copy == 1 );
  VERIFY( o1->ctor.move == 0 );
  VERIFY( o1->assign.copy == 0 );
  VERIFY( o1->assign.move == 0 );

  o1 = std::move(cr);
  VERIFY( o1.has_value() );
  VERIFY( o1->ctor.copy == 1 );
  VERIFY( o1->ctor.move == 0 );
  VERIFY( o1->assign.copy == 1 );
  VERIFY( o1->assign.move == 0 );
}

constexpr void
test_swap()
{
  NonMovable a, b;
  std::optional<NonMovable&> oa(a), ob(b);

  oa.swap(ob);
  static_assert(noexcept(oa.swap(ob)));
  VERIFY( oa.has_value() );
  VERIFY( &oa.value() == &b );
  VERIFY( ob.has_value() );
  VERIFY( &ob.value() == &a );

  swap(oa, ob);
  static_assert(std::is_nothrow_swappable_v<std::optional<NonMovable&>>);
  VERIFY( oa.has_value() );
  VERIFY( &oa.value() == &a );
  VERIFY( ob.has_value() );
  VERIFY( &ob.value() == &b );

  ob.reset();
  oa.swap(ob);
  VERIFY( !oa.has_value() );
  VERIFY( ob.has_value() );
  VERIFY( &ob.value() == &a );

  ob.reset();
  std::swap(oa, ob);
  VERIFY( !oa.has_value() );
  VERIFY( !ob.has_value() );

  std::optional<const NonMovable&> ca(a), cb(b);
  swap(ca, cb);
  VERIFY( ca.has_value() );
  VERIFY( &ca.value() == &b );
  VERIFY( cb.has_value() );
  VERIFY( &cb.value() == &a );

  static_assert(!std::is_swappable_with_v<std::optional<int>&, std::optional<int&>&>);
}

int main()
{
  auto test_all = [] {
    test_copy();
    test_from_value();
    test_from_opt_value();
    test_to_opt_value();
    test_swap();
    return true;
  };

  test_all();
  static_assert(test_all());
}
