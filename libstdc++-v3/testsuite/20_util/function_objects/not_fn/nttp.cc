// Test NTTP version of not_fn, from P2714

// { dg-do run { target c++26 } }

#include <functional>
#include <testsuite_hooks.h>

using std::not_fn;

int func(int, char) { return 0; }

struct F
{
  bool operator()() { return false; }
  bool operator()() const { return true; }
  bool operator()(int) const { return false; }
};

void
test01()
{
  auto f1 = not_fn<func>();
  VERIFY( std::is_empty_v<decltype(f1)> );
  VERIFY( f1(1, '2') == true );

  auto f2 = not_fn<[] { return true; }>();
  VERIFY( std::is_empty_v<decltype(f2)> );
  VERIFY( f2() == false );

  auto f3 = not_fn<F{}>();
  VERIFY( f3() == false );  // Prefer the const member.
  VERIFY( f3(1) == true );
  const auto f4 = f3;
  VERIFY( f4() == false );
}

void
test04()
{
  struct abstract { virtual void f() = 0; };
  struct derived : abstract { void f() { } };
  struct F { bool operator()(const abstract&) const { return false; } };
  constexpr F f;
  constexpr derived d;
  VERIFY( not_fn<f>()(d) );
}

void
test05()
{
  auto nf = std::not_fn<[] { return false; }>();
  auto copy(nf); // PR libstdc++/70564
}

void
test06()
{
  struct Boolean {
    Boolean operator!() const noexcept(false) { return *this; }
  };
  struct F {
    Boolean operator()() const { return {}; }
  };
  const F f;
  const auto notf = std::not_fn<f>();
  using NotF = decltype(notf);
  static_assert( std::is_invocable<NotF>::value, "cannot negate" );
  static_assert( !noexcept(notf()), "conversion to bool affects noexcept" );
}

void
test07()
{
  struct NonNegatable { };  // there is no operator!(NonNegatable)
  struct F {
    NonNegatable operator()() const { return {}; }
  };
  F f;
  constexpr auto notf = std::not_fn<f>();
  using NotF = decltype(notf);
  static_assert( !std::is_invocable<NotF>::value, "cannot negate" );
}

void
test08()
{
  struct quals
  {
    bool as_const;
    bool as_lvalue;

    quals operator!() const
    { return *this; };
  };

  struct F
  {
    quals operator()(int&) const { return { false, true }; }
    quals operator()(int const&) const { return { true, true }; }
    quals operator()(int&&) const { return { false, false }; }
    quals operator()(int const&&) const { return { true, false }; }
  };

  constexpr F f;
  auto g = not_fn<f>();
  const auto& cg = g;
  quals q;
  int i = 10;
  const int ci = i;

  q = g(i);
  VERIFY( ! q.as_const && q.as_lvalue );
  q = g(std::move(i));
  VERIFY( ! q.as_const && ! q.as_lvalue );
  q = g(ci);
  VERIFY( q.as_const && q.as_lvalue );
  q = g(std::move(ci));
  VERIFY( q.as_const && ! q.as_lvalue );

  q = cg(i);
  VERIFY( ! q.as_const && q.as_lvalue );
  q = cg(std::move(i));
  VERIFY( ! q.as_const && ! q.as_lvalue );
  q = cg(ci);
  VERIFY( q.as_const && q.as_lvalue );
  q = cg(std::move(ci));
  VERIFY( q.as_const && ! q.as_lvalue );
}

int
main()
{
  test01();
  test04();
  test05();
  test06();
  test07();
  test08();
  constexpr auto f = []{ return false; };
  static_assert(std::not_fn<f>()());
}
