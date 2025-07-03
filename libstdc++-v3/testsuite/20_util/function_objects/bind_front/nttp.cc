// { dg-do run { target c++26 } }
// { dg-add-options no_pch }

// Test NTTP bind_front<f>(Args...), P2714

#include <functional>

#ifndef __cpp_lib_bind_front
# error "Feature test macro for bind_front is missing in <functional>"
#elif __cpp_lib_bind_front < 202306L
# error "Feature test macro for bind_front has wrong value in <functional>"
#endif

#include <testsuite_hooks.h>

using std::bind_front;
using std::is_same_v;
using std::is_invocable_v;
using std::is_invocable_r_v;

void
test01()
{
  struct F { void operator()(int) {} };
  constexpr F f{};

  // Reference wrappers should be handled:
  static_assert(!std::is_same_v<
      decltype(bind_front<f>(std::declval<int&>())),
      decltype(bind_front<f>(std::ref(std::declval<int&>())))
      >);
  static_assert(!std::is_same_v<
      decltype(bind_front<f>(std::declval<const int&>())),
      decltype(bind_front<f>(std::cref(std::declval<int&>())))
      >);
  static_assert(!std::is_same_v<
      decltype(bind_front<f>(std::ref(std::declval<int&>()))),
      decltype(bind_front<f>(std::cref(std::declval<int&>())))
      >);
}

void
test02()
{
  struct quals
  {
    bool as_const;
    bool as_lvalue;
  };

  struct F
  {
    quals operator()(int, int) & { return { false, true }; }
    quals operator()(int, int) const & { return { true, true }; }
    quals operator()(int, int) && { return { false, false }; }
    quals operator()(int, int) const && { return { true, false }; }
  };

  // Constness and value category forwarded to the target object?
  { // no bound args
    constexpr F f;
    auto g = bind_front<f>();
    const auto& cg = g;
    quals q;

    // Constness and value category forwarded to the target object?
    q = g(0,0);
    VERIFY( q.as_const && q.as_lvalue );
    q = std::move(g)(0,0);
    VERIFY( q.as_const && q.as_lvalue );
    q = cg(0,0);
    VERIFY( q.as_const && q.as_lvalue );
    q = std::move(cg)(0,0);
    VERIFY( q.as_const && q.as_lvalue );
  }
  { // one bound arg (for when we implement that as a separate case)
    constexpr F f;
    auto g = bind_front<f>(0);
    const auto& cg = g;
    quals q;

    // Constness and value category forwarded to the target object?
    q = g(0);
    VERIFY( q.as_const && q.as_lvalue );
    q = std::move(g)(0);
    VERIFY( q.as_const && q.as_lvalue );
    q = cg(0);
    VERIFY( q.as_const && q.as_lvalue );
    q = std::move(cg)(0);
    VERIFY( q.as_const && q.as_lvalue );
  }
  { // two bound args, the general case
    constexpr F f;
    auto g = bind_front<f>(0,0);
    const auto& cg = g;
    quals q;

    q = g();
    VERIFY( q.as_const && q.as_lvalue );
    q = std::move(g)();
    VERIFY( q.as_const && q.as_lvalue );
    q = cg();
    VERIFY( q.as_const && q.as_lvalue );
    q = std::move(cg)();
    VERIFY( q.as_const && q.as_lvalue );
  }
}

void
test02a()
{
  struct quals
  {
    bool as_const;
    bool as_lvalue;
  };

  struct F
  {
    quals operator()(int&, int) const { return { false, true }; }
    quals operator()(int const&, int) const { return { true, true }; }
    quals operator()(int&&, int) const { return { false, false }; }
    quals operator()(int const&&, int) const  { return { true, false }; }
  };
  constexpr F f{};

  // verify propagation
  auto h = bind_front<f>(10);
  auto const& ch = h;
  quals q;

  q = h(0);
  VERIFY( !q.as_const && q.as_lvalue );
  q = ch(0);
  VERIFY( q.as_const && q.as_lvalue );
  q = std::move(h)(0);
  VERIFY( !q.as_const && !q.as_lvalue );
  q = std::move(ch)(0);
  VERIFY( q.as_const && !q.as_lvalue );
}

void
test03()
{
  struct F
  {
    int& operator()(int& i, void*) { return i; }
    void* operator()(long, void* p) const { return p; }
  };

  int i = 5;
  void* vp = &vp; // arbitrary void* value
  constexpr F f;

  // Bound arg always forwarded as const int& so can only call second overload:

  auto g0 = bind_front<f>(); // call wrapper has no bound arg
  using G0 = decltype(g0);
  static_assert(is_invocable_r_v<void*, G0&, int, void*>);
  static_assert(is_invocable_r_v<void*, G0&, int&, void*>);
  static_assert(is_invocable_r_v<void*, G0&, const int&, void*>);
  static_assert(is_invocable_r_v<void*, const G0&, int, void*>);
  static_assert(is_invocable_r_v<void*, G0&&, int, void*>);
  void* p0 = static_cast<G0&&>(g0)(i, vp);
  VERIFY( p0 == vp );

  auto g1 = bind_front<f>(i); // call wrapper has bound arg of type int
  using G1 = decltype(g1);
  static_assert(!is_invocable_r_v<int&, G1&, void*>);
  static_assert(is_invocable_r_v<void*, const G1&, void*>);
  static_assert(is_invocable_r_v<void*, G1&&, void*>);
  void* p1 = static_cast<G1&&>(g1)(vp);
  VERIFY( p1 == vp );

  auto g2 = bind_front<f>(std::ref(i)); // bound arg of type int&
  using G2 = decltype(g2);
  static_assert(is_invocable_r_v<void*, G2&, void*>);
  static_assert(is_invocable_r_v<void*, G2&&, void*>);
  static_assert(is_invocable_r_v<void*, const G2&, void*>);
  static_assert(is_invocable_r_v<void*, const G2&&, void*>);
  void* p2 = g2(vp);
  VERIFY( p2 == vp );
  p2 = static_cast<G2&&>(g2)(vp);
  VERIFY( p2 == vp );
  p2 = const_cast<const G2&>(g2)(vp);
  VERIFY( p2 == vp );

  auto g3 = bind_front<f>(std::cref(i)); // bound arg of type const int&
  using G3 = decltype(g3);
  static_assert(is_invocable_r_v<void*, G3&, void*>);
  static_assert(is_invocable_r_v<void*, G3&&, void*>);
  static_assert(is_invocable_r_v<void*, const G3&, void*>);
  static_assert(is_invocable_r_v<void*, const G3&&, void*>);
}

void test03a()
{
  struct F
  {
    int& operator()(int& i, void*) { return i; }
    void* operator()(long, void* p) const { return p; }
  };

  int i = 5;
  void* vp = &vp; // arbitrary void* value
  constexpr F f;

  // Bound arg always forwarded as const int& so can only call second overload:
  auto g1 = bind_front<f>(i); // call wrapper has bound arg of type int
  using G1 = decltype(g1);
  static_assert(is_invocable_r_v<void*, G1&, void*>);
  static_assert(is_invocable_r_v<void*, const G1&, void*>);
  static_assert(is_invocable_r_v<void*, G1&&, void*>);
  void* p1 = static_cast<G1&&>(g1)(vp);
  VERIFY( p1 == vp );
}

constexpr int f(int i, int j, int k) { return i + 2*j + 3*k; }

consteval bool
test04()
{
  constexpr auto g = bind_front<f>();
  VERIFY( std::is_empty_v<decltype(g)> );
  VERIFY( g(1, 2, 3) == 1 + 2*2 + 3*3 );
  constexpr auto g1 = bind_front<f>(1);
  VERIFY( g1(2, 3) == 1 + 2*2 + 3*3 );
  VERIFY( bind_front<g>(1)(2, 3) == 1 + 2*2 + 3*3 );
  constexpr auto g2 = bind_front<f>(1, 2);
  VERIFY( g2(3) == 1 + 2*2 + 3*3 );
  VERIFY( bind_front<g1>(2)(3) == 1 + 2*2 + 3*3 );
  constexpr auto g3 = bind_front<f>(1, 2, 3);
  VERIFY( g3() == 1 + 2*2 + 3*3 );
  VERIFY(bind_front<g2>(3)() == 1 + 2*2 + 3*3 );
  return true;
}

struct C { int i = 0; };
struct D : C { D(){} D(D&&) { ++i; } };
int f5(D const& d1, D const& d2, D const& d3)
{ return d1.i + d2.i + d3.i; }

void test05()
{
  // Must move arguments into capture object, not construct in place
  // like normal arguments.
  VERIFY( bind_front<f5>(D{}, D{})(D{}) == 2 );
}

int
main()
{
  test01();
  test02();
  test02a();
  test03();
  test03a();
  static_assert(test04());
  test05();
}
