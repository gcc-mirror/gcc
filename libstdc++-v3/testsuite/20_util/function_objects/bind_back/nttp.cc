// { dg-do run { target c++26 } }
// { dg-add-options no_pch }

// Test NTTP bind_back<f>(Args...), P2714

#include <functional>

#ifndef __cpp_lib_bind_back
# error "Feature test macro for bind_back is missing in <functional>"
#elif __cpp_lib_bind_back < 202306L
# error "Feature test macro for bind_back has wrong value in <functional>"
#endif

#include <testsuite_hooks.h>

using std::bind_back;
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
      decltype(bind_back<f>(std::declval<int&>())),
      decltype(bind_back<f>(std::ref(std::declval<int&>())))
      >);
  static_assert(!std::is_same_v<
      decltype(bind_back<f>(std::declval<const int&>())),
      decltype(bind_back<f>(std::cref(std::declval<int&>())))
      >);
  static_assert(!std::is_same_v<
      decltype(bind_back<f>(std::ref(std::declval<int&>()))),
      decltype(bind_back<f>(std::cref(std::declval<int&>())))
      >);
}

struct quals
{
  bool as_const;
  bool as_lvalue;
};

template<typename... Args>
void
testTarget(Args... args)
{
  struct F
  {
    quals operator()(Args...) & { return { false, true }; }
    quals operator()(Args...) const & { return { true, true }; }
    quals operator()(Args...) && { return { false, false }; }
    quals operator()(Args...) const && { return { true, false }; }
  };

  constexpr F f;
  auto g = bind_back<f>(args...);
  const auto& cg = g;
  quals q;

  // template parameter object is always constant lvalue
  q = g();
  VERIFY( q.as_const && q.as_lvalue );
  q = std::move(g)();
  VERIFY( q.as_const && q.as_lvalue );
  q = cg();
  VERIFY( q.as_const && q.as_lvalue );
  q = std::move(cg)();
  VERIFY( q.as_const && q.as_lvalue );
}

template<typename... Args>
void
testBoundArgs(Args... args)
{
  struct F
  {
    quals operator()(Args..., int&) const { return { false, true }; }
    quals operator()(Args..., int const&) const { return { true, true }; }
    quals operator()(Args..., int&&) const { return { false, false }; }
    quals operator()(Args..., int const&&) const { return { true, false }; }
  };

  constexpr F f;
  auto g = bind_back<f>(args..., 10);
  const auto& cg = g;
  quals q;

  // constness and value category should be forwarded to the bound objects:
  q = g();
  VERIFY( ! q.as_const && q.as_lvalue );
  q = std::move(g)();
  VERIFY( ! q.as_const && ! q.as_lvalue );
  q = cg();
  VERIFY( q.as_const && q.as_lvalue );
  q = std::move(cg)();
  VERIFY( q.as_const && ! q.as_lvalue );

  int i = 0;
  auto gr = bind_back<f>(args..., std::ref(i));
  const auto& cgr = gr;

  // bound object is reference wrapper, converts to same type of reference
  q = gr();
  VERIFY( ! q.as_const && q.as_lvalue );
  q = std::move(gr)();
  VERIFY( ! q.as_const && q.as_lvalue );
  q = cgr();
  VERIFY( ! q.as_const && q.as_lvalue );
  q = std::move(cgr)();
  VERIFY( ! q.as_const && q.as_lvalue );

  auto gcr = bind_back<f>(args..., std::cref(i));
  const auto& cgcr = gcr;

  q = gcr();
  VERIFY( q.as_const && q.as_lvalue );
  q = std::move(gcr)();
  VERIFY( q.as_const && q.as_lvalue );
  q = cgcr();
  VERIFY( q.as_const && q.as_lvalue );
  q = std::move(cgcr)();
  VERIFY( q.as_const && q.as_lvalue );
}

template<typename... Args>
void
testCallArgs(Args... args)
{
  struct F
  {
    quals operator()(int&, Args...) const { return { false, true }; }
    quals operator()(int const&, Args...) const { return { true, true }; }
    quals operator()(int&&, Args...) const { return { false, false }; }
    quals operator()(int const&&, Args...) const { return { true, false }; }
  };

  constexpr F f;
  auto g = bind_back<f>(args...);
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

  struct S
  {
    int operator()(long, long, Args...) const { return 1; }
    int operator()(int, void*, Args...) const { return 2; }
  };

  constexpr S s;
  // literal zero can be converted to any pointer, so (int, void*)
  // is best candidate
  VERIFY( s(0, 0, args...) == 2 );
  // both arguments are bound to int&&, and no longer can be
  // converted to pointer, (long, long) is only candidate
  VERIFY( bind_back<s>()(0, 0, args...) == 1 );
  VERIFY( bind_back<s>(args...)(0, 0) == 1 );
}

void
test03()
{
  struct F
  {
    int& operator()(void*, int& i) { return i; }
    void* operator()(void* p, int) const { return p; }
  };

  int i = 5;
  void* vp = &vp; // arbitrary void* value
  constexpr F f;

  // Bound arg always forwarded as const int& so can only call second overload:

  auto g0 = bind_back<f>(); // call wrapper has no bound arg
  using G0 = decltype(g0);
  static_assert(is_invocable_r_v<void*, G0&, void*, int>);
  static_assert(is_invocable_r_v<void*, G0&, void*, int&>);
  static_assert(is_invocable_r_v<void*, G0&, void*, const int&>);
  static_assert(is_invocable_r_v<void*, const G0&, void*, int>);
  static_assert(is_invocable_r_v<void*, G0&&, void*, int>);
  void* p0 = static_cast<G0&&>(g0)(vp, i);
  VERIFY( p0 == vp );

  auto g1 = bind_back<f>(i); // call wrapper has bound arg of type int
  using G1 = decltype(g1);
  static_assert(!is_invocable_r_v<int&, G1&, void*>);
  static_assert(is_invocable_r_v<void*, const G1&, void*>);
  static_assert(is_invocable_r_v<void*, G1&&, void*>);
  void* p1 = static_cast<G1&&>(g1)(vp);
  VERIFY( p1 == vp );

  auto g2 = bind_back<f>(std::ref(i)); // bound arg of type int&
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

  auto g3 = bind_back<f>(std::cref(i)); // bound arg of type const int&
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
    int& operator()(void*, int& i) { return i; }
    void* operator()(void* p, long) const { return p; }
  };

  int i = 5;
  void* vp = &vp; // arbitrary void* value
  constexpr F f;

  // Bound arg always forwarded as const int& so can only call second overload:
  auto g1 = bind_back<f>(i); // call wrapper has bound arg of type int
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
  constexpr auto g = bind_back<f>();
  VERIFY( std::is_empty_v<decltype(g)> );
  VERIFY(g(1, 2, 3) == 1 + 2*2 + 3*3 );
  constexpr auto g1 = bind_back<f>(1);
  VERIFY(g1(2, 3) == 3*1 + 1*2 + 2*3 );
  VERIFY(bind_back<g>(1)(2, 3) == 3*1 + 1*2 + 2*3 );
  constexpr auto g2 = bind_back<f>(1, 2);
  VERIFY(g2(3) == 2*1 + 3*2 + 1*3 );
  VERIFY(bind_back<g1>(2)(3) == 3*1 + 2*2 + 1*3 );
  constexpr auto g3 = bind_back<f>(1, 2, 3);
  VERIFY(g3() == 1 + 2*2 + 3*3);
  VERIFY(bind_back<g2>(3)() == 1*2 + 2*3 + 3*1 );
  return true;
}

struct CountedArg
{
  CountedArg() = default;
  CountedArg(CountedArg&& f) noexcept : counter(f.counter) { ++counter; }
  CountedArg& operator=(CountedArg&&) = delete;

  int counter = 0;
};
CountedArg const c;

void
testMaterialization()
{
  struct F
  {
    int operator()(CountedArg arg, int) const
    { return arg.counter; };
  };

  // CountedArg is bound to rvalue-reference thus moved
  auto f0 = std::bind_back<F{}>();
  VERIFY( f0(CountedArg(), 10) == 1 );

  auto f1 = std::bind_back<F{}>(10);
  VERIFY( f1(CountedArg()) == 1 );
}

int
main()
{
  test01();
  test03();
  test03a();
  static_assert(test04());

  testTarget();
  testTarget(10);
  testTarget(10, 20, 30);

  testBoundArgs();
  testBoundArgs(10);
  testBoundArgs(10, 20, 30);

  testCallArgs();
  testCallArgs(10);
  testCallArgs(10, 20, 30);

  testMaterialization();
}
