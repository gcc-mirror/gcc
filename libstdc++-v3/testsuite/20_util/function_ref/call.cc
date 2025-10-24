// { dg-do run { target c++26 } }

#include <functional>
#include <utility>
#include <testsuite_hooks.h>

using std::nontype;
using std::function_ref;

using std::is_same_v;
using std::is_invocable_v;
using std::is_nothrow_invocable_v;
using std::invoke_result_t;

// Check return types
static_assert( is_same_v<void, invoke_result_t<function_ref<void()>>> );
static_assert( is_same_v<int, invoke_result_t<function_ref<int()>>> );
static_assert( is_same_v<int&, invoke_result_t<function_ref<int&()>>> );

// Const qualier applies to target object
static_assert( is_invocable_v< function_ref<void()> const > );
static_assert( is_invocable_v< function_ref<void()> const &> );
static_assert( is_invocable_v< function_ref<void() const> > );
static_assert( is_invocable_v< function_ref<void() const> &> );
static_assert( is_invocable_v< function_ref<void() const> const > );
static_assert( is_invocable_v< function_ref<void() const> const &> );

// With noexcept-specifier
static_assert( ! is_nothrow_invocable_v< function_ref<void()> > );
static_assert( ! is_nothrow_invocable_v< function_ref<void() noexcept(false)> > );
static_assert( is_nothrow_invocable_v< function_ref<void() noexcept> > );

void
test01()
{
  struct F
  {
    int operator()() { return 0; }
    int operator()() const { return 1; }
  };

  function_ref<int()> f0{F{}};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  function_ref<int()> f1{nontype<F{}>};
  VERIFY( f1() == 1 );
  VERIFY( std::move(f1)() == 1 );

  function_ref<int() const> f2{F{}};
  VERIFY( f2() == 1 );
  VERIFY( std::as_const(f2)() == 1 );
  VERIFY( std::move(f2)() == 1 );
  VERIFY( std::move(std::as_const(f2))() == 1 );

  function_ref<int() const> f3{nontype<F{}>};
  VERIFY( f3() == 1 );
  VERIFY( std::as_const(f3)() == 1 );
  VERIFY( std::move(f3)() == 1 );
  VERIFY( std::move(std::as_const(f3))() == 1 );
}

void
test02()
{
  struct F
  {
    struct Arg {};
    int operator()(Arg& arg) const { return 0; }
    int operator()(const Arg& arg) const { return 1; }
  };
  F::Arg arg;

  function_ref<int()> f0{std::nontype<F{}>, arg};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  function_ref<int() const> f1{std::nontype<F{}>, arg};
  VERIFY( f1() == 1 );
  VERIFY( std::as_const(f1)() == 1 );
}

void
test03()
{
  struct F
  {
    struct Arg {};
    int operator()(Arg* arg) const { return 0; }
    int operator()(const Arg* arg) const { return 1; }
  };
  F::Arg arg;

  function_ref<int()> f0{std::nontype<F{}>, &arg};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  function_ref<int() const> f1{std::nontype<F{}>, &arg};
  VERIFY( f1() == 1 );
  VERIFY( std::as_const(f1)() == 1 );
}

void
test04()
{
  constexpr int (*fp)() = [] { return 0; };
  function_ref<int()> f0{fp};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  function_ref<int()> f1{nontype<fp>};
  VERIFY( f1() == 0 );
  VERIFY( std::move(f1)() == 0 );

  const function_ref<int() const> f2{fp};
  VERIFY( f2() == 0 );
  VERIFY( std::move(f2)() == 0 );

  const function_ref<int() const> f3{nontype<fp>};
  VERIFY( f2() == 0 );
  VERIFY( std::move(f2)() == 0 );
}

using ftype = int(int);
int twice(int x) { return x * 2; }
int cube(int x) { return x * x * x; }
int callback_ptr(ftype* f, int x) { return f(x); }
int callback_ref(ftype& f, int x) { return f(x); }

void
test05()
{
  function_ref<int(int)> r1(nontype<&callback_ptr>, &twice);
  VERIFY( r1(2) == 4 );
  function_ref<int(int)> r2(nontype<&callback_ptr>, cube);
  VERIFY( r2(2) == 8 );

  function_ref<int(int)> r3(nontype<&callback_ref>, twice);
  VERIFY( r3(3) == 6 );
  function_ref<int(int)> r4(nontype<&callback_ref>, cube);
  VERIFY( r4(3) == 27 );
}

void
test06()
{
  struct S { 
   int v;
   int& m() { return v; }
   const int& c() const { return v; }

   int& operator()(int) { return v; }
   int const& operator()(int, int) const { return v; }
  };
  S s{10};
  std::reference_wrapper<S> sr(s);
  std::reference_wrapper<const S> csr(s);

  std::function_ref<int&(int)> e1(sr);
  VERIFY( &e1(0) == &s.v );
  std::function_ref<int&(int) const> e2(sr);
  VERIFY( &e2(0) == &s.v );
  std::function_ref<int&(int) const> e3(std::as_const(sr));
  VERIFY( &e3(0) == &s.v );

  std::function_ref<const int&(int, int)> e4(sr);
  VERIFY( &e4(0, 0) == &s.v );
  std::function_ref<const int&(int, int) const> e5(sr);
  VERIFY( &e5(0, 0) == &s.v );
  std::function_ref<const int&(int, int)> e6(csr);
  VERIFY( &e6(0, 0) == &s.v );
  std::function_ref<const int&(int, int) const> e7(csr);
  VERIFY( &e7(0, 0) == &s.v );
  std::function_ref<const int&(int, int) const> e8(std::as_const(csr));
  VERIFY( &e8(0, 0) == &s.v );

  std::function_ref<int&()> f1(std::nontype<&S::v>, sr);
  VERIFY( &f1() == &s.v );
  std::function_ref<const int&()> f2(std::nontype<&S::v>, sr);
  VERIFY( &f2() == &s.v );
  std::function_ref<int&()> f3(std::nontype<&S::m>, sr);
  VERIFY( &f3() == &s.v );
  std::function_ref<const int&()> f4(std::nontype<&S::c>, sr);
  VERIFY( &f4() == &s.v );

  std::function_ref<const int&()> f5(std::nontype<&S::v>, csr);
  VERIFY( &f5() == &s.v );
  std::function_ref<const int&()> f6(std::nontype<&S::c>, sr);
  VERIFY( &f6() == &s.v );
  static_assert( !std::is_constructible_v<
    std::function_ref<int&()>,
    std::nontype_t<&S::c>, std::reference_wrapper<S>&>
   );

  std::function_ref<int&()> f7(std::nontype<&S::v>, std::as_const(sr));
  VERIFY( &f7() == &s.v );
  std::function_ref<const int&()> f8(std::nontype<&S::m>, std::as_const(sr));
  VERIFY( &f8() == &s.v );

  // No rvalue reference_wrapper support
  static_assert( !std::is_constructible_v<
    std::function_ref<int&()>,
    std::nontype_t<&S::v>, std::reference_wrapper<S>>
  );
  static_assert( !std::is_constructible_v<
    std::function_ref<int&()>,
    std::nontype_t<&S::v>, std::reference_wrapper<const S>>
  );

  // reference to reference_wrapper are bound, so mutation are visible
  S s2{20};
  sr = s2;
  csr = s2;
  VERIFY( &e1(0) == &s2.v );
  VERIFY( &e2(0) == &s2.v );
  VERIFY( &e3(0) == &s2.v );
  VERIFY( &e4(0, 0) == &s2.v );
  VERIFY( &e5(0, 0) == &s2.v );
  VERIFY( &e6(0, 0) == &s2.v );
  VERIFY( &e7(0, 0) == &s2.v );
  VERIFY( &e8(0, 0) == &s2.v );
  VERIFY( &f1() == &s2.v );
  VERIFY( &f2() == &s2.v );
  VERIFY( &f3() == &s2.v );
  VERIFY( &f4() == &s2.v );
  VERIFY( &f5() == &s2.v );
  VERIFY( &f6() == &s2.v );
  VERIFY( &f7() == &s2.v );
  VERIFY( &f8() == &s2.v );

  constexpr auto id = []<typename T>(const std::reference_wrapper<T>& x)
  { return &x; };

  // identity of reference_wrapper is preserved
  std::function_ref<const std::reference_wrapper<S>*()> g1(std::nontype<id>, sr);
  VERIFY( g1() == &sr );
  std::function_ref<const std::reference_wrapper<const S>*()> g2(std::nontype<id>, csr);
  VERIFY( g2() == &csr );
}

struct Incomplete;
enum CompleteEnum : int;

void
test_params()
{
  auto f = [](auto&&) {};
  std::function_ref<void(Incomplete&)> f1(f);
  // See PR libstdc++/120259, this should be supported.
  // std::function_ref<void(Incomplete&&)> f2(f);
  std::function_ref<void(CompleteEnum)> f3(f);
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test_params();
}
