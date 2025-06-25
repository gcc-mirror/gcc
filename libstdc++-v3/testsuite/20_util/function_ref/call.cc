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

  // Checks if distinction between reference and pointer
  // is preserved.
  struct F
  {
    static
    int operator()(ftype* f, int x)
    { return f(x) + 1000; }

    static
    int operator()(ftype& f, int x)
    { return f(x) + 2000; }
  };
  function_ref<int(int)> r5(nontype<F{}>, &twice);
  VERIFY( r5(2) == 1004 );
  function_ref<int(int)> r6(nontype<F{}>, twice);
  VERIFY( r6(2) == 2008 );
  function_ref<int(int)> r7(nontype<F{}>, &cube);
  VERIFY( r7(3) == 1006 );
  function_ref<int(int)> r8(nontype<F{}>, cube);
  VERIFY( r8(3) == 2027 );
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
  test_params();
}
