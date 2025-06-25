// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

#include <functional>
#include <utility>
#include <testsuite_hooks.h>

using std::copyable_function;

using std::is_same_v;
using std::is_invocable_v;
using std::is_nothrow_invocable_v;
using std::invoke_result_t;

// Check return types
static_assert( is_same_v<void, invoke_result_t<copyable_function<void()>>> );
static_assert( is_same_v<int, invoke_result_t<copyable_function<int()>>> );
static_assert( is_same_v<int&, invoke_result_t<copyable_function<int&()>>> );

// With const qualifier
static_assert( ! is_invocable_v< copyable_function<void()> const > );
static_assert( ! is_invocable_v< copyable_function<void()> const &> );
static_assert( is_invocable_v< copyable_function<void() const> > );
static_assert( is_invocable_v< copyable_function<void() const> &> );
static_assert( is_invocable_v< copyable_function<void() const> const > );
static_assert( is_invocable_v< copyable_function<void() const> const &> );

// With no ref-qualifier
static_assert( is_invocable_v< copyable_function<void()> > );
static_assert( is_invocable_v< copyable_function<void()> &> );
static_assert( is_invocable_v< copyable_function<void() const> > );
static_assert( is_invocable_v< copyable_function<void() const> &> );
static_assert( is_invocable_v< copyable_function<void() const> const > );
static_assert( is_invocable_v< copyable_function<void() const> const &> );

// With & ref-qualifier
static_assert( ! is_invocable_v< copyable_function<void()&> > );
static_assert( is_invocable_v< copyable_function<void()&> &> );
static_assert( is_invocable_v< copyable_function<void() const&> > );
static_assert( is_invocable_v< copyable_function<void() const&> &> );
static_assert( is_invocable_v< copyable_function<void() const&> const > );
static_assert( is_invocable_v< copyable_function<void() const&> const &> );

// With && ref-qualifier
static_assert( is_invocable_v< copyable_function<void()&&> > );
static_assert( ! is_invocable_v< copyable_function<void()&&> &> );
static_assert( is_invocable_v< copyable_function<void() const&&> > );
static_assert( ! is_invocable_v< copyable_function<void() const&&> &> );
static_assert( is_invocable_v< copyable_function<void() const&&> const > );
static_assert( ! is_invocable_v< copyable_function<void() const&&> const &> );

// With noexcept-specifier
static_assert( ! is_nothrow_invocable_v< copyable_function<void()> > );
static_assert( ! is_nothrow_invocable_v< copyable_function<void() noexcept(false)> > );
static_assert( is_nothrow_invocable_v< copyable_function<void() noexcept> > );
static_assert( is_nothrow_invocable_v< copyable_function<void()& noexcept>& > );

void
test01()
{
  struct F
  {
    int operator()() { return 0; }
    int operator()() const { return 1; }
  };

  copyable_function<int()> f0{F{}};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  copyable_function<int() const> f1{F{}};
  VERIFY( f1() == 1 );
  VERIFY( std::as_const(f1)() == 1 );
  VERIFY( std::move(f1)() == 1 );
  VERIFY( std::move(std::as_const(f1))() == 1 );

  copyable_function<int()&> f2{F{}};
  VERIFY( f2() == 0 );
  // Not rvalue-callable: std::move(f2)()

  copyable_function<int() const&> f3{F{}};
  VERIFY( f3() == 1 );
  VERIFY( std::as_const(f3)() == 1 );
  VERIFY( std::move(f3)() == 1 );
  VERIFY( std::move(std::as_const(f3))() == 1 );

  copyable_function<int()&&> f4{F{}};
  // Not lvalue-callable: f4()
  VERIFY( std::move(f4)() == 0 );

  copyable_function<int() const&&> f5{F{}};
  // Not lvalue-callable: f5()
  VERIFY( std::move(f5)() == 1 );
  VERIFY( std::move(std::as_const(f5))() == 1 );
}

void
test02()
{
  struct F
  {
    int operator()() & { return 0; }
    int operator()() && { return 1; }
  };

  copyable_function<int()> f0{F{}};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  copyable_function<int()&&> f1{F{}};
  // Not lvalue callable: f1()
  VERIFY( std::move(f1)() == 1 );

  copyable_function<int()&> f2{F{}};
  VERIFY( f2() == 0 );
  // Not rvalue-callable: std::move(f2)()
}

void
test03()
{
  struct F
  {
    int operator()() const & { return 0; }
    int operator()() && { return 1; }
  };

  copyable_function<int()> f0{F{}};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  copyable_function<int()&&> f1{F{}};
  // Not lvalue callable: f1()
  VERIFY( std::move(f1)() == 1 );

  copyable_function<int() const> f2{F{}};
  VERIFY( f2() == 0 );
  VERIFY( std::as_const(f2)() == 0 );
  VERIFY( std::move(f2)() == 0 );
  VERIFY( std::move(std::as_const(f2))() == 0 );

  copyable_function<int() const &&> f3{F{}};
  // Not lvalue callable: f3()
  VERIFY( std::move(f3)() == 0 );
  VERIFY( std::move(std::as_const(f3))() == 0 );

  copyable_function<int() const &> f4{F{}};
  VERIFY( f4() == 0 );
  VERIFY( std::as_const(f4)() == 0 );
  // Not rvalue-callable: std::move(f4)()
}

void
test04()
{
  struct F
  {
    int operator()() & { return 0; }
    int operator()() && { return 1; }
    int operator()() const & { return 2; }
    int operator()() const && { return 3; }
  };

  copyable_function<int()> f0{F{}};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  copyable_function<int()&> f1{F{}};
  VERIFY( f1() == 0 );
  // Not rvalue-callable: std::move(f1)()

  copyable_function<int()&&> f2{F{}};
  // Not lvalue callable: f2()
  VERIFY( std::move(f2)() == 1 );

  copyable_function<int() const> f3{F{}};
  VERIFY( f3() == 2 );
  VERIFY( std::as_const(f3)() == 2 );
  VERIFY( std::move(f3)() == 2 );
  VERIFY( std::move(std::as_const(f3))() == 2 );

  copyable_function<int() const &> f4{F{}};
  VERIFY( f4() == 2 );
  VERIFY( std::as_const(f4)() == 2 );
  // Not rvalue-callable: std::move(f4)()

  copyable_function<int() const &&> f5{F{}};
  // Not lvalue callable: f5()
  VERIFY( std::move(f5)() == 3 );
  VERIFY( std::move(std::as_const(f5))() == 3 );
}

void
test05()
{
  int (*fp)() = [] { return 0; };
  copyable_function<int()> f0{fp};
  VERIFY( f0() == 0 );
  VERIFY( std::move(f0)() == 0 );

  const copyable_function<int() const> f1{fp};
  VERIFY( f1() == 0 );
  VERIFY( std::move(f1)() == 0 );
}

struct Incomplete;
enum CompleteEnum : int;

void
test_params()
{
  std::copyable_function<void(Incomplete&)> f1;
  std::copyable_function<void(Incomplete&&)> f2;
  std::copyable_function<void(CompleteEnum)> f4;
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test_params();
}
