// { dg-do run { target c++23 } }
// { dg-require-effective-target hosted }

#include <functional>
#include <string_view>
#include <testsuite_hooks.h>

using std::move_only_function;

static_assert( !std::is_constructible_v<std::move_only_function<void()>,
					std::move_only_function<void()&>> );
static_assert( !std::is_constructible_v<std::move_only_function<void()>,
					std::move_only_function<void()&&>> );
static_assert( !std::is_constructible_v<std::move_only_function<void()&>,
					std::move_only_function<void()&&>> );
static_assert( !std::is_constructible_v<std::move_only_function<void() const>,
					std::move_only_function<void()>> );

using FuncType = int(int);

// Top level const qualifiers are ignored in function types, and decay
// is performed.
static_assert( std::is_same_v<std::move_only_function<void(int const)>,
			      std::move_only_function<void(int)>> );
static_assert( std::is_same_v<std::move_only_function<void(int[2])>,
			      std::move_only_function<void(int*)>>);
static_assert( std::is_same_v<std::move_only_function<void(int[])>,
			      std::move_only_function<void(int*)>>);
static_assert( std::is_same_v<std::move_only_function<void(int const[5])>,
			      std::move_only_function<void(int const*)>>);
static_assert( std::is_same_v<std::move_only_function<void(FuncType)>,
			      std::move_only_function<void(FuncType*)>>);

// Non-trivial args, guarantess that type is not passed by copy
struct CountedArg
{
  CountedArg() = default;
  CountedArg(const CountedArg& f) noexcept : counter(f.counter) { ++counter; }
  CountedArg& operator=(CountedArg&&) = delete;

  int counter = 0;
};
CountedArg const c;

// When move_only_functions is constructed from other move_only_function,
// the compiler can avoid double indirection per C++26 [func.wrap.general] p2.

void
test01()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::move_only_function<int(CountedArg) const noexcept> m1(f);
  VERIFY( m1(c) == 1 );

  std::move_only_function<int(CountedArg) const> m2(std::move(m1));
  VERIFY( m2(c) == 1 );

  std::move_only_function<int(CountedArg)> m3(std::move(m2));
  VERIFY( m3(c) == 1 );

  // Invokers internally uses Counted&& for non-trivial types,
  // sinature remain compatible.
  std::move_only_function<int(CountedArg&&)> m4(std::move(m3));
  VERIFY( m4({}) == 0 );

  std::move_only_function<int(CountedArg&&)&&> m5(std::move(m4));
  VERIFY( std::move(m5)({}) == 0 );

  m4 = f;
  std::move_only_function<int(CountedArg&&)&> m7(std::move(m4));
  VERIFY( m7({}) == 0 );

  m4 = f;
  std::move_only_function<int(CountedArg&&)&> m8(std::move(m4));
  VERIFY( m8({}) == 0 );

  // Incompatible signatures
  m1 = f;
  std::move_only_function<long(CountedArg) const noexcept> m9(std::move(m1));
  VERIFY( m9(c) == 2 );
}

void
test02()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::move_only_function<int(CountedArg) const noexcept> m1(f);
  VERIFY( m1(c) == 1 );

  std::move_only_function<int(CountedArg) const> m2;
  m2 = std::move(m1);
  VERIFY( m2(c) == 1 );

  std::move_only_function<int(CountedArg)> m3;
  m3 = std::move(m2);
  VERIFY( m3(c) == 1 );

  // Invokers internally uses Counted&& for non-trivial types,
  // sinature remain compatible.
  std::move_only_function<int(CountedArg&&)> m4;
  m4 = std::move(m3);
  VERIFY( m4({}) == 0 );

  std::move_only_function<int(CountedArg&&)&&> m5;
  m5 = std::move(m4);
  VERIFY( std::move(m5)({}) == 0 );

  m4 = f;
  std::move_only_function<int(CountedArg&&)&> m7;
  m7 = std::move(m4);
  VERIFY( m7({}) == 0 );

  m4 = f;
  std::move_only_function<int(CountedArg&&)&> m8;
  m8 = std::move(m4);
  VERIFY( m8({}) == 0 );

  m1 = f;
  std::move_only_function<long(CountedArg) const noexcept> m9;
  m9 = std::move(m1);
  VERIFY( m9(c) == 2 );
}

void
test03()
{
  std::move_only_function<int(long) const noexcept> e;
  VERIFY( e == nullptr );

  std::move_only_function<int(long) const> e2(std::move(e));
  VERIFY( e2 == nullptr );
  e2 = std::move(e);
  VERIFY( e2 == nullptr );

  std::move_only_function<bool(int) const> e3(std::move(e));
  VERIFY( e3 == nullptr );
  e3 = std::move(e);
  VERIFY( e3 == nullptr );
}

void
test04()
{
  struct F
  {
    int operator()(CountedArg const& arg) noexcept
    { return arg.counter; }

    int operator()(CountedArg const& arg) const noexcept
    { return arg.counter + 1000; }
  };

  F f;
  std::move_only_function<int(CountedArg) const> m1(f);
  VERIFY( m1(c) == 1001 );

  // Call const overload as std::move_only_function<int(CountedArg) const>
  // inside std::move_only_function<int(CountedArg)> would do.
  std::move_only_function<int(CountedArg)> m2(std::move(m1));
  VERIFY( m2(c) == 1001 );

  std::move_only_function<int(CountedArg)> m3(f);
  VERIFY( m3(c) == 1 );
}

void
test05()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::move_only_function<int(CountedArg)> w1(f);
  // move_only_function stores move_only_function due incompatibile signatures
  std::move_only_function<int(CountedArg const&)> w2(std::move(w1));
  // copy is made when passing to int(CountedArg)
  VERIFY( w2(c) == 1 );
  // wrapped 3 times
  w1 = std::move(w2);
  VERIFY( w1(c) == 2 );
  // wrapped 4 times
  w2 = std::move(w1);
  VERIFY( w2(c) == 2 );
  // wrapped 5 times
  w1 = std::move(w2);
  VERIFY( w1(c) == 3 );
}

void
test06()
{
  // No special interoperability with std::function
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::function<int(CountedArg)> f1(f);
  std::move_only_function<int(CountedArg) const> m1(std::move(f1));
  VERIFY( m1(c) == 2 );
}

void
test07()
{
  // Scalar types and small trivially move constructible types are passed
  // by value to invoker. So int&& signature is not compatible for such types.
  auto fi = [](CountedArg const& arg, int) noexcept { return arg.counter; };
  std::move_only_function<int(CountedArg, int) const noexcept> mi1(fi);
  VERIFY( mi1(c, 0) == 1 );
  std::move_only_function<int(CountedArg, int&&) const noexcept> mi2(std::move(mi1));
  VERIFY( mi2(c, 0) == 2 );

  auto fs = [](CountedArg const& arg, std::string_view) noexcept { return arg.counter; };
  std::move_only_function<int(CountedArg, std::string_view) const noexcept> ms1(fs);
  VERIFY( ms1(c, "") == 1 );
  std::move_only_function<int(CountedArg, std::string_view&&) const noexcept> ms2(std::move(ms1));
  VERIFY( ms2(c, "") == 2 );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
}
