// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

#include <functional>
#include <string_view>
#include <testsuite_hooks.h>

using std::copyable_function;

static_assert( !std::is_constructible_v<std::copyable_function<void()>,
					std::copyable_function<void()&>> );
static_assert( !std::is_constructible_v<std::copyable_function<void()>,
					std::copyable_function<void()&&>> );
static_assert( !std::is_constructible_v<std::copyable_function<void()&>,
					std::copyable_function<void()&&>> );
static_assert( !std::is_constructible_v<std::copyable_function<void() const>,
					std::copyable_function<void()>> );

using FuncType = int(int);

// Top level const qualifiers are ignored and decay is performed in parameters
// of function_types.
static_assert( std::is_same_v<std::copyable_function<void(int const)>,
			      std::copyable_function<void(int)>> );
static_assert( std::is_same_v<std::copyable_function<void(int[2])>,
			      std::copyable_function<void(int*)>>);
static_assert( std::is_same_v<std::copyable_function<void(int[])>,
			      std::copyable_function<void(int*)>>);
static_assert( std::is_same_v<std::copyable_function<void(int const[5])>,
			      std::copyable_function<void(int const*)>>);
static_assert( std::is_same_v<std::copyable_function<void(FuncType)>,
			      std::copyable_function<void(FuncType*)>>);

// Non-trivial args, guarantess that type is not passed by copy
struct CountedArg
{
  CountedArg() = default;
  CountedArg(const CountedArg& f) noexcept : counter(f.counter) { ++counter; }
  CountedArg& operator=(CountedArg&&) = delete;

  int counter = 0;
};
CountedArg const c;

// When copyable_function or move_only_function is constructed from other copyable_function,
// the compiler can avoid double indirection per C++26 [func.wrap.general] p2.

void
test01()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::copyable_function<int(CountedArg) const noexcept> c1(f);
  using CF = std::copyable_function<int(CountedArg) const noexcept>;
  VERIFY( c1(c) == 1 );

  std::copyable_function<int(CountedArg) const> c2a(c1);
  VERIFY( c2a(c) == 1 );

  std::copyable_function<int(CountedArg) const> c2b(static_cast<CF>(c1));
  VERIFY( c2b(c) == 1 );

  std::move_only_function<int(CountedArg) const> m2a(c1);
  VERIFY( m2a(c) == 1 );

  std::move_only_function<int(CountedArg) const> m2b(static_cast<CF>(c1));
  VERIFY( m2b(c) == 1 );

  std::copyable_function<int(CountedArg)> c3a(c1);
  VERIFY( c3a(c) == 1 );

  std::copyable_function<int(CountedArg)> c3b(static_cast<CF>(c1));
  VERIFY( c3b(c) == 1 );

  std::move_only_function<int(CountedArg)> m3a(c1);
  VERIFY( m3a(c) == 1 );

  std::move_only_function<int(CountedArg)> m3b(static_cast<CF>(c1));
  VERIFY( m3b(c) == 1 );

  // Invokers internally uses Counted&& for non-trivial types,
  // sinature remain compatible.
  std::copyable_function<int(CountedArg&&)> c4a(c1);
  VERIFY( c4a({}) == 0 );

  std::copyable_function<int(CountedArg&&)> c4b(static_cast<CF>(c1));
  VERIFY( c4b({}) == 0 );

  std::move_only_function<int(CountedArg&&)> m4a(c1);
  VERIFY( m4a({}) == 0 );

  std::move_only_function<int(CountedArg&&)> m4b(static_cast<CF>(c1));
  VERIFY( m4b({}) == 0 );

  std::copyable_function<int(CountedArg&&)&> c5a(c1);
  VERIFY( c5a({}) == 0 );

  std::copyable_function<int(CountedArg&&)&&> c5b(static_cast<CF>(c1));
  VERIFY( std::move(c5b)({}) == 0 );

  std::move_only_function<int(CountedArg&&)&> m5a(c1);
  VERIFY( m5a({}) == 0 );

  std::move_only_function<int(CountedArg&&)&&> m5b(static_cast<CF>(c1));
  VERIFY( std::move(m5b)({}) == 0 );

  // Incompatible signatures
  std::copyable_function<long(CountedArg) const noexcept> c6a(c1);
  VERIFY( c6a(c) == 2 );

  std::copyable_function<long(CountedArg) const noexcept> c6b(static_cast<CF>(c1));
  VERIFY( c6b(c) == 2 );

  std::move_only_function<long(CountedArg) const noexcept> m6a(c1);
  VERIFY( m6a(c) == 2 );

  std::move_only_function<long(CountedArg) const noexcept> m6b(static_cast<CF>(c1));
  VERIFY( m6b(c) == 2 );
}

void
test02()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::copyable_function<int(CountedArg) const noexcept> c1(f);
  using CF = std::copyable_function<int(CountedArg) const noexcept>;
  VERIFY( c1(c) == 1 );

  std::copyable_function<int(CountedArg) const> c2;
  c2 = c1;
  VERIFY( c2(c) == 1 );
  c2 = static_cast<CF>(c1);
  VERIFY( c2(c) == 1 );

  std::move_only_function<int(CountedArg) const> m2;
  m2 = c1;
  VERIFY( m2(c) == 1 );
  m2 = static_cast<CF>(c1);
  VERIFY( m2(c) == 1 );

  // Incompatible signatures
  std::copyable_function<long(CountedArg) const noexcept> c3;
  c3 = c1;
  VERIFY( c3(c) == 2 );
  c3 = static_cast<CF>(c1);
  VERIFY( c3(c) == 2 );

  std::move_only_function<long(CountedArg) const noexcept> m3;
  m3 = c1;
  VERIFY( m3(c) == 2 );
  m3 = static_cast<CF>(c1);
  VERIFY( m3(c) == 2 );
}

void
test03()
{
  std::copyable_function<int(long) const noexcept> c1;
  VERIFY( c1 == nullptr );

  std::copyable_function<int(long) const> c2(c1);
  VERIFY( c2 == nullptr );
  c2 = c1;
  VERIFY( c2 == nullptr );
  c2 = std::move(c1);
  VERIFY( c2 == nullptr );

  std::copyable_function<bool(int) const> c3(std::move(c1));
  VERIFY( c3 == nullptr );
  c3 = c1;
  VERIFY( c3 == nullptr );
  c3 = std::move(c1);
  VERIFY( c3 == nullptr );

  // LWG4255 move_only_function constructor should recognize empty
  //         copyable_functions 
  std::move_only_function<int(long) const noexcept> m1(c1);
  VERIFY( m1 == nullptr );
  m1 = c1;
  VERIFY( m1 == nullptr );
  m1 = std::move(c1);
  VERIFY( m1 == nullptr );

  std::move_only_function<int(long) const> m2(c1);
  VERIFY( m2 == nullptr );
  m2 = c1;
  VERIFY( m2 == nullptr );
  m2 = std::move(c1);
  VERIFY( m2 == nullptr );

  std::move_only_function<bool(int) const> m3(std::move(c1));
  VERIFY( m3 == nullptr );
  m3 = c1;
  VERIFY( m3 == nullptr );
  m3 = std::move(c1);
  VERIFY( m3 == nullptr );
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
  std::copyable_function<int(CountedArg) const> c1(f);
  VERIFY( c1(c) == 1001 );

  // Call const overload as std::copyable_function<int(CountedArg) const>
  // inside td::copyable_function<int(CountedArg)> would do.
  std::copyable_function<int(CountedArg)> c2(c1);
  VERIFY( c2(c) == 1001 );
  std::move_only_function<int(CountedArg)> m2(c1);
  VERIFY( m2(c) == 1001 );

  std::copyable_function<int(CountedArg)> m3(f);
  VERIFY( m3(c) == 1 );
}

void
test05()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::copyable_function<int(CountedArg)> w1(f);
  // copyable_function stores copyable_function due incompatibile signatures
  std::copyable_function<int(CountedArg const&)> w2(std::move(w1));
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
  std::copyable_function<int(CountedArg) const> c1(std::move(f1));
  VERIFY( c1(c) == 2 );

  std::copyable_function<int(CountedArg) const> c2(f);
  std::function<int(CountedArg)> f2(c2);
  VERIFY( f2(c) == 2 );
}

void
test07()
{
  // Scalar types and small trivially move constructible types are passed
  // by value to invoker. So int&& signature is not compatible for such types.
  auto fi = [](CountedArg const& arg, int) noexcept { return arg.counter; };
  std::copyable_function<int(CountedArg, int) const noexcept> ci1(fi);
  VERIFY( ci1(c, 0) == 1 );
  std::copyable_function<int(CountedArg, int&&) const noexcept> ci2(ci1);
  VERIFY( ci2(c, 0) == 2 );

  auto fs = [](CountedArg const& arg, std::string_view) noexcept { return arg.counter; };
  std::copyable_function<int(CountedArg, std::string_view) const noexcept> cs1(fs);
  VERIFY( cs1(c, "") == 1 );
  std::copyable_function<int(CountedArg, std::string_view&&) const noexcept> cs2(cs1);
  VERIFY( cs2(c, "") == 2 );
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
