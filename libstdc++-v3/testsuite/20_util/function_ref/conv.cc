// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

#include <functional>
#include <string_view>
#include <testsuite_hooks.h>

using std::function_ref;

static_assert( std::is_constructible_v<std::function_ref<void() const>,
				       std::function_ref<void()>> );

// Non-trivial args, guarantess that type is not passed by copy
struct CountedArg
{
  CountedArg() = default;
  CountedArg(const CountedArg& f) noexcept : counter(f.counter) { ++counter; }
  CountedArg& operator=(CountedArg&&) = delete;

  int counter = 0;
};
CountedArg const c;

using FuncType = int(int);

// Top level const qualifiers are ignored in function types, and decay
// is performed.
static_assert( std::is_same_v<std::function_ref<void(int const)>,
			      std::function_ref<void(int)>> );
static_assert( std::is_same_v<std::function_ref<void(int[2])>,
			      std::function_ref<void(int*)>>);
static_assert( std::is_same_v<std::function_ref<void(int[])>,
			      std::function_ref<void(int*)>>);
static_assert( std::is_same_v<std::function_ref<void(int const[5])>,
			      std::function_ref<void(int const*)>>);
static_assert( std::is_same_v<std::function_ref<void(FuncType)>,
			      std::function_ref<void(FuncType*)>>);

// The C++26 [func.wrap.general] p2 does not currently cover funciton_ref,
// so we make extra copies of arguments.

void
test01()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::function_ref<int(CountedArg) const noexcept> r1(f);
  std::move_only_function<int(CountedArg) const noexcept> m1(f);
  std::copyable_function<int(CountedArg) const noexcept> c1(f);

  // Complatible signatures
  std::function_ref<int(CountedArg) const noexcept> r2m(m1);
  VERIFY( r2m(c) == 2 );
  std::function_ref<int(CountedArg) const noexcept> r2c(c1);
  VERIFY( r2c(c) == 2 );

  std::function_ref<int(CountedArg) const> r3r(r1);
  VERIFY( r3r(c) == 2 );
  std::function_ref<int(CountedArg) const> r3m(m1);
  VERIFY( r3m(c) == 2 );
  std::function_ref<int(CountedArg) const> r3c(c1);
  VERIFY( r3c(c) == 2 );

  std::function_ref<int(CountedArg)> r4r(r1);
  VERIFY( r4r(c) == 2 );
  std::function_ref<int(CountedArg)> r4m(m1);
  VERIFY( r4m(c) == 2 );
  std::function_ref<int(CountedArg)> r4c(c1);
  VERIFY( r4c(c) == 2 );

  // Incompatible signatures
  std::function_ref<long(CountedArg) const noexcept> r5r(r1);
  VERIFY( r5r(c) == 2 );
  std::function_ref<long(CountedArg) const noexcept> r5m(m1);
  VERIFY( r5r(c) == 2 );
  std::function_ref<long(CountedArg) const noexcept> r5c(c1);
  VERIFY( r5r(c) == 2 );
}

void
test02()
{
  // Constructing move_only_function and copyable_function from function_ref,
  // have not chance to restore manager, so we store function_ref inside.
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::function_ref<int(CountedArg) const noexcept> r1(f);

  std::move_only_function<int(CountedArg) const noexcept> m1(r1);
  VERIFY( m1(c) == 2 );

  std::copyable_function<int(CountedArg) const noexcept> c1(r1);
  VERIFY( c1(c) == 2 );
}

void
test03()
{
  struct F
  {
    int operator()(CountedArg const& arg) noexcept
    { return arg.counter; }

    int operator()(CountedArg const& arg) const noexcept
    { return arg.counter + 1000; }
  };

  F f;
  std::function_ref<int(CountedArg) const> r1(f);
  VERIFY( r1(c) == 1001 );

  // Call const overload as std::function_ref<int(CountedArg) const>
  // inside std::function_ref<int(CountedArg)> would do.
  std::function_ref<int(CountedArg)> r2(r1);
  VERIFY( r2(c) == 1002 );
  std::move_only_function<int(CountedArg)> m2(r1);
  VERIFY( m2(c) == 1002 );

  // Call non-const overload as const-qualifed operator() for
  // std::function_ref<int(CountedArg)> do.
  std::function_ref<int(CountedArg)> r3(f);
  VERIFY( r3(c) == 1 );
  std::function_ref<int(CountedArg) const> r4(r3);
  VERIFY( r4(c) == 2 );
  std::move_only_function<int(CountedArg) const> m4(r3);
  VERIFY( m4(c) == 2 );
}

void
test04()
{
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::function_ref<int(CountedArg)> w1(f);
  // function_ref stores function_ref due incompatibile signatures
  std::function_ref<int(CountedArg const&)> w2(std::move(w1));
  // copy is made when passing to int(CountedArg)
  VERIFY( w2(c) == 1 );
  // wrapped 3 times
  std::function_ref<int(CountedArg)> w3(w2);
  VERIFY( w3(c) == 2 );
  // wrapped 4 times
  std::function_ref<int(CountedArg const&)> w4(w3);
  VERIFY( w4(c) == 2 );
  // wrapped 5 times
  std::function_ref<int(CountedArg)> w5(w4);
  VERIFY( w5(c) == 3 );
}

void
test05()
{
  // No special interoperability with std::function
  auto f = [](CountedArg const& arg) noexcept { return arg.counter; };
  std::function<int(CountedArg)> f1(f);
  std::function_ref<int(CountedArg) const> c1(std::move(f1));
  VERIFY( c1(c) == 2 );

  std::function_ref<int(CountedArg) const> c2(f);
  std::function<int(CountedArg)> f2(c2);
  VERIFY( f2(c) == 2 );
}

void
test06()
{
  auto* func = +[]{ static int x; return &x; };
  std::move_only_function<const void*() const> m1(func);
  std::function_ref<const void*() const> rm1(m1);
  VERIFY( m1() == rm1() );
  std::copyable_function<const void*() const> c1(func);
  std::function_ref<const void*() const> rc1(c1);
  VERIFY( c1() == rc1() );

  struct Trivial
  {
    void const* operator()() const
    { return this; }
  };
  std::move_only_function<const void*() const> m2(Trivial{});
  std::function_ref<const void*() const> rm2(m2);
  VERIFY( m2() == rm2() );
  std::copyable_function<const void*() const> c2(Trivial{});
  std::function_ref<const void*() const> rc2(c2);
  VERIFY( c2() == rc2() );

  struct NonTrivial : Trivial
  {
    NonTrivial() {}
    NonTrivial(NonTrivial&&) noexcept {}
    NonTrivial(const NonTrivial&) {}
  };
  std::move_only_function<const void*() const> m3(NonTrivial{});
  std::function_ref<const void*() const> rm3(m3);
  VERIFY( m3() == rm3() );
  std::copyable_function<const void*() const> c3(NonTrivial{});
  std::function_ref<const void*() const> rc3(c3);
  VERIFY( c3() == rc3() );

  struct Large : Trivial
  {
    int tab[10];
  };
  std::move_only_function<const void*() const> m4(Large{});
  std::function_ref<const void*() const> rm4(m4);
  VERIFY( m4() == rm4() );
  std::copyable_function<const void*() const> c4(Large{});
  std::function_ref<const void*() const> rc4(c4);
  VERIFY( c4() == rc4() );
}

void
test07()
{
  int (*f1)() = [] { return 1; };
  int (*f2)() = [] { return 2; };

  std::function_ref<int() const> r1(f1);
  std::move_only_function<int() const> m1(f1);
  std::copyable_function<int() const> c1(f1);

  std::function_ref<int() const> r2r(r1);
  VERIFY( r2r() == 1 );
  r1 = f2;
  VERIFY( r2r() == 1 ); // same-siganture, copy constructor is used
		
  std::function_ref<int() const> r2m(m1);
  VERIFY( r2m() == 1 );
  m1 = f2;
  VERIFY( r2m() == 2 );
			
  std::function_ref<int() const> r2c(c1);
  VERIFY( r2c() == 1 );
  c1 = f2;
  VERIFY( r2c() == 2 );

  std::function_ref<int()> r3r(r1);
  VERIFY( r3r() == 2 );
  r1 = f1;
  VERIFY( r3r() == 1 ); // converting-constructor

  std::function_ref<int()> r3m(m1);
  VERIFY( r3m() == 2 );
  m1 = f1;
  VERIFY( r3m() == 1 );

  std::function_ref<int()> r3c(c1);
  VERIFY( r3c() == 2 );
  c1 = f1;
  VERIFY( r3c() == 1 );

}

constexpr bool
test08()
{
  auto f = [](int x) noexcept { return x; };
  std::function_ref<int(int) const noexcept> rf(f);

  std::function_ref<int(int) const noexcept> rr1(rf);
  std::function_ref<int(int)> rr2(rf);
  std::function_ref<int(long)> rr3(rf);
  return true;
};

void
test09()
{
  // Scalar types and small trivially move constructible types are passed
  // by value to invoker. So int&& signature is not compatible for such types.
  auto fi = [](CountedArg const& arg, int) noexcept { return arg.counter; };
  std::function_ref<int(CountedArg, int) const noexcept> ri1(fi);
  VERIFY( ri1(c, 0) == 1 );
  std::function_ref<int(CountedArg, int&&) const noexcept> ri2(ri1);
  VERIFY( ri2(c, 0) == 2 );

  auto fs = [](CountedArg const& arg, std::string_view) noexcept { return arg.counter; };
  std::function_ref<int(CountedArg, std::string_view) const noexcept> rs1(fs);
  VERIFY( rs1(c, "") == 1 );
  std::function_ref<int(CountedArg, std::string_view&&) const noexcept> rs2(rs1);
  VERIFY( rs2(c, "") == 2 );
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
  test09();

  static_assert( test08() );
}
