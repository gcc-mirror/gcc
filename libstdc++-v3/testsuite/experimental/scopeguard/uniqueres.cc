// { dg-do run { target c++20 } }
// { dg-add-options no_pch }

#include <experimental/scope>

#ifndef __cpp_lib_experimental_scope
# error Feature-test macro is not defined.
#elif __cpp_lib_experimental_scope < 201902
# error Feature-test macro has bad value.
#endif

#include <testsuite_hooks.h>

using std::experimental::unique_resource;

void
test_default_cons()
{
  struct val { int i; };

  struct del
  {
    void operator()(val) const { VERIFY(false); }
    int i;
  };

  static_assert( std::is_default_constructible_v<unique_resource<val, del>> );
  static_assert( !std::is_default_constructible_v<unique_resource<val&, del>> );
  // GCC extension:
  static_assert( std::is_nothrow_default_constructible_v<unique_resource<val, del>> );
  struct exval : val { exval() noexcept(false) { } };
  static_assert( !std::is_nothrow_default_constructible_v<unique_resource<exval, del>> );

  unique_resource<val, del> res;
  VERIFY( res.get().i == 0 ); // value-initialized
  VERIFY( res.get_deleter().i == 0 ); // value-initialized
}

void
test_cons()
{
  struct val { int i; };

  struct del
  {
    void operator()(val v) const { VERIFY(v.i == i); }
    int i;
  };

  auto r1 = unique_resource<val, del>(val{}, del{});
  VERIFY( r1.get().i == 0 );
  VERIFY( r1.get_deleter().i == 0 );

  auto r2 = unique_resource<val, del>(1, 2);
  VERIFY( r2.get().i == 1 );
  VERIFY( r2.get_deleter().i == 2 );
  r2.release();

  val v{3};
  auto r3 = unique_resource<val&, del>(v, 3);

  static_assert( !std::is_constructible_v<unique_resource<val&, del>, val, del> );
  static_assert( !std::is_constructible_v<unique_resource<val&, del>, int, del> );
  static_assert( !std::is_constructible_v<unique_resource<val&, del>, const val&, del> );

  del d4{4};
  auto r4 = unique_resource(std::ref(v), std::ref(d4));
  --d4.i;

  static_assert( std::is_same_v<decltype(r4),
				unique_resource<std::reference_wrapper<val>,
						std::reference_wrapper<del>>> );
  static_assert( !std::is_constructible_v<decltype(r4), val, del> );

  int counter = 0, dcounter = 99;
  {
    unique_resource r(std::ref(counter),
		      [&dcounter] (int& i) { ++dcounter; ++i; });
  }
  VERIFY( counter == 1 );
  VERIFY( dcounter == 100 );

  {
    struct NothrowMove
    {
      NothrowMove() noexcept { }
      NothrowMove(NothrowMove&&) noexcept(true) { }
      NothrowMove(const NothrowMove&) { throw 1; }
    };

    unique_resource r(NothrowMove{},
		      [&dcounter] (NothrowMove&) { ++dcounter; });
  }
  VERIFY( dcounter == 101 );

  {
    struct ThrowOnCopy
    {
      ThrowOnCopy() noexcept { }
      ThrowOnCopy(ThrowOnCopy&&) noexcept(false) { VERIFY(false); };
      ThrowOnCopy(const ThrowOnCopy&) { throw 1; }
      explicit ThrowOnCopy(val) noexcept(false) { VERIFY(false); }
      explicit ThrowOnCopy(val&) noexcept(false) { }
    };
    auto d = [&dcounter] (auto&) { ++dcounter; };

    unique_resource r(val(1), d); // uses ThrowOnCopy(val&)

    try {
      unique_resource r(ThrowOnCopy{}, d); // uses copy constructor
      VERIFY( false );
    } catch (int) {
      VERIFY( dcounter == 102 );
    }
  }
  VERIFY( dcounter == 103 );

  {
    struct CopyVal
    {
      explicit CopyVal(const val& v) : i(v.i) { }
      int i;
    };

    struct Del
    {
      void operator()(const val&) { VERIFY(false); }
      void operator()(const CopyVal& c) { ref = c.i; }
      int& ref;
    };

    struct CopyDel
    {
      explicit CopyDel(Del&&) noexcept(false) { VERIFY(false); }
      explicit CopyDel(const Del&) noexcept(false) { throw 1; }
      void operator()(const val&) = delete;
      void operator()(const CopyVal&) { VERIFY(false); }
    };

    try {
      // CopyVal is constructed from val(11), then initializing CopyDel throws.
      // The CopyVal member is passed to the Del argument to be freed.
      unique_resource<CopyVal, CopyDel> r(val(11), Del{dcounter});
      VERIFY( false );
    } catch (int) {
      VERIFY( dcounter == 11 );
    }
  }
}

void
test_move_cons()
{
  {
    struct Del
    {
      void operator()(int) const { VERIFY(false); }
    };

    unique_resource<int, Del> r0;
    auto rr0 = std::move(r0);
    VERIFY( r0.get() == 0 );
    VERIFY( rr0.get() == 0 );

    struct DelThrowingCopy
    {
      DelThrowingCopy() = default;
      DelThrowingCopy(const DelThrowingCopy&) { throw 1; }
      void operator()(int) const { VERIFY(false); }
    };

    unique_resource<int, DelThrowingCopy> r1;
    try {
      auto rr1 = std::move(r1); // Initializing deleter throws.
      VERIFY( false );
    } catch (int) {
    }
  }

  {
    struct Res
    {
      Res() = default;
      Res(Res&& r) noexcept : moved(r.moved) { r.moved = true; }
      Res(Res& r) : moved(r.moved) { }
      bool moved = false;
    };

    unique_resource r(Res{}, [](const auto&) { });
    auto rr = std::move(r);
    VERIFY( r.get().moved == true );
    VERIFY( rr.get().moved == false );
  }

  {
    struct Res2
    {
      Res2() = default;
      Res2(Res2&& r) noexcept(false) : moved(r.moved) { r.moved = false; }
      Res2(Res2& r) : moved(r.moved) { }
      bool moved = false;
    };

    unique_resource r2(Res2{}, [](const auto&) { });
    auto rr2 = std::move(r2);
    VERIFY( r2.get().moved == false );
    VERIFY( rr2.get().moved == false );
  }

  {
    struct ThrowingCopy
    {
      ThrowingCopy(int) { }
      ThrowingCopy(const ThrowingCopy&) { throw 1; }
    };

    int dcounter = 0;
    {
      auto d = [&dcounter] (const auto&) { ++dcounter; };
      unique_resource<ThrowingCopy, decltype(d)> r(1, d);
      try {
	auto rr = std::move(r); // Ownership of resource left with 'r'
	VERIFY(false);
      } catch (int) {
	VERIFY( dcounter == 0 );
      }
    }
    VERIFY( dcounter == 1 );
  }
}

int called1 = 0;

void
test_assign()
{
  struct ThrowingDel
  {
    ThrowingDel() = default;
    ThrowingDel(int& called) : called(called) { }
    ThrowingDel(const ThrowingDel&) = default;
    ThrowingDel& operator=(const ThrowingDel&) { throw 1; }

    void operator()(int i) const noexcept { ++called; }
    int& called = called1;
  };

  int called2 = 0;
  {
    unique_resource<int, ThrowingDel> r1;
    VERIFY( r1.get() == 0 );
    unique_resource<int, ThrowingDel> r2(2, ThrowingDel{called2});
    VERIFY( r2.get() == 2 );
    try
    {
      r1 = std::move(r2);
      VERIFY( false );
    }
    catch (int)
    {
    }
    VERIFY( called1 == 0 ); // r1.reset() was called, but did nothing.
    VERIFY( called2 == 0 ); // r2.reset() not called.
    VERIFY( r1.get() == 0 );
    VERIFY( r2.get() == 2 );
  }
  VERIFY( called1 == 0 ); // r1.reset() was called, but did nothing.
  VERIFY( called2 == 1 ); // r2 destructor invoked its deleter.
}

void
test_modifiers()
{
  int dcounter = 0;
  auto d = [&dcounter] (int i) { dcounter += i; };
  unique_resource<int, decltype(d)> r(1, d);
  r.reset();
  VERIFY( dcounter == 1 );
  r.reset(2);
  VERIFY( dcounter == 1 );
  r.release();
  VERIFY( dcounter == 1 );
  r.release();
  VERIFY( dcounter == 1 );
  r.reset(3);
  VERIFY( dcounter == 1 );
  r.reset(4);
  VERIFY( dcounter == 4 );
}

template<typename T> concept has_star = requires (T& t) { *t; };
template<typename T> concept has_arrow = requires (T& t) { t.operator->(); };

void
test_observers()
{
  struct D { void operator()(int* p) const noexcept { delete p; } };
  int* p = new int(3);
  unique_resource<int*, D> r(p, D{});
  VERIFY( r.get() == p );
  VERIFY( *r == 3 );
  VERIFY( r.operator->() == p );
  (void) r.get_deleter();

  using R1 = unique_resource<int, void(*)(int)>;
  static_assert( ! has_star<R1> );
  static_assert( ! has_arrow<R1> );
  using R2 = unique_resource<const void*, void(*)(const void*)>;
  static_assert( ! has_star<R2> );
  static_assert( has_arrow<R2> );
}

void
test_make_checked()
{
  struct Boolish {
    explicit operator bool() const noexcept { return val; }
    bool val;
  };

  using std::experimental::make_unique_resource_checked;

  {
    struct ThrowingCopy
    {
      ThrowingCopy(int i) : val(i) { }
      ThrowingCopy(const ThrowingCopy&) { throw 1; }
      Boolish operator==(int i) const noexcept { return {i == val}; }
      int val;
    };

    int dcounter = 0;
    auto d = [&dcounter] (const auto&) { ++dcounter; };

    try
    {
      (void) make_unique_resource_checked(ThrowingCopy(1), 0, d);
      VERIFY(false);
    }
    catch (int)
    {
      VERIFY(dcounter == 1);
    }

    dcounter = 0;
    try
    {
      (void) make_unique_resource_checked(ThrowingCopy(1), 1, d);
      VERIFY(false);
    }
    catch (int)
    {
      VERIFY(dcounter == 0);
    }
  }
}

int main()
{
  test_default_cons();
  test_cons();
  test_move_cons();
  test_assign();
  test_modifiers();
  test_observers();
  test_make_checked();
}
