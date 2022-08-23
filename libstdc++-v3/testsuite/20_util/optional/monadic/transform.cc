// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <optional>
#include <testsuite_hooks.h>

constexpr bool
test_transform()
{
  std::optional<int> o;
  auto&& r = o.transform([](int) -> unsigned { throw 1; });
  static_assert( std::is_same_v<decltype(r), std::optional<unsigned>&&> );
  VERIFY( ! r.has_value() );

  o = 10;
  auto&& r2 = o.transform([](int i) -> unsigned { return i + 2u; });
  static_assert( std::is_same_v<decltype(r2), std::optional<unsigned>&&> );
  VERIFY( *r2 == 12u );

  return true;
}

static_assert( test_transform() );

enum { CalledLvalue = 1, CalledConst = 2, PassedLvalue = 4, PassedConst = 8 };

struct F
{
  template<typename This, typename Value>
    static constexpr int
    called_as()
    {
      int res = 0;
      if constexpr (std::is_lvalue_reference_v<This>)
	res |= CalledLvalue;
      if constexpr (std::is_const_v<std::remove_reference_t<This>>)
	res |= CalledConst;

      if constexpr (std::is_lvalue_reference_v<Value>)
	res |= PassedLvalue;
      if constexpr (std::is_const_v<std::remove_reference_t<Value>>)
	res |= PassedConst;

      return res;
    }

  template<typename T>
    constexpr int
    operator()(T&&) &
    { return called_as<F&, T>(); }

  template<typename T>
    constexpr int
    operator()(T&&) const &
    { return called_as<const F&, T>(); }

  template<typename T>
    constexpr int
    operator()(T&&) &&
    { return called_as<F, T>(); }

  template<typename T>
    constexpr int
    operator()(T&&) const &&
    { return called_as<const F, T>(); }
};

constexpr bool
test_forwarding()
{
  std::optional<long> o = 1;
  F f;

  VERIFY( *o.transform(f) == (PassedLvalue|CalledLvalue) );
  VERIFY( *o.transform(std::move(f)) == PassedLvalue );
  VERIFY( *std::move(o).transform(f) == CalledLvalue );
  VERIFY( *std::move(o).transform(std::move(f)) == 0 );

  const auto& co = o;

  VERIFY( *co.transform(f) == (PassedLvalue|PassedConst|CalledLvalue) );
  VERIFY( *co.transform(std::move(f)) == (PassedLvalue|PassedConst) );
  VERIFY( *std::move(co).transform(f) == (PassedConst|CalledLvalue) );
  VERIFY( *std::move(co).transform(std::move(f)) == PassedConst );

  const auto& cf = f;

  VERIFY( *o.transform(cf) == (PassedLvalue|CalledLvalue|CalledConst) );
  VERIFY( *o.transform(std::move(cf)) == (PassedLvalue|CalledConst) );
  VERIFY( *std::move(o).transform(cf) == (CalledLvalue|CalledConst) );
  VERIFY( *std::move(o).transform(std::move(cf)) == CalledConst );

  VERIFY( *co.transform(cf) == (PassedLvalue|PassedConst|CalledLvalue|CalledConst) );
  VERIFY( *co.transform(std::move(cf)) == (PassedLvalue|PassedConst|CalledConst) );
  VERIFY( *std::move(co).transform(cf) == (PassedConst|CalledLvalue|CalledConst) );
  VERIFY( *std::move(co).transform(std::move(cf)) == (PassedConst|CalledConst) );

  o = std::nullopt;

  VERIFY( ! o.transform(f).has_value() );
  VERIFY( ! co.transform(f).has_value() );
  VERIFY( ! std::move(o).transform(f).has_value() );
  VERIFY( ! std::move(co).transform(f).has_value() );

  return true;
}

static_assert( test_forwarding() );

constexpr bool
test_copy_elision()
{
  struct immovable
  {
    constexpr immovable(int p) : power_level(p) { }
    immovable(immovable&&) = delete;

    int power_level;
  };

  struct Force
  {
    constexpr immovable operator()(int i) const { return {i+1}; }
  };

  std::optional<int> irresistible(9000);
  std::optional<immovable> object = irresistible.transform(Force{});
  VERIFY( object->power_level > 9000 );

  return true;
}

static_assert( test_copy_elision() );

void f(int&) { }

void
test_unconstrained()
{
  // PR libstdc++/102863 - Optional monadic ops should not be constrained
  std::optional<int> x;
  auto answer = x.transform([](auto& y) { f(y); return 42; });
  VERIFY( !answer );
}

int main()
{
  test_transform();
  test_forwarding();
  test_copy_elision();
  test_unconstrained();
}
