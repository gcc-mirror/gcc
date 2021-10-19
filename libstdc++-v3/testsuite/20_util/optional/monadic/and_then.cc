// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <optional>

#ifndef __cpp_lib_monadic_optional
# error "Feature test macro for monadic optional is missing in <optional>"
#elif __cpp_lib_monadic_optional < 202110L
# error "Feature test macro for monadic optional has wrong value in <optional>"
#endif

#include <testsuite_hooks.h>

constexpr bool
test_and_then()
{
  std::optional<int> o;
  auto r = o.and_then([](int) -> std::optional<short> { throw 1; });
  VERIFY( !r.has_value() );
  static_assert( std::is_same_v<decltype(r), std::optional<short>> );

  o = 111;
  r = o.and_then([](int i) -> std::optional<short> { return {i/10}; });
  VERIFY( *r == 11 );

  return true;
}

static_assert( test_and_then() );

enum { CalledLvalue = 1, CalledConst = 2, PassedLvalue = 4, PassedConst = 8 };

struct F
{
  template<typename This, typename Value>
    static constexpr std::optional<int>
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

      return {res};
    }

  template<typename T>
    constexpr std::optional<int>
    operator()(T&&) &
    { return called_as<F&, T>(); }

  template<typename T>
    constexpr std::optional<int>
    operator()(T&&) const &
    { return called_as<const F&, T>(); }

  template<typename T>
    constexpr std::optional<int>
    operator()(T&&) &&
    { return called_as<F, T>(); }

  template<typename T>
    constexpr std::optional<int>
    operator()(T&&) const &&
    { return called_as<const F, T>(); }
};

constexpr bool
test_forwarding()
{
  std::optional<long> o = 1;
  F f;

  VERIFY( *o.and_then(f) == (PassedLvalue|CalledLvalue) );
  VERIFY( *o.and_then(std::move(f)) == PassedLvalue );
  VERIFY( *std::move(o).and_then(f) == CalledLvalue );
  VERIFY( *std::move(o).and_then(std::move(f)) == 0 );

  const auto& co = o;

  VERIFY( *co.and_then(f) == (PassedLvalue|PassedConst|CalledLvalue) );
  VERIFY( *co.and_then(std::move(f)) == (PassedLvalue|PassedConst) );
  VERIFY( *std::move(co).and_then(f) == (PassedConst|CalledLvalue) );
  VERIFY( *std::move(co).and_then(std::move(f)) == PassedConst );

  const auto& cf = f;

  VERIFY( *o.and_then(cf) == (PassedLvalue|CalledLvalue|CalledConst) );
  VERIFY( *o.and_then(std::move(cf)) == (PassedLvalue|CalledConst) );
  VERIFY( *std::move(o).and_then(cf) == (CalledLvalue|CalledConst) );
  VERIFY( *std::move(o).and_then(std::move(cf)) == CalledConst );

  VERIFY( *co.and_then(cf) == (PassedLvalue|PassedConst|CalledLvalue|CalledConst) );
  VERIFY( *co.and_then(std::move(cf)) == (PassedLvalue|PassedConst|CalledConst) );
  VERIFY( *std::move(co).and_then(cf) == (PassedConst|CalledLvalue|CalledConst) );
  VERIFY( *std::move(co).and_then(std::move(cf)) == (PassedConst|CalledConst) );

  o = std::nullopt;

  VERIFY( ! o.and_then(f).has_value() );
  VERIFY( ! co.and_then(f).has_value() );
  VERIFY( ! std::move(o).and_then(f).has_value() );
  VERIFY( ! std::move(co).and_then(f).has_value() );

  return true;
}

static_assert( test_forwarding() );

int main()
{
  test_and_then();
  test_forwarding();
}
