// { dg-do compile { target c++17 }  }

#include <optional>
#include <type_traits>

struct C {
  C();
  C(int);
};
C s(10);
const C cs(1);

template<typename T>
using decay_pre26 =
#if __cplusplus > 202302
  T;
#else
  std::decay_t<T>;
#endif

auto lr1 = std::make_optional<C&>(s);        // changed meaning
static_assert( std::is_same_v< decltype(lr1), std::optional<decay_pre26<C&>>> );
auto lr2 = std::make_optional<const C&>(s);  // { dg-error "here" "" { target c++23_down } }
auto lr3 = std::make_optional<C&&>(s);       // { dg-error "no matching function for call" }
auto lr4 = std::make_optional<const C&&>(s); // { dg-error "no matching function for call" }

auto clr1 = std::make_optional<C&>(cs);        // { dg-error "no matching function for call" }
auto clr2 = std::make_optional<const C&>(cs);  // changed meaning
static_assert( std::is_same_v< decltype(clr2), std::optional<decay_pre26<const C&>>> );
auto clr3 = std::make_optional<C&&>(cs);       // { dg-error "no matching function for call" }
auto clr3 = std::make_optional<const C&&>(cs); // { dg-error "no matching function for call" }

// { dg-prune-output "no type named 'type' in 'struct std::enable_if" }
// { dg-prune-output "type/value mismatch at argument 1 in template parameter list" }
// { dg-prune-output "in a union may not have reference type" }
// { dg-prune-output "static assertion failed" }
// { dg-prune-output "forming pointer to reference type" }
// { dg-prune-output "cannot bind .* reference of type" }
// { dg-prune-output "binding reference of type" }
// { dg-prune-output "no matching function for call to `std::optional" }
