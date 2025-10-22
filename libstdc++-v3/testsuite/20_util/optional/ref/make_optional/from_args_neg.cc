// { dg-do compile { target c++17 }  }

#include <optional>

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

auto z1 = std::make_optional<C&>();        // { dg-error "no matching function for call" }
auto z2 = std::make_optional<const C&>();  // { dg-error "no matching function for call" }
auto z3 = std::make_optional<C&&>();       // { dg-error "no matching function for call" }
auto z4 = std::make_optional<const C&&>(); // { dg-error "no matching function for call" }

auto o1 = std::make_optional<C&>(10);        // { dg-error "no matching function for call" }
auto o2 = std::make_optional<const C&>(10);  // { dg-error "from here" }
auto o3 = std::make_optional<C&&>(10);       // { dg-error "from here" }
auto o4 = std::make_optional<const C&&>(10); // { dg-error "from here" }

auto t1 = std::make_optional<C&>(10, 20);        // { dg-error "no matching function for call" }
auto t2 = std::make_optional<const C&>(10, 20);  // { dg-error "no matching function for call" }
auto t3 = std::make_optional<C&&>(10, 20);       // { dg-error "no matching function for call" }
auto t3 = std::make_optional<const C&&>(10, 20); // { dg-error "no matching function for call" }

// { dg-prune-output "no type named 'type' in 'struct std::enable_if" }
// { dg-prune-output "type/value mismatch at argument 1 in template parameter list" }
// { dg-prune-output "in a union may not have reference type" }
// { dg-prune-output "static assertion failed" }
// { dg-prune-output "forming pointer to reference type" }
// { dg-prune-output "cannot bind .* reference of type" }
// { dg-prune-output "binding reference of type" }
// { dg-prune-output "no matching function for call to 'std::optional" }

