// { dg-do compile { target c++17 }  }

#include <initializer_list>
#include <optional>

struct S { int x; int* p; };
int v;

auto os1 = std::make_optional<S>({1, &v}); // { dg-error "no matching function for" "" { target c++26 } }

struct Cont
{
  Cont();
  Cont(std::initializer_list<int>, int);
};

auto oc1 = std::make_optional<Cont>({}); // { dg-error "no matching function for" "" { target c++26 } }

// { dg-prune-output "no type named 'type' in 'struct std::enable_if" }
// { dg-prune-output "type/value mismatch at argument 1 in template parameter list" }
