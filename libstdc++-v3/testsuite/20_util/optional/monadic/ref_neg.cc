// { dg-do compile { target c++23 } }

#include <optional>

struct S { int x; int y; };

void test()
{
  std::optional<S> o;
  const std::optional<S>& co = o;

  o.transform(&S::x);  // { dg-error "from here" "optional<int&>" { target c++23_down } }
  co.transform(&S::x); // { dg-error "from here" "optional<const int&>" { target c++23_down } }
  std::move(o).transform(&S::x);  // { dg-error "from here" "optional<int&&>" }
  std::move(co).transform(&S::x); // { dg-error "from here" "optional<const int&&>" }
}

// { dg-prune-output "in a union may not have reference type" }
// { dg-prune-output "static assertion failed" }
// { dg-prune-output "forming pointer to reference type" }
