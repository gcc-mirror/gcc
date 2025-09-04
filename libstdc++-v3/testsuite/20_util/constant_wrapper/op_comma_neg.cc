// { dg-do compile { target c++26 } }
#include <type_traits>

constexpr void
test_comma_same_types()
{
  (std::cw<1>, std::cw<2>); // { dg-error "use of deleted function" }
}

constexpr void
test_comma_different_types()
{
  (std::cw<1>, std::cw<2.0>); // { dg-error "use of deleted function" }
}
