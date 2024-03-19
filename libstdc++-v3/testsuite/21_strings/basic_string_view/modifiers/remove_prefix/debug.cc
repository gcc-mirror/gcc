// { dg-do compile { target c++17 } }

#include <string_view>

constexpr bool
check_remove_prefix()
{
  std::string_view sv("123");
  sv.remove_prefix(4);
  // { dg-error "assert_fail" "" { target *-*-* } 0 }
  return true;
}

constexpr bool test = check_remove_prefix();
