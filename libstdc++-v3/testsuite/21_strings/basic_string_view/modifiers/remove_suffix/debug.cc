// { dg-do compile { target c++17 } }

#include <string_view>

constexpr bool
check_remove_suffix()
{
  std::string_view sv("123");
  sv.remove_suffix(4);
  // { dg-error "assert_fail" "" { target *-*-* } 0 }
  return true;
}

constexpr bool test = check_remove_suffix();
