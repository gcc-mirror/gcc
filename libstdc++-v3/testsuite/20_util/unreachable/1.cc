// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <utility>

#ifndef __cpp_lib_unreachable
# error "Feature-test macro for unreachable missing in <utility>"
#elif __cpp_lib_unreachable != 202202L
# error "Feature-test macro for unreachable has wrong value in <utility>"
#endif

bool test01(int i)
{
  if (i == 4)
    return true;
  std::unreachable();
} // { dg-bogus "control reaches end of non-void function" }
