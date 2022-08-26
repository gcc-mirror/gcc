// { dg-options "-std=gnu++23 -Wnonnull -O0 -Wno-unused-result" }
// { dg-do compile { target c++23 } }

#include <string_view>

void
test01(std::string_view s)
{
  s.contains((const char*)nullptr);  // { dg-warning "\\\[-Wnonnull" }
  s.contains((char*)nullptr);	     // { dg-warning "\\\[-Wnonnull" }
  s.contains(nullptr);		     // { dg-warning "\\\[-Wnonnull" }
}
