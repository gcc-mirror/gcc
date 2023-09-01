// { dg-options "-Wnonnull -O0 -Wno-unused-result" }
// { dg-do compile { target c++23 } }

#include <string>

void
test01(const std::string& s)
{
  s.contains((const char*)nullptr);  // { dg-warning "\\\[-Wnonnull" }
  s.contains((char*)nullptr);	     // { dg-warning "\\\[-Wnonnull" }
  s.contains(nullptr);		     // { dg-warning "\\\[-Wnonnull" }
}
