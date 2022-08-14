// { dg-options "-std=gnu++20 -Wnonnull -O0 -Wno-unused-result" }
// { dg-do compile { target c++20 } }

#include <testsuite_string.h>

void
test01(const __gnu_test::string& s)
{
  s.ends_with((const char*)nullptr);  // { dg-warning "\\\[-Wnonnull" }
  s.ends_with((char*)nullptr);	      // { dg-warning "\\\[-Wnonnull" }
  s.ends_with(nullptr);		      // { dg-warning "\\\[-Wnonnull" }
}
