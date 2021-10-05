// { dg-options "-std=gnu++23 -fno-inline" }
// { dg-do run { target c++23 } }

#include <sstream>
#include <testsuite_hooks.h>

int main()
{
  int i = 0;
  volatile void* vp = &i;
  std::ostringstream s1, s2;
  s1 << &i;
  s2 << vp;
  VERIFY( s1.str() == s2.str() );
}
