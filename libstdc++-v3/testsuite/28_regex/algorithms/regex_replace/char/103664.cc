// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

#include <regex>
#include <testsuite_hooks.h>

int main()
{
  // PR libstdc++/103664
  std::string a = regex_replace("123", std::regex("2"), std::string("a\0b", 3));
  VERIFY( a == std::string("1a\0b3", 5) );
}
