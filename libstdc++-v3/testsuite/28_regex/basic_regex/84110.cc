// { dg-do run { target c++11 } }
#include <regex>
#include <string>
#include <testsuite_hooks.h>

void test01()
{
  const std::string s(1ul, '\0');
  std::regex re(s);
  VERIFY( std::regex_match(s, re) ); // PR libstdc++/84110

#if __cpp_exceptions
  using namespace std::regex_constants;
  for (auto syn : {basic, extended, awk, grep, egrep})
  {
    try
    {
      std::regex{s, syn}; // '\0' is not valid for other grammars
      VERIFY( false );
    }
    catch (const std::regex_error&)
    {
    }
  }
#endif
}

void test02()
{
  const std::string s("uh-\0h", 5);
  std::regex re(s);
  VERIFY( std::regex_match(s, re) );
}

int main()
{
  test01();
  test02();
}
