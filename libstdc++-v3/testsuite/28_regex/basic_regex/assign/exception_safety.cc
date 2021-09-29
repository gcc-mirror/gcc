// { dg-do run { target c++11 } }
#include <regex>
#include <testsuite_hooks.h>

int main()
{
  const auto f = std::regex::ECMAScript|std::regex::icase;
  std::regex re("abc", f);
  try
  {
    re.assign("[", std::regex::extended);
    VERIFY( false );
  }
  catch (const std::regex_error&)
  {
    // [re.regex.assign] "If an exception is thrown, *this is unchanged."
    VERIFY( re.flags() == f );
    VERIFY( std::regex_match("abc", re) );
  }
}
