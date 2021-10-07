// { dg-do run { target c++11 } }
#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  std::regex re{""};
  VERIFY( re.flags() & std::regex::ECMAScript );

  std::regex re2{"", std::regex::flag_type{}};
  VERIFY( re2.flags() == std::regex::flag_type() ); // See also PR 83598
}

void
test02()
{
  // A valid value of type syntax_option_type shall have at most one of the
  // grammar elements ECMAScript, basic, extended, awk, grep, egrep, set.

  try
  {
    std::regex{"", std::regex::ECMAScript|std::regex::basic};
    VERIFY( false );
  }
  catch (const std::regex_error&)
  {
  }

  try
  {
    std::regex{"", std::regex::extended|std::regex::basic};
    VERIFY( false );
  }
  catch (const std::regex_error&)
  {
  }

  try
  {
    std::regex{"", std::regex::grep|std::regex::basic};
    VERIFY( false );
  }
  catch (const std::regex_error&)
  {
  }
}

int main()
{
  test01();
  test02();
}
