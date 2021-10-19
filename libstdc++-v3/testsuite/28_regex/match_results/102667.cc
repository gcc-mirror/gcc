// { dg-do run { target c++11 } }

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  std::cmatch sm;
  VERIFY( sm.empty() );
  VERIFY( sm.size() == 0 );
  VERIFY( sm.begin() == sm.end() );  // PR libstdc++/83600

  bool matched = std::regex_match("a", sm, std::regex("b"));
  VERIFY( ! matched );
  VERIFY( sm.ready() );
  VERIFY( sm.empty() );
  VERIFY( sm.size() == 0 );
  VERIFY( sm.begin() == sm.end() ); // PR libstdc++/102667

  matched = std::regex_match("a", sm, std::regex("a"));
  VERIFY( matched );
  VERIFY( sm.ready() );
  VERIFY( ! sm.empty() );
  VERIFY( sm.size() == 1 );
  VERIFY( (sm.end() - sm.begin()) == 1 );

  matched = std::regex_search("abcd", sm, std::regex("(b)(c)"));
  VERIFY( matched );
  VERIFY( sm.ready() );
  VERIFY( ! sm.empty() );
  VERIFY( sm.size() == 3 );
  VERIFY( (sm.end() - sm.begin()) == 3 );
}

int main()
{
  test01();
}
