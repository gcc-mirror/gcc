// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }
#include <regex>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

void
test01()
{
  signed char s[] = { 'a', '.' };
  std::regex re(s, s+2); // This used to fail up to GCC 11.2
  // VERIFY( regex_match("an", re) );

  std::wstring str = L"xx";
  str[0] = '1';
  str[1] = '2';
  re.assign(str.begin(), str.end());
  VERIFY( regex_match("12", re) );
}

void
test02()
{
  int i[] = { 'a', '.', '[', 'x', 'y', 'z', ']' };
  __gnu_test::forward_container<int> fwd(i);
  std::regex re(fwd.begin(), fwd.end());
  VERIFY( regex_match("any", re) );

  __gnu_test::input_container<int> input(i);
  re.assign(input.begin(), input.end(), std::regex::icase);
  VERIFY( regex_match("ANY", re) );
}

int main()
{
  test01();
  test02();
}
