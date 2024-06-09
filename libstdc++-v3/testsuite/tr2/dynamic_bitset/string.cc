// { dg-do run { target c++11 } }

#include <tr2/dynamic_bitset>
#include <string>
#include <testsuite_hooks.h>

void
test_string_ctor()
{
  std::tr2::dynamic_bitset<> b("101001");
  VERIFY( b[0] == true );
  VERIFY( b[1] == false );
  VERIFY( b[2] == false );
  VERIFY( b[3] == true );
  VERIFY( b[4] == false );
  VERIFY( b[5] == true );
}

void
test_alt_chars()
{
  std::string str = "xOIOIOIOx";
  std::tr2::dynamic_bitset<> b(str, 1, 6, 'I', 'O');
  VERIFY( b[0] == false );
  VERIFY( b[1] == true );
  VERIFY( b[2] == false );
  VERIFY( b[3] == true );
  VERIFY( b[4] == false );
  VERIFY( b[5] == true );
}

int main()
{
  test_string_ctor();
  test_alt_chars();
}
