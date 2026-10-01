// { dg-do run }

// C++98 23.3.5.3 bitset operators

#include <bitset>
#include <sstream>
#include <testsuite_hooks.h>

#pragma GCC diagnostic ignored "-Wc++14-extensions" // 0b literals

// Bug 124370 - Out-of-bounds write for wistream >> bitset
void
test_pr124370()
{
  std::wistringstream input(L"10011011001101");
  std::bitset<10> b;
  input >> b;
  VERIFY( b == std::bitset<10>(0b1001101100) );
}

// Round-trip through a wide stream.
template<int N>
void
test_round_trip()
{
  std::bitset<N> a(0b100110110011010101101010), b;
  std::wstringstream ss;
  ss << a;
  ss >> b;
  VERIFY( b == a );
}

int main()
{
  test_pr124370();
  test_round_trip<0>();
  test_round_trip<24>();
  test_round_trip<1024>();
}
