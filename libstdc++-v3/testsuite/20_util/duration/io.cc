// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::chrono;
  std::stringstream ss;
  ss << 0s << '\n';
  ss << 3h + 5min << '\n';
  ss << duration<long, std::ratio<2>>(3) << '\n';
  ss << duration<long, std::ratio<2, 3>>(9) << '\n';
  std::string s;
  std::getline(ss, s);
  VERIFY( s == "0s" );
  std::getline(ss, s);
  VERIFY( s == "185min" );
  std::getline(ss, s);
  VERIFY( s == "3[2]s" );
  std::getline(ss, s);
  VERIFY( s == "9[2/3]s" );
}

void
test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  using namespace std::chrono;
  std::wstringstream ss;
  ss << 0s << L'\n';
  ss << 3h + 5min << L'\n';
  ss << duration<long, std::ratio<2>>(3) << L'\n';
  ss << duration<long, std::ratio<2, 3>>(9) << L'\n';
  std::wstring s;
  std::getline(ss, s);
  VERIFY( s == L"0s" );
  std::getline(ss, s);
  VERIFY( s == L"185min" );
  std::getline(ss, s);
  VERIFY( s == L"3[2]s" );
  std::getline(ss, s);
  VERIFY( s == L"9[2/3]s" );
#endif
}

int main()
{
  test01();
  test02();
}
