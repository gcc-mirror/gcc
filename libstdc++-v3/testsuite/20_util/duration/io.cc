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

void
test_format()
{
  using namespace std::chrono_literals;
  auto s = std::format("{} {}", 1h + 23min + 45s, -42min);
  VERIFY( s == "5025s -42min" );
  s = std::format("{:%j} {:%j} {:%j}", 1h + 23min + 45s, 75h, -99h);
  VERIFY( s == "0 3 -4" );
  s = std::format("{:%T = %H:%M:%S}", 1h + 23min + 45s);
  VERIFY( s == "01:23:45 = 01:23:45" );
  s = std::format("{:%Q} {:%q} {:%Q%q}", 6min + 1s, 44min, -22h);
  VERIFY( s == "361 min -22h" );

  std::wstring ws = std::format(L"{:%Q%q}", 81s);
  VERIFY( ws == L"81s" );

  // Only print '-' on numeric fields for negative durations:
  s = std::format("{:%Q} {:%q} {:%q%Q}", -21h, -20h, -19h);
  VERIFY( s == "-21 h h-19" );
  s = std::format("{:%p} {:%p%H}", -2h, -13h);
  VERIFY( s == "AM PM-13" );
  s = std::format("{:%t} {:%t%M}", -2h, -123s);
  VERIFY( s == "\t \t-02" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "HIjMpqQrRSTX";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(1s));
      // The call above should throw for any conversion-spec not in my_specs:
      VERIFY(my_specs.find(c) != my_specs.npos);
    }
    catch (const std::format_error& e)
    {
      VERIFY(my_specs.find(c) == my_specs.npos);
      std::string_view s = e.what();
      // Libstdc++-specific message:
      VERIFY(s.find("format argument does not contain the information "
		    "required by the chrono-specs") != s.npos);
    }
  }
}

int main()
{
  test01();
  test02();
  test_format();
  // TODO: test_parse();
}
