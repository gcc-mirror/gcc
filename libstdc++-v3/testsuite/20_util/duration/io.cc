// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

#ifndef __cpp_lib_char8_t
using char8_t = unsigned char; // Prevent errors if -fno-char8_t is used.
#endif

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

  // LWG 4118. How should duration formatters format custom rep types?
  ss.str("");
  ss << duration<char>(121) << ' ';
  ss << duration<wchar_t>(122) << ' ';
  ss << duration<signed char>(123) << ' ';
  ss << duration<unsigned char>(124) << ' ';
  ss << duration<char8_t>(125) << ' ';
  ss << duration<char16_t>(126) << ' ';
  ss << duration<char32_t>(127) << ' ';
  VERIFY( ss.str() == "121s 122s 123s 124s 125s 126s 127s " );

  ss.str("");
  ss << std::hex << std::uppercase << duration<const char>(0x1A) << ' ';
  ss << std::hex << std::uppercase << duration<const wchar_t>(0x2A) << ' ';
  ss << std::hex << std::uppercase << duration<signed char>(0x3A) << ' ';
  ss << std::scientific << duration<const double>(4.5) << ' ';
  VERIFY( ss.str() == "1As 2As 3As 4.500000E+00s " );
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

  // LWG 4118. How should duration formatters format custom rep types?
  ss.str(L"");
  ss << duration<char>(121) << ' ';
  ss << duration<wchar_t>(122) << ' ';
  ss << duration<signed char>(123) << ' ';
  ss << duration<unsigned char>(124) << ' ';
  ss << duration<char8_t>(125) << ' ';
  ss << duration<char16_t>(126) << ' ';
  ss << duration<char32_t>(127) << ' ';
  VERIFY( ss.str() == L"121s 122s 123s 124s 125s 126s 127s " );

  ss.str(L"");
  ss << std::hex << std::uppercase << duration<const char>(0x1A) << ' ';
  ss << std::hex << std::uppercase << duration<const wchar_t>(0x2A) << ' ';
  ss << std::hex << std::uppercase << duration<signed char>(0x3A) << ' ';
  ss << std::scientific << duration<const double>(4.5) << ' ';
  VERIFY( ss.str() == L"1As 2As 3As 4.500000E+00s " );
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

  // Locale-specific formats:
  s = std::format(std::locale::classic(), "{:%r %OH:%OM:%OS}", 123456ms);
  VERIFY( s == "12:02:03 AM 00:02:03" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "HIjMpqQrRSTX";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      auto s = 1s;
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(s));
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

  std::chrono::duration<float, std::milli> d{0.5};
  s = std::format("{}", d);
  VERIFY( s == "0.5ms" );

  std::chrono::duration<unsigned, std::milli> u{500}; // PR libstdc++/115668
  s = std::format("{}", u);
  VERIFY( s == "500ms" );
  s = std::format("{:%Q %q}", u);
  VERIFY( s == "500 ms" );

  // PR libstdc++/116755 extra minus sign for most negative value
  auto minsec = std::chrono::seconds::min();
  s = std::format("{}", minsec);
  auto expected = std::format("{}s", minsec.count());
  VERIFY( s == expected );
  s = std::format("{:%Q%q}", minsec);
  VERIFY( s == expected );

  // LWG 4118. How should duration formatters format custom rep types?
  s = std::format("{}", std::chrono::duration<char>(100));
  VERIFY( s == "100s" );
  s = std::format("{:%Q}", std::chrono::duration<char>(101));
  VERIFY( s == "101" );
#ifdef _GLIBCXX_USE_WCHAR_T
  ws = std::format(L"{}", std::chrono::duration<char>(102));
  VERIFY( ws == L"102s" );
  ws = std::format(L"{}", std::chrono::duration<wchar_t>(103));
  VERIFY( ws == L"103s" );
#endif
  s = std::format("{}", std::chrono::duration<signed char>(50));
  VERIFY( s == "50s" );
  s = std::format("{:%Q}", std::chrono::duration<signed char>(51));
  VERIFY( s == "51" );
  s = std::format("{}", std::chrono::duration<unsigned char>(52));
  VERIFY( s == "52s" );
  s = std::format("{:%Q}", std::chrono::duration<unsigned char>(53));
  VERIFY( s == "53" );

#if __cplusplus > 202002L
  static_assert( ! std::formattable<std::chrono::duration<wchar_t>, char> );
  static_assert( ! std::formattable<std::chrono::duration<char16_t>, char> );
  static_assert( ! std::formattable<std::chrono::duration<char32_t>, char> );
  static_assert( ! std::formattable<std::chrono::duration<char16_t>, wchar_t> );
  static_assert( ! std::formattable<std::chrono::duration<char32_t>, wchar_t> );
#ifdef __cpp_lib_char8_t
  static_assert( ! std::formattable<std::chrono::duration<char8_t>, char> );
  static_assert( ! std::formattable<std::chrono::duration<char8_t>, wchar_t> );
#endif
#endif
}

void
test_parse()
{
  using namespace std::chrono;
  seconds s;
  milliseconds ms;
  microseconds us;

  std::istringstream is("   2023-07-24 13:05");
  VERIFY( is >> parse(" %Y-%m-%d %H:%M", s) );
  VERIFY( is.good() );
  VERIFY( s == 13h + 5min );

  s = 999s;

  is.clear();
  is.str("Thursday July 2023");
  VERIFY( !(is >> parse("%a %b %C%y", s)) );
  VERIFY( ! is.eof() );
  VERIFY( s == 999s );

  is.clear();
  is.str("27");
  VERIFY( is >> parse("%j", s) );
  VERIFY( is.eof() );
  VERIFY( s == 24h * 27 );

  is.clear();
  is.str("027");
  VERIFY( is >> parse("%j", s) );
  VERIFY( ! is.eof() );
  VERIFY( s == 24h * 27 );

  is.clear();
  is.str("0027");
  VERIFY( is >> parse("%j", s) ); // defaults to %3j
  VERIFY( is.get() == '7' );
  VERIFY( s == 24h * 2 );

  is.clear();
  is.str("1234");
  VERIFY( is >> parse("%2j", s) );
  VERIFY( is.get() == '3' );
  VERIFY( s == 24h * 12 );

  is.clear();
  is.str("001234");
  VERIFY( is >> parse("%4j", s) );
  VERIFY( is.get() == '3' );
  VERIFY( s == 24h * 12 );

  is.clear();
  is.str("1234");
  VERIFY( is >> parse("%4j", s) );
  VERIFY( ! is.eof() );
  VERIFY( s == 24h * 1234 );

  is.clear();
  is.str("125");
  VERIFY( is >> parse("%S", s) );
  VERIFY( s == 12s );
  VERIFY( is.get() == '5' );

  is.clear();
  is.str("0.125");
  VERIFY( is >> parse("%S", s) );
  VERIFY( s == 0s );
  VERIFY( is.get() == '.' );

  is.clear();
  is.str("0.125");
  VERIFY( is >> parse("%S", ms) );
  VERIFY( ms == 125ms );
  VERIFY( ! is.eof() );

  is.clear();
  is.str("00.125");
  VERIFY( is >> parse("%S", ms) );
  VERIFY( ms == 125ms );
  VERIFY( ! is.eof() );

  is.clear();
  is.str("012.345");
  VERIFY( is >> parse("%S", ms) );
  VERIFY( ms == 1000ms );
  VERIFY( is.get() == '2' );

  is.clear();
  is.str("0.1256");
  VERIFY( is >> parse("%S", ms) );
  VERIFY( ms == 125ms );
  VERIFY( is.get() == '6' );

  is.clear();
  is.str("0.0009765");
  VERIFY( is >> parse("%S", us) );
  VERIFY( us == 976us );
  VERIFY( is.get() == '5' );

  is.clear();
  is.str("0.5");
  std::chrono::duration<double> ds;
  VERIFY( is >> parse("%S", ds) );
  VERIFY( ds == 0.5s );

  is.clear();
  is.str("0.125");
  std::chrono::duration<double, std::milli> dms;
  VERIFY( is >> parse("%S", dms) );
  VERIFY( dms == 0.125s );
}

int main()
{
  test01();
  test02();
  test_format();
  test_parse();
}
