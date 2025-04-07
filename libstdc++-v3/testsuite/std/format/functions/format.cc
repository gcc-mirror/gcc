// { dg-options "-fexec-charset=UTF-8" }
// { dg-do run { target c++20 } }
// { dg-add-options no_pch }

#include <format>

#ifndef __cpp_lib_format
# error "Feature test macro for std::format is missing in <format>"
#elif __cpp_lib_format < 202110L
# error "Feature test macro for std::format has wrong value in <format>"
#elif __cplusplus > 202302L && __cpp_lib_format < 202311L
# error "Feature test macro for std::format has wrong value in <format>"
#endif

#ifndef __cpp_lib_format_uchar
# error "Feature test macro for formatting chars as integers is missing in <format>"
#elif __cpp_lib_format_uchar < 202311L
# error "Feature test macro for formatting chars as integers has wrong value in <format>"
#endif

#undef __cpp_lib_format
#include <version>
#ifndef __cpp_lib_format
# error "Feature test macro for std::format is missing in <version>"
#elif __cpp_lib_format < 202110L
# error "Feature test macro for std::format has wrong value in <version>"
#elif __cplusplus > 202302L && __cpp_lib_format < 202311L
# error "Feature test macro for std::format has wrong value in <version>"
#endif

#ifndef __cpp_lib_format_uchar
# error "Feature test macro for formatting chars as integers is missing in <version>"
#elif __cpp_lib_format_uchar < 202311L
# error "Feature test macro for formatting chars as integers has wrong value in <version>"
#endif

#include <string>
#include <limits>
#include <cstdint>
#include <cstdio>
#include <testsuite_hooks.h>

void
test_no_args()
{
  std::string s;
  s = std::format("disco");
  VERIFY( s == "disco" );

  s = std::format("}} machine {{ funk }} specialists {{");
  VERIFY( s == "} machine { funk } specialists {" );

  s = std::format("128bpm }}");
  VERIFY( s == "128bpm }" );
}

void
test_unescaped()
{
#ifdef __cpp_exceptions
  for (auto f : { "{", "}", "{{{", "{{}", "}{", "{{{{{" })
    try {
      (void) std::vformat(f, std::make_format_args());
      VERIFY( false );
    } catch (const std::format_error& e) {
      std::string what = e.what();
      VERIFY( what.find("unmatched") != what.npos );
    }
#endif
}

struct brit_punc : std::numpunct<char>
{
  std::string do_grouping() const override { return "\3\3"; }
  char do_thousands_sep() const override { return ','; }
  std::string do_truename() const override { return "yes mate"; }
  std::string do_falsename() const override { return "nah bruv"; }
};

void
test_std_examples()
{
  using namespace std;

  string s = format("{0}-{{", 8); // value of s is "8-{"
  VERIFY( s == "8-{" );

  // align
  {
    char c = 120;
    string s0 = format("{:6}", 42);
    VERIFY(s0 == "    42");
    string s1 = format("{:6}", 'x');
    VERIFY(s1 == "x     ");
    string s2 = format("{:*<6}", 'x');
    VERIFY(s2 == "x*****");
    string s3 = format("{:*>6}", 'x');
    VERIFY(s3 == "*****x");
    string s4 = format("{:*^6}", 'x');
    VERIFY(s4 == "**x***");
    string s5 = format("{:6d}", c);
    VERIFY(s5 == "   120");
    string s6 = format("{:6}", true);
    VERIFY(s6 == "true  ");
    string s7 = format("{:*<6.3}", "123456");
    VERIFY( s7 == "123***" );
    string s8 = format("{:02}", 1234);
    VERIFY( s8 == "1234" );
    string s9 = format("{:*<}", "12");
    VERIFY( s9 == "12" );
    string sA = format("{:*<6}", "12345678");
    VERIFY( sA == "12345678" );
    string sB = format("{:🤡^6}", "x");
    VERIFY( sB == "🤡🤡x🤡🤡🤡" );
    string sC = format("{:*^6}", "🤡🤡🤡");
    VERIFY( sC == "🤡🤡🤡" );
  }

  // sign
  {
    double inf = numeric_limits<double>::infinity();
    double nan = numeric_limits<double>::quiet_NaN();
    string s0 = format("{0:},{0:+},{0:-},{0: }", 1);
    VERIFY(s0 == "1,+1,1, 1");
    string s1 = format("{0:},{0:+},{0:-},{0: }", -1);
    VERIFY(s1 == "-1,-1,-1,-1");
    string s2 = format("{0:},{0:+},{0:-},{0: }", inf);
    VERIFY(s2 == "inf,+inf,inf, inf");
    string s3 = format("{0:},{0:+},{0:-},{0: }", nan);
    VERIFY(s3 == "nan,+nan,nan, nan");
  }

  // alternate form and zero fill
  {
    char c = 120;
    string s1 = format("{:+06d}", c);
    VERIFY(s1 == "+00120");
    string s2 = format("{:#06x}", 0xa);
    VERIFY(s2 == "0x000a");
    string s3 = format("{:<06}", -42);
    VERIFY(s3 == "-42   "); // 0 is ignored because of < alignment
  }

  // integer presentation types
  {
    // Change global locale so "{:L}" adds digit separators.
    std::locale::global(std::locale({}, new brit_punc));

    string s0 = format("{}", 42);
    VERIFY(s0 == "42");
    string s1 = format("{0:b} {0:d} {0:o} {0:x}", 42);
    VERIFY(s1 == "101010 42 52 2a");
    string s2 = format("{0:#x} {0:#X}", 42);
    VERIFY(s2 == "0x2a 0X2A");
    string s3 = format("{:L}", 1234);
    VERIFY(s3 == "1,234");

    // Test locale's "byte-and-a-half" grouping (Imperial word? tribble?).
    string s4 = format("{:#Lx}", 0xfffff);
    VERIFY(s4 == "0xff,fff");

    // Restore
    std::locale::global(std::locale::classic());

    string s5 = format("{}", -100); // PR libstdc++/114325
    VERIFY(s5 == "-100");
    string s6 = format("{:d} {:d}", -123, 999);
    VERIFY(s6 == "-123 999");
  }
}

void
test_alternate_forms()
{
  std::string s;

  s = std::format("{0:#b} {0:+#B} {0:#o} {0:#x} {0:+#X} {0: #d}", 42);
  VERIFY( s == "0b101010 +0B101010 052 0x2a +0X2A  42" );
  s = std::format("{0:#b} {0:+#B} {0:#o} {0:#x} {0:+#X} {0: #d}", 0);
  VERIFY( s == "0b0 +0B0 0 0x0 +0X0  0" );

  s = std::format("{0:+#012g} {0:+#014g} {0:+#014g}", 1234.0);
  VERIFY( s == "+00001234.00 +0000001234.00 +0000001234.00" );
  s = std::format("{0:+#0{1}g} {0:+#0{2}g} {0:+#0{2}g}", 1234.5, 12, 14);
  VERIFY( s == "+00001234.50 +0000001234.50 +0000001234.50" );

  s = std::format("{:#.2g}", -0.0);
  VERIFY( s == "-0.0" );

  // PR libstdc++/108046
  s = std::format("{0:#.0} {0:#.1} {0:#.0g}", 10.0);
  VERIFY( s == "1.e+01 1.e+01 1.e+01" );

  // PR libstdc++/113512
  s = std::format("{:#.3g}", 0.025);
  VERIFY( s == "0.0250" );
  s = std::format("{:#07.3g}", 0.02);
  VERIFY( s == "00.0200" );
}

void
test_infnan()
{
  double inf = std::numeric_limits<double>::infinity();
  double nan = std::numeric_limits<double>::quiet_NaN();
  std::string s;
  s = std::format("{0} {0:e} {0:E} {0:f} {0:F} {0:g} {0:G} {0:a} {0:A}", inf);
  VERIFY( s == "inf inf INF inf INF inf INF inf INF" );
  s = std::format("{0} {0:e} {0:E} {0:f} {0:F} {0:g} {0:G} {0:a} {0:A}", nan);
  VERIFY( s == "nan nan NAN nan NAN nan NAN nan NAN" );
}

struct euro_punc : std::numpunct<char>
{
  std::string do_grouping() const override { return "\3\3"; }
  char do_thousands_sep() const override { return '.'; }
  char do_decimal_point() const override { return ','; }
};

void
test_locale()
{
  // The default C locale.
  std::locale cloc = std::locale::classic();
  // A custom locale using comma digit separators.
  std::locale bloc(cloc, new brit_punc);
  // A custom locale using period digit separators.
  std::locale eloc(cloc, new euro_punc);

  std::string s;

  // Change the global locale:
  std::locale::global(bloc);
  // Format using the global locale:
  s = std::format("{0:L} {0:Lx} {0:Lb}", 12345);
  VERIFY( s == "12,345 3,039 11,000,000,111,001" );
  s = std::format("{0:L} {0:.7Lg} {0:La}", 12345.6789);
  VERIFY( s == "12,345.6789 12,345.68 1.81cd6e631f8a1p+13" );

  s = std::format("{0:s} {0:L} {1:Ls} {0:Ld}", true, false);
  VERIFY( s == "true yes mate nah bruv 1" );

  // Format using a specific locale:
  s = std::format(eloc, "{0:L} {0:Lx} {0:Lb}", 12345);
  VERIFY( s == "12.345 3.039 11.000.000.111.001" );
  s = std::format(eloc, "{0:L} {0:.7LG} {0:La}", 12345.6789);
  VERIFY( s == "12.345,6789 12.345,68 1,81cd6e631f8a1p+13" );

  s = std::format(eloc, "{0:#Lg} {0:+#.3Lg} {0:#08.4Lg}", -1234.);
  VERIFY( s == "-1.234,00 -1,23e+03 -01.234," );

  s = std::format(cloc, "{:05L}", -1.0); // PR libstdc++/110968
  VERIFY( s == "-0001" );

  // PR libstdc++/114863 grouping applied to nan and inf
  double inf = std::numeric_limits<double>::infinity();
  s = std::format(eloc, "{0:Le} {0:Lf} {0:Lg}", -inf);
  VERIFY( s == "-inf -inf -inf" );
  double nan = std::numeric_limits<double>::quiet_NaN();
  s = std::format(eloc, "{0:Le} {0:Lf} {0:Lg}", -nan);
  VERIFY( s == "-nan -nan -nan" );

  // Restore
  std::locale::global(cloc);
}

void
test_width()
{
  std::string s;

  s = std::format("{:4}", "");
  VERIFY( s == "    " );
  s = std::format("{:{}}", "", 3);
  VERIFY( s == "   " );
  s = std::format("{:{}}|{:{}}", 1, 2, 3, 4);
  VERIFY( s == " 1|   3" );
  s = std::format("{1:{0}}", 2, "");
  VERIFY( s == "  " );
  s = std::format("{:03}", 9);
  VERIFY( s == "009" );

  s = std::format("DR {0:{1}}: allow width {1} from arg-id", 3721, 0);
  VERIFY( s == "DR 3721: allow width 0 from arg-id" );

  try {
    s = std::format("Negative width is an error: {0:{1}}", 123, -1);
    VERIFY(false);
  } catch (const std::format_error&) {
  }

  try {
    bool no = false, yes = true;
    auto args = std::make_format_args(no, yes);
    s = std::vformat("DR 3720: restrict type of width arg-id {0:{1}}", args);
    VERIFY(false);
  } catch (const std::format_error&) {
  }

  try {
    char wat = '?', bang = '!';
    auto args = std::make_format_args(wat, bang);
    s = std::vformat("DR 3720: restrict type of width arg-id {0:{1}}", args);
    VERIFY(false);
  } catch (const std::format_error&) {
  }
}

void
test_char()
{
  std::string s;

  s = std::format("{}", 'a');
  VERIFY( s == "a" );

  s = std::format("{:c} {:d} {:o}", 'b', '\x17', '\x3f');
  VERIFY( s == "b 23 77" );

  s = std::format("{:#d} {:#o}", '\x17', '\x3f');
  VERIFY( s == "23 077" );

  s = std::format("{:04d} {:04o}", '\x17', '\x3f');
  VERIFY( s == "0023 0077" );

  s = std::format("{:b} {:B} {:#b} {:#B}", '\xff', '\xa0', '\x17', '\x3f');
  VERIFY( s == "11111111 10100000 0b10111 0B111111" );

  s = std::format("{:x} {:#x} {:#X}", '\x12', '\x34', '\x45');
  VERIFY( s == "12 0x34 0X45" );

  // P2909R4 Fix formatting of code units as integers (Dude, where’s my char?)
  // char and wchar_t should be converted to unsigned when formatting them
  // with an integer presentation type.
  s = std::format("{0:b} {0:B} {0:d} {0:o} {0:x} {0:X}", '\xf0');
  VERIFY( s == "11110000 11110000 240 360 f0 F0" );
}

void
test_wchar()
{
  using namespace std::literals;
  std::wstring s;

  s = std::format(L"{}", L'a');
  VERIFY( s == L"a" );

  s = std::format(L"{} {} {} {} {} {}", L'0', 1, 2LL, 3.4, L"five", L"six"s);
  VERIFY( s == L"0 1 2 3.4 five six" );

  std::locale loc;
  s = std::format(loc, L"{:L} {:.3s}{:Lc}", true, L"data"sv, '.');
  VERIFY( s == L"true dat." );

  s = std::format(L"{}", 0.0625);
  VERIFY( s == L"0.0625" );
  s = std::format(L"{}", 0.25);
  VERIFY( s == L"0.25" );
  s = std::format(L"{:+a} {:A}", 0x1.23p45, -0x1.abcdefp-15);
  VERIFY( s == L"+1.23p+45 -1.ABCDEFP-15" );

  double inf = std::numeric_limits<double>::infinity();
  double nan = std::numeric_limits<double>::quiet_NaN();
  s = std::format(L"{0} {0:F} {1} {1:E}", -inf, -nan);
  VERIFY( s == L"-inf -INF -nan -NAN" );

  s = std::format(L"{0:#b} {0:#B} {0:#x} {0:#X}", 99);
  VERIFY( s == L"0b1100011 0B1100011 0x63 0X63" );

  // P2909R4 Fix formatting of code units as integers (Dude, where’s my char?)
  s = std::format(L"{:d} {:d}", wchar_t(-1), char(-1));
  VERIFY( s.find('-') == std::wstring::npos );

  auto ws = std::format(L"{:L}", 0.5);
  VERIFY( ws == L"0.5" );
  // The default C locale.
  std::locale cloc = std::locale::classic();
  // PR libstdc++/119671 use-after-free formatting floating-point to wstring
  ws = std::format(cloc, L"{:L}", 0.5);
  VERIFY( ws == L"0.5" );
  // A locale with no name, but with the same facets as the C locale.
  std::locale locx(cloc, &std::use_facet<std::ctype<char>>(cloc));
  ws = std::format(locx, L"{:L}", 0.5);
  VERIFY( ws == L"0.5" );
}

void
test_minmax()
{
  auto check = []<typename T, typename U = std::make_unsigned_t<T>>(T, U = 0) {
    const int digits = std::numeric_limits<T>::digits;
    const std::string zeros(digits, '0');
    const std::string ones(digits, '1');
    auto s = std::format("{:b}" , std::numeric_limits<T>::min());
    VERIFY( s == "-1" + zeros );
    s = std::format("{:b}" , std::numeric_limits<T>::max());
    VERIFY( s == ones );
    s = std::format("{:0{}b}" , std::numeric_limits<U>::min(), digits + 1);
    VERIFY( s == '0' + zeros );
    s = std::format("{:b}" , std::numeric_limits<U>::max());
    VERIFY( s == '1' + ones );
  };
  check((signed char)(0)); // int8_t is char on Solaris, see PR 113450
  check(std::int16_t(0));
  check(std::int32_t(0));
  check(std::int64_t(0));
#ifdef __SIZEOF_INT128__
  // std::make_unsigned_t<__int128> is invalid for strict -std=c++20 mode,
  // so pass a second argument of the unsigned type.
  check(__int128(0), (unsigned __int128)(0));
#endif
}

void
test_p1652r1() // printf corner cases in std::format
{
  std::string s;

  // Problem 1: "#o" specification should not print 0 as "00"
  s = std::format("{:#o}", 0);
  VERIFY( s == "0" );

  // Problem 2: 'c' should be able to print 65 as "A" (ASCII)
  int c = 'A';
  s = std::format("{:c}", c);
  VERIFY( s == "A" );

  // Problem 3: "-000nan" is not a floating point value
  double nan = std::numeric_limits<double>::quiet_NaN();
  try {
    s = std::vformat("{:0=6}", std::make_format_args(nan));
    VERIFY( false );
  } catch (const std::format_error&) {
  }

  s = std::format("{:06}", nan);
  VERIFY( s == "   nan" );

  // Problem 4: bool needs a type format specifier
  s = std::format("{:s}", true);
  VERIFY( s == "true" );

  // Problem 5: double does not roundtrip float
  s = std::format("{}", 3.31f);
  VERIFY( s == "3.31" );
}

void
test_pointer()
{
  void* p = nullptr;
  const void* pc = p;
  std::string s, str_int;

  s = std::format("{}", p);
  VERIFY( s == "0x0" );

  s = std::format("{} {} {}", p, pc, nullptr);
  VERIFY( s == "0x0 0x0 0x0" );
  s = std::format("{:p} {:p} {:p}", p, pc, nullptr);
  VERIFY( s == "0x0 0x0 0x0" );
  s = std::format("{:4},{:5},{:6}", p, pc, nullptr); // width
  VERIFY( s == " 0x0,  0x0,   0x0" );
  s = std::format("{:<4},{:>5},{:^7}", p, pc, nullptr); // align+width
  VERIFY( s == "0x0 ,  0x0,  0x0  " );
  s = std::format("{:o<4},{:o>5},{:o^7}", p, pc, nullptr); // fill+align+width
  VERIFY( s == "0x0o,oo0x0,oo0x0oo" );

  pc = p = &s;
  str_int = std::format("{:#x}", reinterpret_cast<std::uintptr_t>(p));
  s = std::format("{} {} {}", p, pc, nullptr);
  VERIFY( s == (str_int + ' ' + str_int + " 0x0") );
  str_int = std::format("{:#20x}", reinterpret_cast<std::uintptr_t>(p));
  s = std::format("{:20} {:20p}", p, pc);
  VERIFY( s == (str_int + ' ' + str_int) );

#if __cpp_lib_format >= 202304L
  // P2510R3 Formatting pointers
  s = std::format("{:06} {:07P} {:08p}", (void*)0, (const void*)0, nullptr);
  VERIFY( s == "0x0000 0X00000 0x000000" );
  str_int = std::format("{:#016x}", reinterpret_cast<std::uintptr_t>(p));
  s = std::format("{:016} {:016}", p, pc);
  VERIFY( s == (str_int + ' ' + str_int) );
  str_int = std::format("{:#016X}", reinterpret_cast<std::uintptr_t>(p));
  s = std::format("{:016P} {:016P}", p, pc);
  VERIFY( s == (str_int + ' ' + str_int) );
#endif
}

void
test_bool()
{
  std::string s;

  s = std::format("{}", true);
  VERIFY( s == "true" );
  s = std::format("{:} {:s}", true, false);
  VERIFY( s == "true false" );
  s = std::format("{:b} {:#b}", true, false);
  VERIFY( s == "1 0b0" );
  s = std::format("{:B} {:#B}", false, true);
  VERIFY( s == "0 0B1" );
  s = std::format("{:d} {:#d}", false, true);
  VERIFY( s == "0 1" );
  s = std::format("{:o} {:#o} {:#o}", false, true, false);
  VERIFY( s == "0 01 0" );
  s = std::format("{:x} {:#x} {:#X}", false, true, false);
  VERIFY( s == "0 0x1 0X0" );
}

void
test_unicode()
{
  // Similar to sC example in test_std_examples, but not from the standard.
  // Verify that the character "🤡" has estimated field width 2,
  // rather than estimated field width equal to strlen("🤡"), which would be 4,
  // or just width 1 for single character.
  std::string sC = std::format("{:*<3}", "🤡");
  VERIFY( sC == "🤡*" );
  std::wstring wsC = std::format(L"{:*<3}", L"🤡");
  VERIFY( wsC == L"🤡*" );
  wsC = std::format(L"{:*<3}", L'🤡');
  VERIFY( wsC == L"🤡*" );

  // Verify that "£" has estimated field width 1, not strlen("£") == 2.
  std::string sL = std::format("{:*<3}", "£");
  VERIFY( sL == "£**" );

  // Verify that precision is measured in field width units (column positions)
  // not bytes. The result should contain complete Unicode characters, not be
  // truncated in the middle of a multibyte UTF-8 sequence. The string "£" has
  // field width 1 despite being 2 bytes, and the string "🤡" has field width 2
  // and so cannot be formatted into a replacement field using .1 precision.
  std::string sP = std::format("{:1.1} {:*<1.1}", "£", "🤡");
  VERIFY( sP == "£ *" );
  sP = std::format("{:*<2.1} {:*<2.1}", "£", "🤡");
  VERIFY( sP == "£* **" );

  // Verify field width handling for extended grapheme clusters,
  // and that a cluster gets output as a single item, not truncated.
  std::string sG = std::format("{:*>2.1}", "\u006f\u0302\u0323!");
  VERIFY( sG == "*\u006f\u0302\u0323" );

  // Examples from P1868R2
  // https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2020/p1868r2.html
  const char* inputs[][2] = {
    {"\x41",         "    \u0041"},
    {"\xC3\x81",     "    \u00c1"},
    {"\x41\xCC\x81", "    \u0041\u0301"},
    {"\xc4\xb2", "    \u0132"},
    {"\xce\x94", "    \u0394"},
    {"\xd0\xa9", "    \u0429"},
    {"\xd7\x90", "    \u05D0"},
    {"\xd8\xb4", "    \u0634"},
    {"\xe3\x80\x89", "   \u3009"},
    {"\xe7\x95\x8c", "   \u754C"},
    {"\xf0\x9f\xa6\x84", "   \U0001F984"},
    {"\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d"
     "\xf0\x9f\x91\xa7\xe2\x80\x8d\xf0\x9f\x91\xa6",
     "   \U0001F468\u200D\U0001F469\u200D\U0001F467\u200D\U0001F466" }
  };
  for (auto& input : inputs)
  {
    std::string sA = std::format("{:>5}", input[0]);
    VERIFY( sA == input[1] );
  }
}

int main()
{
  test_no_args();
  test_unescaped();
  test_std_examples();
  test_alternate_forms();
  test_locale();
  test_width();
  test_char();
  test_wchar();
  test_minmax();
  test_p1652r1();
  test_pointer();
  test_bool();
  test_unicode();
}
