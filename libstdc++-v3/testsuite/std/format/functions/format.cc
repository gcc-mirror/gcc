// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <format>

#ifndef __cpp_lib_format
# error "Feature test macro for std::format is missing in <format>"
#elif __cpp_lib_format < 202106L
# error "Feature test macro for std::format has wrong value in <format>"
#endif

#undef __cpp_lib_format
#include <version>
#ifndef __cpp_lib_format
# error "Feature test macro for std::format is missing in <version>"
#elif __cpp_lib_format < 202106L
# error "Feature test macro for std::format has wrong value in <version>"
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
    auto args = std::make_format_args(false, true);
    s = std::vformat("DR 3720: restrict type of width arg-id {0:{1}}", args);
    VERIFY(false);
  } catch (const std::format_error&) {
  }

  try {
    auto args = std::make_format_args('?', '!');
    s = std::vformat("DR 3720: restrict type of width arg-id {0:{1}}", args);
    VERIFY(false);
  } catch (const std::format_error&) {
  }
}

void
test_wchar()
{
  using namespace std::literals;
  std::wstring s;

  s = std::format(L"{} {} {} {} {} {}", L'0', 1, 2LL, 3.4, L"five", L"six"s);
  VERIFY( s == L"0 1 2 3.4 five six" );

  std::locale loc;
  s = std::format(loc, L"{:L} {:.3s}{:Lc}", true, L"data"sv, '.');
  VERIFY( s == L"true dat." );
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
  check(std::int8_t(0));
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

template<typename T>
bool format_float()
{
    auto s = std::format("{:#} != {:<+7.3f}", (T)-0.0, (T)0.5);
    return s == "-0. != +0.500 ";
}

#if __cplusplus > 202002L
template<typename T>
concept formattable = std::formattable<T, char>;
#else
template<typename T>
concept formattable = requires (T t, char* p) { std::to_chars(p, p, t); };
#endif

void
test_float128()
{
#ifdef __SIZEOF_FLOAT128__
  if constexpr (formattable<__float128>)
    VERIFY( format_float<__float128>() );
  else
    std::puts("Cannot format __float128 on this target");
#endif
#if __FLT128_DIG__
  if constexpr (formattable<_Float128>)
    VERIFY( format_float<_Float128>() );
  else
    std::puts("Cannot format _Float128 on this target");
#endif
}

void
test_pointer()
{
  void* p = nullptr;
  const void* pc = p;
  std::string s, str_int;

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

#if __cplusplus > 202302L || ! defined __STRICT_ANSI__
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

int main()
{
  test_no_args();
  test_unescaped();
  test_std_examples();
  test_alternate_forms();
  test_locale();
  test_width();
  test_wchar();
  test_minmax();
  test_p1652r1();
  test_float128();
  test_pointer();
}
