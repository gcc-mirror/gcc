// { dg-do run { target c++20 } }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

template<typename T, typename CharT>
  concept stream_extractable
    = requires (std::basic_istream<CharT>& is) { is >> std::declval<T>(); };

void
test_recommended_practice()
{
  std::chrono::seconds s;
  using parse_manip = decltype(std::chrono::parse("", s));
  static_assert( stream_extractable<parse_manip, char> );
  static_assert( not stream_extractable<parse_manip, wchar_t> );
  using wparse_manip = decltype(std::chrono::parse(L"", s));
  static_assert( stream_extractable<wparse_manip, wchar_t> );
  static_assert( not stream_extractable<wparse_manip, char> );

  // These properties are recommended by the standard, to avoid using a
  // parse manipulator that has a dangling reference to a format string.
  static_assert( not std::is_move_constructible_v<parse_manip> );
  static_assert( not std::is_move_assignable_v<parse_manip> );
  static_assert( not stream_extractable<parse_manip&, char> );
  static_assert( not stream_extractable<const parse_manip&, char> );
  static_assert( not stream_extractable<wparse_manip&, wchar_t> );
  static_assert( not stream_extractable<const wparse_manip&, wchar_t> );
}

template<typename... Args>
  concept parsable = requires(Args... args) { std::chrono::parse(args...); };

const std::string f = "format string";

namespace N
{
  struct A { };

  void
  from_stream(std::istream&, const char* fmt, A&)
  {
    VERIFY( fmt == f.c_str() );
  }

  template<typename... Args>
    void
    from_stream(std::istream&, const char*, A&, void*, void*) = delete;

  struct B { };

  void
  from_stream(std::istream&, const char* fmt, B&, std::string* abbrev)
  {
    VERIFY( fmt == f.c_str() );
    VERIFY( abbrev != nullptr );
  }

  void
  from_stream(std::istream&, const char*, B&, std::string*, void*) = delete;

  struct C { };

  void
  from_stream(std::istream&, const char* fmt, C&, std::string* abbrev,
	      std::chrono::minutes* offset)
  {
    VERIFY( fmt == f.c_str() );
    VERIFY( abbrev == nullptr );
    VERIFY( offset != nullptr );
  }

  struct D { };

  void
  from_stream(std::istream&, const char* fmt, D&, std::string* abbrev,
	      std::chrono::minutes* offset)
  {
    VERIFY( fmt == f.c_str() );
    VERIFY( abbrev != nullptr );
    VERIFY( offset != nullptr );
  }

  struct E { };

  void
  from_stream(std::wistream&, const wchar_t*, E&, std::wstring* = nullptr,
	      std::chrono::minutes* = nullptr)
  { }
}

void
test_adl()
{
  using std::string;
  using std::wstring;
  using std::chrono::minutes;

  string abbrev;
  minutes offset;

  // Check that valid calls are well-formed.
  N::A a;
  (void) std::chrono::parse(f, a);
  N::B b;
  (void) std::chrono::parse(f, b, abbrev);
  N::C c;
  (void) std::chrono::parse(f, c, offset);
  // This satisfies the concept, but would fail the VERIFY assertion:
  static_assert( parsable<const char*, N::C, string, minutes> );
  N::D d;
  (void) std::chrono::parse(f, d, abbrev, offset);
  // This satisfies the concept, but would fail the VERIFY assertion:
  static_assert( parsable<const char*, N::D, minutes> );

  // Wide strings.
  static_assert( parsable<const wchar_t*, N::E, wstring> );
  static_assert( parsable<const wchar_t*, N::E, wstring> );
  static_assert( parsable<const wchar_t*, N::E, minutes> );
  static_assert( parsable<const wchar_t*, N::E, wstring, minutes> );

  // Check that invalid calls are properly constrained.

  // from_stream is only overloaded for N::A without abbrev or offset.
  static_assert( not parsable<const char*, N::A, std::string> );
  static_assert( not parsable<const char*, N::A, minutes> );
  static_assert( not parsable<const char*, N::A, string, minutes> );
  // from_stream is only overloaded for N::B with abbrev.
  static_assert( not parsable<const char*, N::B> );
  static_assert( not parsable<const char*, N::B, minutes> );
  static_assert( not parsable<const char*, N::B, string, minutes> );
  // from_stream is only overloaded for N::C with abbrev and minutes.
  static_assert( not parsable<const char*, N::C> );
  static_assert( not parsable<const char*, N::C, string> );
  // from_stream is only overloaded for N::D with abbrev and minutes.
  static_assert( not parsable<const char*, N::D> );
  static_assert( not parsable<const char*, N::D, string> );

  // Mismatched strings
  static_assert( not parsable<string, std::chrono::year, wstring> );
  static_assert( not parsable<string, std::chrono::year, wstring, minutes> );

  using Alloc = __gnu_test::SimpleAllocator<char>;
  using String = std::basic_string<char, std::char_traits<char>, Alloc>;
  // Custom allocator
  static_assert( parsable<String, std::chrono::year> );
  static_assert( parsable<String, std::chrono::year, String> );
  static_assert( parsable<String, std::chrono::year, minutes> );
  static_assert( parsable<String, std::chrono::year, String, minutes> );
  static_assert( parsable<const char*, std::chrono::year, String> );
  static_assert( parsable<const char*, std::chrono::year, String, minutes> );
  // Mismatched allocators
  static_assert( not parsable<string, std::chrono::year, String> );
  static_assert( not parsable<string, std::chrono::year, String, minutes> );
  static_assert( not parsable<String, std::chrono::year, string> );
  static_assert( not parsable<String, std::chrono::year, string, minutes> );
}

void
test_whitespace()
{
  using namespace std::chrono_literals;
  std::chrono::minutes min;
  std::istringstream is;
  is.str("   a  b  1  ");
  is >> parse(" a b %M", min);
  VERIFY( is.good() );
  VERIFY( min == 1min );
  is.str("   a  b  1  ");
  is >> parse(" a b %M ", min);
  VERIFY( is.eof() && !is.fail() );
  VERIFY( min == 1min );
  is.clear();
  is.str("   1");
  is >> parse(" %n%M%n", min);
  VERIFY( is.fail() );
  is.clear();
  is.str("   a  b  1  ");
  is >> parse("%n a%n%nb %t%M%n", min);
  VERIFY( is.good() );
  VERIFY( min == 1min );
  is.str("a  b  1  ");
  is >> parse("%ta b %M%n%t", min);
  VERIFY( is.good() );
  VERIFY( min == 1min );
  is.str("1  ");
  is >> parse("%M%n%t%t", min);
  VERIFY( is.eof() && !is.fail() );
  VERIFY( min == 1min );
  is.clear();
  is.str("1  ");
  is >> parse("%M%n%t%t", min);
  VERIFY( is.eof() && !is.fail() );
  VERIFY( min == 1min );
  is.clear();
  is.str("1  ");
  is >> parse("%M%n%t%n", min);
  VERIFY( is.eof() && is.fail() );
  VERIFY( min == 1min );
}

void
test_errors()
{
  using namespace std::chrono_literals;
  std::chrono::minutes min(999);
  std::chrono::year y(-1);
  std::istringstream is;

  is.str("x");
  is >> parse("x", min); // Matches expected pattern, but no minutes present.
  VERIFY( !is.eof() && is.fail() );
  VERIFY( min == 999min );

  is.clear();
  is.str("x");
  is >> parse("%M", min); // Doesn't match expected pattern.
  VERIFY( !is.eof() && is.fail() );
  VERIFY( min == 999min );

  is.clear();
  is.str("001:002");
  is >> parse("%H:%M", min); // Extracts "00" then fails to find ':' next.
  VERIFY( !is.eof() && is.fail() );
  VERIFY( min == 999min );

  is.clear();
  is.str("12:61");
  is >> parse("%H:%M", min); // 61min is out of range.
  VERIFY( !is.eof() && is.fail() );
  VERIFY( min == 999min );

  is.clear();
  is.str("12:15 100");
  is >> parse("%H:%M %3y", min); // 100y is out of range for %y but not needed
  VERIFY( is.good() );
  VERIFY( min == (12h + 15min) );

  min = 999min;
  is.clear();
  is.str("12:15 100");
  is >> parse("%H:%M %3y", y); // 100y is out of range for %y and needed
  VERIFY( is.fail() );
  VERIFY( y == -1y );

  is.clear();
  is.str("23:61 10");
  is >> parse("%H:%M %3y", y); // 61min is out of range but not needed
  VERIFY( is.eof() && ! is.fail() );
  VERIFY( y == 2010y );

  min = -1min;
  is.clear();
  is.str("25:59");
  is >> parse("%H:%M", min); // 25h is out of range and needed
  VERIFY( is.fail() );
  VERIFY( min == -1min );

  is.clear();
  is.str("328 00");
  is >> parse("%3C %y", y); // 328 is out of range for %C (PR libstdc++/111162)
  VERIFY( is.fail() );
  VERIFY( y == 2010y );

  is.clear();
  is.str("-328 00");
  is >> parse("%3C %y", y); // -328 is out of range for %C
  VERIFY( is.fail() );
  VERIFY( y == 2010y );
}

void
test_modifiers()
{
  using namespace std::chrono_literals;
  std::chrono::minutes min;
  std::istringstream is;

  is.str("0001:000002");
  is >> parse("%4H:%5M", min);
  VERIFY( is.good() );
  VERIFY( min == 60min );

  is.str("0001:000002");
  is >> parse("%6H:%6M", min);
  VERIFY( is.good() );
  VERIFY( min == 62min );

  is.str("002");
  is >> parse("%4M", min);
  VERIFY( is.eof() && !is.fail() );
  VERIFY( min == 2min );

  is.clear();
  is.str("0061");
  is >> parse("%3M", min);
  VERIFY( is.good() );
  VERIFY( min == 6min );

  is.clear();
  is.str("0061");
  is >> parse("%4M", min);
  VERIFY( !is.eof() && is.fail() );

  is.clear();
  is.str("0061");
  is >> parse("%5M", min);
  VERIFY( is.eof() && is.fail() );

  std::chrono::seconds s;
  is.clear();
  is.str("000000000012345");
  is >> parse("%12S", s); // Read more than 10 digits to check overflow logic.
  VERIFY( is.good() );
  VERIFY( s == 12s );
}

int main()
{
  test_recommended_practice();
  test_adl();
  test_whitespace();
  test_errors();
  test_modifiers();
}
