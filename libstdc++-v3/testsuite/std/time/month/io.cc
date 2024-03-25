// { dg-do run { target c++20 } }
// { dg-require-namedlocale "fr_FR.ISO8859-15" }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using std::ostringstream;
  using namespace std::chrono;

  ostringstream ss;
  for (int i = 1; i <= 12; ++i)
    ss << month(i);
  VERIFY( ss.str() == "JanFebMarAprMayJunJulAugSepOctNovDec" );
  ss.str("");
  ss << month(0) << '|' << month(13);
  VERIFY( ss.str() == "0 is not a valid month|13 is not a valid month" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << month(1);
  VERIFY( ss.str() == "janv." );
}

void
test_format()
{
  using std::chrono::month;

  auto s = std::format("{:%b%%%B%t%m%n}", month(1));
  VERIFY( s == "Jan%January\t01\n" );
  auto ws = std::format(L"{:%b%%%B%t%m%n}", month(12));
  VERIFY( ws == L"Dec%December\t12\n" );

  try
  {
    (void) std::format("{:%b}", month(13));
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }

  s = std::format("{} is OK, but {:L}", month(2), month(13));
  VERIFY( s == "Feb is OK, but 13 is not a valid month" );

  std::locale loc_fr(ISO_8859(15,fr_FR));

  s = std::format("{:%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(std::locale::classic(), "{:%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(std::locale::classic(), "{:L%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(loc_fr, "{:%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(loc_fr, "{:L%Om}", month(1));
  VERIFY( s == "01" );
  // TODO test "{:L%Om}" with locale that has alternative numeric rep.

  s = std::format(loc_fr, "{:%b}", month(1));
  VERIFY( s == "Jan" );
  s = std::format(loc_fr, "{:L%b}", month(1));
  VERIFY( s == "janv." );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "bBhm";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      month m(1);
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(m));
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

void
test_parse()
{
  using namespace std::chrono;
  std::istringstream is;
  month m{};

  is.str("JUL");
  VERIFY( is >> parse("%B", m) );
  VERIFY( is.eof() );
  VERIFY( m == July );

  is.clear();
  is.str("junE bug");
  VERIFY( is >> parse("  %b  bug ", m) );
  VERIFY( is.eof() );
  VERIFY( m == June );

  is.clear();
  is.str("012");
  VERIFY( is >> parse("%m", m) );
  VERIFY( ! is.eof() );
  VERIFY( is.peek() == '2' );
  VERIFY( m == January );

  is.clear();
  is.str("012");
  VERIFY( is >> parse("%4m", m) );
  VERIFY( is.eof() );
  VERIFY( m == December );

  m = month(15);
  is.clear();
  is.str("Janvember");
  VERIFY( is >> parse("%B", m) ); // Stops parsing after "Jan"
  VERIFY( ! is.eof() );
  VERIFY( is.peek() == 'v' );
  VERIFY( m == January );

  m = month(15);
  is.clear();
  is.str("Junuary");
  VERIFY( is >> parse("%B", m) ); // Stops parsing after "Jun"
  VERIFY( ! is.eof() );
  VERIFY( is.peek() == 'u' );
  VERIFY( m == June );

  m = month(15);
  is.clear();
  is.str("Jebruary");
  VERIFY( ! (is >> parse("%B", m)) );
  VERIFY( is.fail() );
  VERIFY( ! is.eof() );
  is.clear();
  VERIFY( is.peek() == 'e' );
  VERIFY( m == month(15) );

  m = month(13);
  is.clear();
  is.str("2023-6-31");
  VERIFY( ! (is >> parse("%F", m)) ); // June only has 30 days.
  VERIFY( ! is.eof() );
  VERIFY( m == month(13) );

  m = month(14);
  is.clear();
  is.str("2023-2-29");
  VERIFY( ! (is >> parse("%Y-%m-%e", m)) ); // Feb only has 28 days in 2023.
  VERIFY( ! is.eof() );
  VERIFY( m == month(14) );

  is.clear();
  is.str("2-29");
  VERIFY( is >> parse("%m-%d", m) ); // But Feb has 29 days in some years.
  VERIFY( ! is.eof() );
  VERIFY( m == February );

  m = month(14);
  is.clear();
  is.str("6-31");
  VERIFY( ! (is >> parse("%m-%d", m)) ); // June only has 30 days in all years.
  VERIFY( ! is.eof() );
  VERIFY( m == month(14) );

  m = month(15);
  is.clear();
  is.str("2023-13-1");
  VERIFY( ! (is >> parse("%F", m)) );
  VERIFY( is.eof() );
  VERIFY( m == month(15) );

  m = month(16);
  is.clear();
  is.str("13/1/23");
  VERIFY( ! (is >> parse("%D", m)) );
  VERIFY( m == month(16) );

  m = month(17);
  is.clear();
  is.str("13");
  VERIFY( ! (is >> parse("%m", m)) );
  VERIFY( ! is.eof() );
  VERIFY( m == month(17) );

  m = month(18);
  is.clear();
  is.str("1234");
  VERIFY( ! (is >> parse("%3m", m)) );
  VERIFY( ! is.eof() );
  is.clear();
  VERIFY( is.peek() == '4' );
  VERIFY( m == month(18) );

  is.clear();
  is.str("2023-W32-5");
  VERIFY( is >> parse("%G-W%V-%u", m) );
  VERIFY( ! is.eof() );
  VERIFY( m == August );
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
