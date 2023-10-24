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
  ss << 2023y/January << ' ' << 2023y/month(13);
  VERIFY( ss.str() == "2023/Jan 2023/13 is not a valid month" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 2023y/July;
  VERIFY( ss.str() == "2023/juil." );
}

void
test_parse()
{
  using namespace std::chrono;
  year_month ym;

  std::istringstream is("20238");
  VERIFY( is >> parse("%Y%m", ym) );
  VERIFY( is.eof() );
  VERIFY( ym == 2023y/August );

  ym = 1y/January;
  is.clear();
  is.str("20238");
  VERIFY( ! (is >> parse("%5Y%m", ym)) );
  VERIFY( is.eof() );
  VERIFY( ym == 1y/January );

  is.clear();
  is.str("2023");
  VERIFY( is >> parse("%2Y%1m", ym) );
  VERIFY( ! is.eof() );
  VERIFY( ym == 20y/February );

  is.clear();
  is.str("2012");
  VERIFY( is >> parse("%y%m", ym) );
  VERIFY( ! is.eof() );
  VERIFY( ym == 2020y/December );

  minutes offset;
  std::string abbrev;

  is.clear();
  is.str("4/1/20 25:61 +1:30 WAT"); // Invalid %H:%M doesn't matter for year_mon
  VERIFY( is >> parse("%D %H:%M %Oz %Z", ym, abbrev, offset) );
  VERIFY( is.eof() );
  VERIFY( ym == 2020y/April );
  VERIFY( abbrev == "WAT" );
  VERIFY( offset == 90min );

  is.clear();
  is.str("02022-W052-7");
  is >> parse("%6G-W%4V-%2u", ym);
  VERIFY( is.eof() );
  VERIFY( ym == 2023y/January );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  test_parse();
}
