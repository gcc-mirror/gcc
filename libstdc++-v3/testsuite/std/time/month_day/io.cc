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
  ss << January/14 << ' ' << February/30 << ' ' << March/34;
  VERIFY( ss.str() == "Jan/14 Feb/30 Mar/34 is not a valid day" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/27;
  VERIFY( ss.str() == "juil./27" );
}

void
test_parse()
{
  using namespace std::chrono;
  std::istringstream is;
  month_day md{};

  is.str("jul 0123");
  VERIFY( is >> parse("%B %3e", md) );
  VERIFY( ! is.eof() );
  VERIFY( is.peek() == '3' );
  VERIFY( md == July/12 );

  is.str("August 11");
  VERIFY( is >> parse("%b %d", md) );
  VERIFY( ! is.eof() );
  VERIFY( md == August/11 );

  is.clear();
  is.str("012");
  VERIFY( is >> parse("%m%2d", md) );
  VERIFY( is.eof() );
  VERIFY( md == January/2 );

  is.clear();
  is.str("012/311");
  VERIFY( is >> parse("%4m/%d", md) );
  VERIFY( ! is.eof() );
  VERIFY( md == December/31 );

  is.clear();
  is.str("2023-7-31");
  VERIFY( is >> parse("%F", md) );
  VERIFY( ! is.eof() );
  VERIFY( md == July/31 );

  md = month(13)/day(32);
  is.clear();
  is.str("2023-13-1");
  VERIFY( ! (is >> parse("%F", md)) );
  VERIFY( is.eof() );
  VERIFY( md == month(13)/day(32) );

  md = month(13)/day(33);
  is.clear();
  is.str("2023-6-31");
  VERIFY( ! (is >> parse("%F", md)) ); // June only has 30 days.
  VERIFY( ! is.eof() );
  VERIFY( md == month(13)/day(33) );

  md = month(13)/day(34);
  is.clear();
  is.str("6-31");
  VERIFY( ! (is >> parse("%m-%d", md)) ); // June only has 30 days in any year.
  VERIFY( ! is.eof() );
  VERIFY( md == month(13)/day(34) );

  md = month(13)/day(35);
  is.clear();
  is.str("2023-2-29");
  VERIFY( ! (is >> parse("%Y-%m-%e", md)) ); // Feb only has 28 days in 2023.
  VERIFY( ! is.eof() );
  VERIFY( md == month(13)/day(35) );

  is.clear();
  is.str("2-29");
  VERIFY( is >> parse("%m-%d", md) ); // But Feb has 29 days in some years.
  VERIFY( ! is.eof() );
  VERIFY( md == February/29 );

  is.clear();
  is.str("2023-W32-5");
  VERIFY( is >> parse("%G-W%V-%u", md) );
  VERIFY( ! is.eof() );
  VERIFY( md == August/11 );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  test_parse();
}
