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
  ss << 2022y/December/19 << ' ' << 2022y/November/31;
  VERIFY( ss.str() == "2022-12-19 2022-11-31 is not a valid date" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 1789y/July/14;
  VERIFY( ss.str() == "1789-07-14" );
}

void
test_format()
{
  using std::chrono::year_month_day;
  using std::chrono::December;
  using std::chrono::January;
  using namespace std::chrono_literals;

  auto s = std::format("{:%y%%%Y%t%C%n%j %a %b}", 2022y/December/19);
  VERIFY( s == "22%2022\t20\n353 Mon Dec" );
  auto ws = std::format(L"{:%y%%%Y%t%C%n%d}", 2023y/January/32);
  VERIFY( ws == L"23%2023\t20\n32" );

  s = std::format("{:%F} {}", 2023y/January/32, 2023y/January/32);
  VERIFY( s == "2023-01-32 2023-01-32 is not a valid date" );

  s = std::format("{:%C%g-W%V-%u}", 2022y/January/1);
  VERIFY( s == "2021-W52-6" );
  s = std::format("{:%G-W%V-%u}", 2022y/January/3);
  VERIFY( s == "2022-W01-1" );

  // %U: Week number for weeks starting on Sunday
  s = std::format("Day {:%w (%a) of Week %U of %Y}", 2022y/January/1);
  VERIFY( s == "Day 6 (Sat) of Week 00 of 2022" );
  s = std::format("Day {:%w (%a) of Week %U of %Y}", 2022y/January/2);
  VERIFY( s == "Day 0 (Sun) of Week 01 of 2022" );
  // %W: Week number for weeks starting on Monday
  s = std::format("Day {:%u (%a) of Week %W of %Y}", 2022y/January/2);
  VERIFY( s == "Day 7 (Sun) of Week 00 of 2022" );
  s = std::format("Day {:%u (%a) of Week %W of %Y}", 2022y/January/3);
  VERIFY( s == "Day 1 (Mon) of Week 01 of 2022" );

  // %V: ISO week number (ISO 8601).
  s = std::format("W{:%V}", 1977y/1/1);
  VERIFY( s == "W53" );
  s = std::format("W{:%V}", 1977y/1/2);
  VERIFY( s == "W53" );
  s = std::format("W{:%V}", 1977y/12/31);
  VERIFY( s == "W52" );
  s = std::format("W{:%V}", 1978y/1/1);
  VERIFY( s == "W52" );
  s = std::format("W{:%V}", 1978y/1/2);
  VERIFY( s == "W01" );
  s = std::format("W{:%V}", 1978y/12/31);
  VERIFY( s == "W52" );
  s = std::format("W{:%V}", 1979y/1/1);
  VERIFY( s == "W01" );
  s = std::format("W{:%V}", 1979y/12/30);
  VERIFY( s == "W52" );
  s = std::format("W{:%V}", 1979y/12/31);
  VERIFY( s == "W01" );
  s = std::format("W{:%V}", 1980y/1/1);
  VERIFY( s == "W01" );

  s = std::format("{:%x}", 2022y/December/19);
  VERIFY( s == "12/19/22" );
  s = std::format("{:L%x}", 2022y/December/19);
  VERIFY( s == "12/19/22" );
  std::locale loc_fr(ISO_8859(15,fr_FR));
  s = std::format(loc_fr, "{:%x}", 2022y/December/19);
  VERIFY( s == "12/19/22" );
  s = std::format(loc_fr, "{:L%x}", 2022y/December/19);
  VERIFY( s == "19/12/2022" || s == "19.12.2022" ); // depends on locale defs
  s = std::format(loc_fr, "{}", 2022y/December/19);
  VERIFY( s == "2022-12-19" );
  s = std::format(loc_fr, "{:L%F}", 2022y/December/19);
  VERIFY( s == "2022-12-19" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "aAbBCdDeFgGhjmuUVwWxyY";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      year_month_day ymd = 2022y/December/19;
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(ymd));
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
  const year_month_day expected = 2023y/August/10;
  year_month_day ymd;

  minutes offset;
  std::string abbrev;
  std::istringstream is("23 2220 21:44:3 +1 'BST'");
  VERIFY( is >> parse("%y %j0 %4H:%5M:%6S %Oz '%Z'", ymd, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( ymd == expected );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );

  is.clear();
  is.str("2023 365");
  VERIFY( is >> parse("%Y %j", ymd) );
  VERIFY( ymd == 2023y/December/31 );

  ymd = 1970y/January/1;
  is.clear();
  is.str("2023 366");
  VERIFY( ! (is >> parse("%Y %j", ymd)) ); // Not a leap year, no 366th day.
  VERIFY( ymd == 1970y/January/1 );

  is.clear();
  is.str("2020 366");
  VERIFY( is >> parse("%Y %j", ymd) );
  VERIFY( ! is.eof() );
  VERIFY( ymd == 2020y/December/31 );

  ymd = 1970y/January/1;
  is.clear();
  is.str("2020 0");
  VERIFY( ! (is >> parse("%Y %j", ymd)) ); // zero is invalid for day-of-year
  VERIFY( is.eof() );
  VERIFY( ymd == 1970y/January/1 );

  is.clear();
  is.str("2023-01-01 00:30 0100");
  VERIFY( is >> parse("%F %R %z", ymd) );
  VERIFY( ! is.eof() );
  VERIFY( ymd == 2023y/January/1 ); // Date not adjusted by TZ offset.

  ymd = {};
  is.clear();
  is.str("2022-W52-6");
  VERIFY( is >> parse("%G-W%V-%u", ymd) );
  VERIFY( ymd == 2022y/December/31 );

  is.clear();
  is.str("2022-W52-8");
  VERIFY( ! (is >> parse("%G-W%V-%u", ymd)) ); // 8 is not a valid weekday
  is.clear();
  is.str("2022-W52-0");
  VERIFY( ! (is >> parse("%G-W%V-%u", ymd)) ); // 0 is not a valid weekday
  is.clear();
  is.str("2022-W53-1");
  VERIFY( ! (is >> parse("%G-W%V-%u", ymd)) ); // W53 is not valid for 2022
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
