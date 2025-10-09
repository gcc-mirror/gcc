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
test_format()
{
  using namespace std::chrono;
  std::locale loc_fr(ISO_8859(15,fr_FR));

  auto s = std::format("{:%C%%%y\t%Y %b%%%B%t%m%n}", year(2019)/month(4));
  VERIFY( s == "20%19\t2019 Apr%April\t04\n" );
  s = std::format(loc_fr, "{:L%C%%%y\t%Y %b%%%B%t%m%n}", year(2019)/month(4));
  VERIFY( s == "20%19\t2019 avril%avril\t04\n");

  s = std::format("{0:%Y/%m} {0}", year(2018)/month(2));
  VERIFY( s == "2018/02 2018/Feb" );
  s = std::format("{0:%Y/%m} {0}", year(-32768)/month(15));
  VERIFY( s == "-32768/15 -32768 is not a valid year/15 is not a valid month" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "CbBhmyY";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      auto ym = year(2013)/month(1);
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(ym));
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
  test_format();
  test_parse();
}
