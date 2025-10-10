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
  ss << 2023y/January/last << ' ' << 2023y/month(13)/last;
  VERIFY( ss.str() == "2023/Jan/last 2023/13 is not a valid month/last" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 2023y/July/last;
  VERIFY( ss.str() == "2023/juil./last" );
}

void
test_format()
{
  using namespace std::chrono;

  std::string s = std::format("{:%Y%t%C%%%y%n%j %b %a}", 2024y/January/last);
  VERIFY( s == "2024\t20%24\n031 Jan Wed" );
  std::wstring ws = std::format(L"{:%Y%t%C%%%y%n%j %b %a}", 2024y/December/last);
  VERIFY( ws == L"2024\t20%24\n366 Dec Tue" );

  s = std::format("{0:%Y-%m-%d} {0}", 2023y/May/last);
  VERIFY( s == "2023-05-31 2023/May/last" );
  s = std::format("{0:%Y-%m-%d} {0}", 2023y/month(13)/last);
  VERIFY( s == "2023-13-30 2023/13 is not a valid month/last" );

  s = std::format("{:%Y-%m-%d %j}", 2024y/February/last);
  VERIFY( s == "2024-02-29 060" );
  s = std::format("{:%Y-%m-%d %j}", 2023y/February/last);
  VERIFY( s == "2023-02-28 059" );
  s = std::format("{:%Y-%m-%d %j}", 2024y/September/last);
  VERIFY( s == "2024-09-30 274" );

  // %U: Week number for weeks starting on Sunday
  s = std::format("{:%Y-U%U}", 2023y/January/last);
  VERIFY( s == "2023-U05" );
  s = std::format("{:%Y-U%U}", 2023y/December/last);
  VERIFY( s == "2023-U53" );
  // %W: Week number for weeks starting on Monday
  s = std::format("{:%Y-W%W}", 2023y/January/last);
  VERIFY( s == "2023-W05" );
  s = std::format("{:%Y-W%W}", 2023y/December/last);
  VERIFY( s == "2023-W52" );

  // %G: ISO week-calendar year (ISO 8601)
  // %V: ISO week number (ISO 8601).
  s = std::format("{:%G-V%V}", 2019y/December/last);
  VERIFY( s == "2020-V01" );
  s = std::format("{:%G-V%V}", 2023y/January/last);
  VERIFY( s == "2023-V05" );
  s = std::format("{:%G-V%V}", 2023y/December/last);
  VERIFY( s == "2023-V52" );

  s = std::format("{:%F}", 2023y/July/last);
  VERIFY( s == "2023-07-31" );
  s = std::format("{:%x}", 2023y/July/last);
  VERIFY( s == "07/31/23" );
  s = std::format("{:L%x}", 2023y/July/last);
  VERIFY( s == "07/31/23" );
  std::locale loc_fr(ISO_8859(15,fr_FR));
  s = std::format(loc_fr, "{:%x}", 2023y/July/last);
  VERIFY( s == "07/31/23" );
  s = std::format(loc_fr, "{:L%x}", 2023y/July/last);
  VERIFY( s == "31/07/2023" || s == "31.07.2023" ); // depends on locale defs
  s = std::format(loc_fr, "{:L}", 2023y/July/last);
  VERIFY( s == "2023/juil./last" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "aAbBCdDeFgGhjmuUVwWxyY";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      year_month_weekday ymw = 2023y/July/Thursday[2];
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(ymw));
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
  test_ostream();
  test_format();
}
