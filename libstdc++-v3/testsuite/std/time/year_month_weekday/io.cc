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
  ss << 2023y/January/Saturday[2];
  VERIFY( ss.str() == "2023/Jan/Sat[2]" );
  ss.clear();
  ss.str("");
  ss << 2023y/month(13)/Monday[1];
  VERIFY( ss.str() == "2023/13 is not a valid month/Mon[1]" );
  ss.clear();
  ss.str("");
  ss << 2023y/December/weekday(9)[5];
  VERIFY( ss.str() == "2023/Dec/9 is not a valid weekday[5]" );
  ss.clear();
  ss.str("");
  ss << 2023y/December/Monday[6];
  VERIFY( ss.str() == "2023/Dec/Mon[6 is not a valid index]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 2023y/July/Thursday[2];
  VERIFY( ss.str() == "2023/juil./jeu.[2]" );
}

void
test_format()
{
  using namespace std::chrono;

  std::string s = std::format("{:%Y%t%C%%%y%n%j %b %a}", 2024y/January/Friday[2]);
  VERIFY( s == "2024\t20%24\n012 Jan Fri" );
  std::wstring ws = std::format(L"{:%Y%t%C%%%y%n%j %b %a}", 2024y/December/Monday[1]);
  VERIFY( ws == L"2024\t20%24\n337 Dec Mon" );

  s = std::format("{0:%Y-%m-%d} {0}", 2023y/May/Monday[1]);
  VERIFY( s == "2023-05-01 2023/May/Mon[1]" );
  s = std::format("{0:%Y-%m-%d} {0}", 2023y/month(13)/Monday[1]);
  VERIFY( s == "2023-13-01 2023/13 is not a valid month/Mon[1]" );

  s = std::format("{:%u %w}", Monday[1]);
  VERIFY( s == "1 1" );
  s = std::format("{:%u %w}", Sunday[2]);
  VERIFY( s == "7 0" );
  // 0 and 7 are both Sundays
  s = std::format("{:%u %w}", weekday(0)[1]);
  VERIFY( s == "7 0" );
  s = std::format("{:%u %w}", weekday(7)[1]);
  VERIFY( s == "7 0" );
  s = std::format("{:%u %w}", weekday(9)[1]);
  VERIFY( s == "9 9" );

  s = std::format("{:%Y-%m-%d %j}", 2024y/February/Thursday[5]);
  VERIFY( s == "2024-02-29 060" );
  s = std::format("{:%Y-%m-%d %j}", 2023y/February/Tuesday[4]);
  VERIFY( s == "2023-02-28 059" );
  s = std::format("{:%Y-%m-%d %j}", 2024y/September/Sunday[1]);
  VERIFY( s == "2024-09-01 245" );
  s = std::format("{:%Y-%m-%d %j}", 2024y/September/Sunday[5]);
  VERIFY( s == "2024-09-29 273" );
  // first weeks of next month
  s = std::format("{:%Y-%m-%d %j}", 2024y/September/Sunday[6]);
  VERIFY( s == "2024-09-36 280" );
  s = std::format("{:%Y-%m-%d %j}", 2024y/September/Sunday[7]);
  VERIFY( s == "2024-09-43 287" );
  // last week on previous month
  s = std::format("{:%Y-%m-%d %j}", 2024y/September/Saturday[0]);
  VERIFY( s == "2024-09-00 244" );
  s = std::format("{:%Y-%m-%d %j}", 2024y/September/Sunday[0]);
  VERIFY( s == "2024-09-00 238" ); // day is de-facto -6

  // %U: Week number for weeks starting on Sunday
  s = std::format("{:%Y-U%U}", 2023y/January/Sunday[0]);
  VERIFY( s == "2023-U00" );
  s = std::format("{:%Y-U%U}", 2023y/January/Sunday[1]);
  VERIFY( s == "2023-U01" );
  s = std::format("{:%Y-U%U}", 2023y/January/Sunday[4]);
  VERIFY( s == "2023-U04" );
  s = std::format("{:%Y-U%U}", 2023y/January/Sunday[7]);
  VERIFY( s == "2023-U07" );
  s = std::format("{:%Y-U%U}", 2023y/December/Sunday[0]);
  VERIFY( s == "2023-U48" );
  s = std::format("{:%Y-U%U}", 2023y/December/Sunday[1]);
  VERIFY( s == "2023-U49" );
  s = std::format("{:%Y-U%U}", 2023y/December/Sunday[4]);
  VERIFY( s == "2023-U52" );
  s = std::format("{:%Y-U%U}", 2023y/December/Sunday[7]);
  VERIFY( s == "2023-U55" );
  // %W: Week number for weeks starting on Monday
  s = std::format("{:%Y-W%W}", 2023y/January/Monday[0]);
  VERIFY( s == "2023-W00" );
  s = std::format("{:%Y-W%W}", 2023y/January/Monday[1]);
  VERIFY( s == "2023-W01" );
  s = std::format("{:%Y-W%W}", 2023y/January/Monday[4]);
  VERIFY( s == "2023-W04" );
  s = std::format("{:%Y-W%W}", 2023y/January/Monday[7]);
  VERIFY( s == "2023-W07" );
  s = std::format("{:%Y-W%W}", 2023y/December/Monday[0]);
  VERIFY( s == "2023-W48" );
  s = std::format("{:%Y-W%W}", 2023y/December/Monday[1]);
  VERIFY( s == "2023-W49" );
  s = std::format("{:%Y-W%W}", 2023y/December/Monday[4]);
  VERIFY( s == "2023-W52" );
  s = std::format("{:%Y-W%W}", 2023y/December/Monday[7]);
  VERIFY( s == "2023-W55" );
  // First Sunday precedes first Monday, so happens on
  // last week of previous month and thus year
  s = std::format("{:%Y-W%W}", 2023y/January/Sunday[1]);
  VERIFY( s == "2023-W00" );
  // Last Sunday of pevious year happens on second to
  // last week on previous year
  s = std::format("{:%Y-W%W}", 2023y/January/Sunday[0]);
  VERIFY( s == "2023-W99" );

  // %G: ISO week-calendar year (ISO 8601)
  // %V: ISO week number (ISO 8601).
  s = std::format("{:%G-V%V}", 2023y/January/Monday[0]);
  VERIFY( s == "2022-V52" );
  s = std::format("{:%G-V%V}", 2023y/January/Monday[1]);
  VERIFY( s == "2023-V01" );
  s = std::format("{:%G-V%V}", 2023y/January/Monday[4]);
  VERIFY( s == "2023-V04" );
  s = std::format("{:%G-V%V}", 2023y/January/Monday[7]);
  VERIFY( s == "2023-V07" );
  s = std::format("{:%G-V%V}", 2023y/December/Friday[0]);
  VERIFY( s == "2023-V47" );
  s = std::format("{:%G-V%V}", 2023y/December/Friday[1]);
  VERIFY( s == "2023-V48" );
  s = std::format("{:%G-V%V}", 2023y/December/Friday[5]);
  VERIFY( s == "2023-V52" );
  s = std::format("{:%G-V%V}", 2023y/December/Friday[6]);
  VERIFY( s == "2024-V01" );

  s = std::format("{:%F}", 2023y/July/Thursday[2]);
  VERIFY( s == "2023-07-13" );
  s = std::format("{:%x}", 2023y/July/Thursday[2]);
  VERIFY( s == "07/13/23" );
  s = std::format("{:L%x}", 2023y/July/Thursday[2]);
  VERIFY( s == "07/13/23" );
  std::locale loc_fr(ISO_8859(15,fr_FR));
  s = std::format(loc_fr, "{:%x}", 2023y/July/Thursday[2]);
  VERIFY( s == "07/13/23" );
  s = std::format(loc_fr, "{:L%x}", 2023y/July/Thursday[2]);
  VERIFY( s == "13/07/2023" || s == "13.07.2023" ); // depends on locale defs
  s = std::format(loc_fr, "{:L}", 2023y/July/Thursday[2]);
  VERIFY( s == "2023/juil./jeu.[2]" );

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
