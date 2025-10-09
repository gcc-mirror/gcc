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
  ss << January/Saturday[last] << ' ' << February/Monday[last] << ' '
    << March/weekday(9)[last];
  VERIFY( ss.str() == "Jan/Sat[last] Feb/Mon[last] Mar/9 is not a valid weekday[last]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/Thursday[last];
  VERIFY( ss.str() == "juil./jeu.[last]" );
}

void
test_format()
{
  using namespace std::chrono;
  std::locale loc_fr(ISO_8859(15,fr_FR));

  auto s = std::format("{:%b%%%B%t%m%n %a%%%A%t%u%n%w}", month(6)/weekday(2)[last]);
  VERIFY( s == "Jun%June\t06\n Tue%Tuesday\t2\n2" );
  s = std::format(loc_fr, "{:L%b%%%B%t%m%n %a%%%A%t%u%n%w}", month(6)/weekday(2)[last]);
  VERIFY( s == "juin%juin\t06\n mar.%mardi\t2\n2");

  s = std::format("{0:%m/%w[last]} {0}", month(8)/weekday(7)[last]);
  VERIFY( s == "08/0[last] Aug/Sun[last]" );
  s = std::format("{0:%m/%w[last]} {0}", month(70)/weekday(9)[last]);
  VERIFY( s == "70/9[last] 70 is not a valid month/9 is not a valid weekday[last]" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "aAbBhmuw";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      auto mwl = month(1)/weekday(1)[last];
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(mwl));
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
