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
  ss << January/last << ' ' << February/last << ' ' << March/last;
  VERIFY( ss.str() == "Jan/last Feb/last Mar/last" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/last;
  VERIFY( ss.str() == "juil./last" );
}

void
test_format()
{
  using namespace std::chrono;
  std::locale loc_fr(ISO_8859(15,fr_FR));

  auto s = std::format("{:%b%%%B%t%m%n}", month(3)/last);
  VERIFY( s == "Mar%March\t03\n" );
  s = std::format(loc_fr, "{:L%b%%%B%t%m%n}", month(3)/last);
  VERIFY( s == "mars%mars\t03\n");

  s = std::format("{0:%m/last} {0}", month(4)/last);
  VERIFY( s == "04/last Apr/last" );
  s = std::format("{0:%m/last} {0}", month(0)/last);
  VERIFY( s == "00/last 0 is not a valid month/last" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "bBhm";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      auto mdl = month(1)/last;
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(mdl));
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
