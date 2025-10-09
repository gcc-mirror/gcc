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
  ss << Monday[2] << ' ' << Wednesday[5] << ' ' << Friday[13];
  VERIFY( ss.str() == "Mon[2] Wed[5] Fri[13 is not a valid index]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << Saturday[1];
  VERIFY( ss.str() == "sam.[1]" );
}

void
test_format()
{
  using namespace std::chrono;
  std::locale loc_fr(ISO_8859(15,fr_FR));

  auto s = std::format("{:%a%%%A%t%u%n%w}", weekday(7)[3]);
  VERIFY( s == "Sun%Sunday\t7\n0" );
  s = std::format(loc_fr, "{:L%a%%%A%t%u%n%w}", weekday(7)[3]);
  VERIFY( s == "dim.%dimanche\t7\n0");

  s = std::format("{0:%w[]} {0}", weekday(4)[4]);
  VERIFY( s == "4[] Thu[4]" );
  s = std::format("{0:%w[]} {0}", weekday(10)[7]);
  VERIFY( s == "10[] 10 is not a valid weekday[7 is not a valid index]" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "aAuw";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      auto wi = weekday(1)[1];
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(wi));
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
