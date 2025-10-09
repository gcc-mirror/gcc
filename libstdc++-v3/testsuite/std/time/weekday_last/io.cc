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
  ss << Monday[last] << ' ' << Wednesday[last] << ' ' << weekday(9)[last];
  VERIFY( ss.str() == "Mon[last] Wed[last] 9 is not a valid weekday[last]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << Saturday[last];
  VERIFY( ss.str() == "sam.[last]" );
}

void
test_format()
{
  using namespace std::chrono;
  std::locale loc_fr(ISO_8859(15,fr_FR));

  auto s = std::format("{:%a%%%A%t%u%n%w}", weekday(5)[last]);
  VERIFY( s == "Fri%Friday\t5\n5" );
  s = std::format(loc_fr, "{:L%a%%%A%t%u%n%w}", weekday(5)[last]);
  VERIFY( s == "ven.%vendredi\t5\n5");

  s = std::format("{0:%w[last]} {0}", weekday(6)[last]);
  VERIFY( s == "6[last] Sat[last]" );
  s = std::format("{0:%w[last]} {0}", weekday(9)[last]);
  VERIFY( s == "9[last] 9 is not a valid weekday[last]" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "aAuw";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      auto wl = weekday(1)[last];
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(wl));
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
