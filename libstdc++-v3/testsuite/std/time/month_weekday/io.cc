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
  ss << January/Saturday[2] << ' ' << February/Monday[5] << ' '
    << March/Sunday[8];
  VERIFY( ss.str() == "Jan/Sat[2] Feb/Mon[5] Mar/Sun[8 is not a valid index]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/Thursday[4];
  VERIFY( ss.str() == "juil./jeu.[4]" );
}

void
test_format()
{
  using namespace std::chrono;
  std::locale loc_fr(ISO_8859(15,fr_FR));

  auto s = std::format("{:%b%%%B%t%m%n %a%%%A%t%u%n%w}", month(5)/weekday(1)[2]);
  VERIFY( s == "May%May\t05\n Mon%Monday\t1\n1" );
  s = std::format(loc_fr, "{:L%b%%%B%t%m%n %a%%%A%t%u%n%w}", month(5)/weekday(1)[2]);
  VERIFY( s == "mai%mai\t05\n lun.%lundi\t1\n1");

  s = std::format("{0:%m/%u[]} {0}", month(9)/weekday(0)[2]);
  VERIFY( s == "09/7[] Sep/Sun[2]" );
  s = std::format("{0:%m/%u[]} {0}", month(111)/weekday(8)[0]);
  VERIFY( s == "111/8[] 111 is not a valid month/8 is not a valid weekday[0 is not a valid index]" );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "aAbBhmuw";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      auto mwi = month(1)/weekday(1)[1];
      (void) std::vformat(std::string_view(fmt, 5), std::make_format_args(mwi));
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
