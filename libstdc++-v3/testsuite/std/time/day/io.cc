// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-namedlocale "fr_FR.ISO8859-15" }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using std::ostringstream;
  using namespace std::chrono;

  ostringstream ss;
  ss << day(1) << ' ' << day(11) << ' ' << day(21) << ' ' << day(31)
    << ' ' << day(41);
  auto s = ss.str();
  VERIFY( s == "01 11 21 31 41 is not a valid day" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << day(1);
  VERIFY( ss.str() == "01" );
}

void
test_format()
{
  using std::chrono::day;

  auto s = std::format("{:%d%%%e%t}{:%d%%%e%n}", day(1), day(11));
  VERIFY( s == "01% 1\t11%11\n" );
  auto ws = std::format(L"{:%d%%%e%t}{:%d%%%e%n}", day(1), day(11));
  VERIFY( ws == L"01% 1\t11%11\n" );

  VERIFY( std::format("{} {}", day(8), day(0)) == "08 00 is not a valid day" );

  s = std::format("{:%Od}", day(1));
  VERIFY( s == "01" );
  s = std::format(std::locale::classic(), "{:%Od}", day(1));
  VERIFY( s == "01" );
  s = std::format(std::locale::classic(), "{:L%Od}", day(1));
  VERIFY( s == "01" );
  // TODO test "{:L%Od}" with locale that has alternative numeric rep.

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "de";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      (void) std::vformat(std::string_view(fmt, 5),
			  std::make_format_args(day(1)));
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
  // TODO: test_parse();
}
