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
  for (int i = 1; i <= 12; ++i)
    ss << month(i);
  VERIFY( ss.str() == "JanFebMarAprMayJunJulAugSepOctNovDec" );
  ss.str("");
  ss << month(0) << '|' << month(13);
  VERIFY( ss.str() == "0 is not a valid month|13 is not a valid month" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << month(1);
  VERIFY( ss.str() == "janv." );
}

void
test_format()
{
  using std::chrono::month;

  auto s = std::format("{:%b%%%B%t%m%n}", month(1));
  VERIFY( s == "Jan%January\t01\n" );
  auto ws = std::format(L"{:%b%%%B%t%m%n}", month(12));
  VERIFY( ws == L"Dec%December\t12\n" );

  try
  {
    (void) std::format("{:%b}", month(13));
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }

  s = std::format("{} is OK, but {:L}", month(2), month(13));
  VERIFY( s == "Feb is OK, but 13 is not a valid month" );

  std::locale loc_fr(ISO_8859(15,fr_FR));

  s = std::format("{:%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(std::locale::classic(), "{:%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(std::locale::classic(), "{:L%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(loc_fr, "{:%Om}", month(1));
  VERIFY( s == "01" );
  s = std::format(loc_fr, "{:L%Om}", month(1));
  VERIFY( s == "01" );
  // TODO test "{:L%Om}" with locale that has alternative numeric rep.

  s = std::format(loc_fr, "{:%b}", month(1));
  VERIFY( s == "Jan" );
  s = std::format(loc_fr, "{:L%b}", month(1));
  VERIFY( s == "janv." );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "bBhm";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      (void) std::vformat(std::string_view(fmt, 5),
			  std::make_format_args(month(1)));
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
