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
  for (int i = 0; i <= 7; ++i)
    ss << weekday(i);
  VERIFY( ss.str() == "SunMonTueWedThuFriSatSun" );
  ss.str("");
  ss << weekday(8) << '|' << weekday(99);
  VERIFY( ss.str() == "8 is not a valid weekday|99 is not a valid weekday" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << weekday(6);
  VERIFY( ss.str() == "sam." );
}

void
test_format()
{
  using std::chrono::weekday;

  auto s = std::format("{:%a%%%A%t%u%n%w}", std::chrono::Monday);
  VERIFY( s == "Mon%Monday\t1\n1" );
  auto ws = std::format(L"{:%a%%%A%t%u%n%w}", weekday(7));
  VERIFY( ws == L"Sun%Sunday\t7\n0" );

  s = std::format("{:%w}", weekday(8));
  VERIFY( s == "8" );

  try
  {
    (void) std::format("{:%a}", weekday(8));
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }

  s = std::format("{} is OK, but {:L}", weekday(2), weekday(13));
  VERIFY( s == "Tue is OK, but 13 is not a valid weekday" );

  std::locale loc_fr(ISO_8859(15,fr_FR));

  s = std::format("{:%Ow}", weekday(1));
  VERIFY( s == "1" );
  s = std::format(std::locale::classic(), "{:%Ow}", weekday(1));
  VERIFY( s == "1" );
  s = std::format(std::locale::classic(), "{:L%Ow}", weekday(1));
  VERIFY( s == "1" );
  s = std::format(loc_fr, "{:%Ow}", weekday(1));
  VERIFY( s == "1" );
  s = std::format(loc_fr, "{:L%Ow}", weekday(1));
  VERIFY( s == "1" );
  // TODO test "{:L%Ow}" with locale that has alternative numeric rep.

  s = std::format(loc_fr, "{:%a}", weekday(1));
  VERIFY( s == "Mon" );
  s = std::format(loc_fr, "{:L%a}", weekday(1));
  VERIFY( s == "lun." );

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "aAuw";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      (void) std::vformat(std::string_view(fmt, 5),
			  std::make_format_args(weekday(1)));
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
