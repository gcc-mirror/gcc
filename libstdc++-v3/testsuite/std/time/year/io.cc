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
  for (int y : {-1234, -44, -1, 0, 5, 32, 325, 1066, 2022})
    ss << year(y) << ' ';
  VERIFY( ss.str() == "-1234 -0044 -0001 0000 0005 0032 0325 1066 2022 " );
  ss.str("");
  ss << year::min() << ' ' << year::max() << ' ' << --year::min();
  VERIFY( ss.str() == "-32767 32767 -32768 is not a valid year" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 1789y;
  VERIFY( ss.str() == "1789" );
}

void
test_format()
{
  using std::chrono::year;
  using namespace std::chrono_literals;

  auto s = std::format("{:%y%%%Y%t%C%n}", 2022y);
  VERIFY( s == "22%2022\t20\n" );
  auto ws = std::format(L"{:%y%%%Y%t%C%n}", 2023y);
  VERIFY( ws == L"23%2023\t20\n" );

  s = std::format("{:%Y}", --year::min());
  VERIFY( s == "-32768" );

  s = std::format("{}", --year::min()); // formatted via ostream
  VERIFY( s == "-32768 is not a valid year" );

  s = std::format("{:%y} {:%y}", 1976y, -1976y);
  VERIFY( s == "76 76" ); // LWG 3831

  s = std::format("{0:%EC}{0:%Ey} = {0:%EY}", 1642y);
  VERIFY( s == "1642 = 1642" );
  s = std::format("{0:L%EC}{0:L%Ey} = {0:L%EY}", 1642y);
  VERIFY( s == "1642 = 1642" );
  s = std::format(std::locale::classic(), "{0:L%EC}{0:L%Ey} = {0:L%EY}", 1642y);
  VERIFY( s == "1642 = 1642" );

  // TODO test "{:L%EC}" with locale that has alternative era rep.
  // TODO test "{:L%Ey}" with locale that has alternative year rep.
  // TODO test "{:L%EY}" with locale that has alternative year rep.
  // TODO test "{:L%Oy}" with locale that has alternative numeric rep.

  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view my_specs = "CyY";
  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      (void) std::vformat(std::string_view(fmt, 5),
			  std::make_format_args(year(2022)));
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
