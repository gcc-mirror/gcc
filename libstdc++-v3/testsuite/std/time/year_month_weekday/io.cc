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

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
