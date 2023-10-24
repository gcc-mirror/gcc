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
  ss << 2023y/January/Monday[last];
  VERIFY( ss.str() == "2023/Jan/Mon[last]" );
  ss.clear();
  ss.str("");
  ss << 2023y/month(13)/Monday[last];
  VERIFY( ss.str() == "2023/13 is not a valid month/Mon[last]" );
  ss.clear();
  ss.str("");
  ss << 2023y/December/weekday(9)[last];
  VERIFY( ss.str() == "2023/Dec/9 is not a valid weekday[last]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 2023y/July/Thursday[last];
  VERIFY( ss.str() == "2023/juil./jeu.[last]" );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
