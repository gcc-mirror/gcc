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

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
