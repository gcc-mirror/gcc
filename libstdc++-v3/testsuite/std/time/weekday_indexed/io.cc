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

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
