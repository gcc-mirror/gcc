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
  ss << January/last << ' ' << February/last << ' ' << March/last;
  VERIFY( ss.str() == "Jan/last Feb/last Mar/last" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/last;
  VERIFY( ss.str() == "juil./last" );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
