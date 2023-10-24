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
  ss << 2023y/January/last << ' ' << 2023y/month(13)/last;
  VERIFY( ss.str() == "2023/Jan/last 2023/13 is not a valid month/last" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 2023y/July/last;
  VERIFY( ss.str() == "2023/juil./last" );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
