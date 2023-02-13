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
  ss << 2023y/January << ' ' << 2023y/month(13);
  VERIFY( ss.str() == "2023/Jan 2023/13 is not a valid month" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << 2023y/July;
  VERIFY( ss.str() == "2023/juil." );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
