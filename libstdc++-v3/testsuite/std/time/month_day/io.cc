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
  ss << January/14 << ' ' << February/30 << ' ' << March/34;
  VERIFY( ss.str() == "Jan/14 Feb/30 Mar/34 is not a valid day" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/27;
  VERIFY( ss.str() == "juil./27" );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
