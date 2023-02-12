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
  ss << January/Saturday[last] << ' ' << February/Monday[last] << ' '
    << March/weekday(9)[last];
  VERIFY( ss.str() == "Jan/Sat[last] Feb/Mon[last] Mar/9 is not a valid weekday[last]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/Thursday[last];
  VERIFY( ss.str() == "juil./jeu.[last]" );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
