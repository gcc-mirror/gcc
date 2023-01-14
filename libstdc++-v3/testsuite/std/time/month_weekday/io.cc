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
  ss << January/Saturday[2] << ' ' << February/Monday[5] << ' '
    << March/Sunday[8];
  VERIFY( ss.str() == "Jan/Sat[2] Feb/Mon[5] Mar/Sun[8 is not a valid index]" );

  ss.str("");
  ss.imbue(std::locale(ISO_8859(15,fr_FR)));
  ss << July/Thursday[4];
  VERIFY( ss.str() == "juil./jeu.[4]" );
}

int main()
{
  test_ostream();
  // TODO: test_format();
  // TODO: test_parse();
}
