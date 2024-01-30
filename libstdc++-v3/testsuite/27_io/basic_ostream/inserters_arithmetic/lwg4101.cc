// { dg-do run }
// LWG 4101. LWG 117 loses the sign for negative NaN on some architectures

#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

int main()
{
  float nan = std::numeric_limits<float>::quiet_NaN();
  std::ostringstream os;
  os << -nan;
  VERIFY( os.str()[0] == '-' );
}
