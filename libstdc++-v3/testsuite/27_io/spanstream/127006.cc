// { dg-do run { target c++23 } }

#include <spanstream>
#include <testsuite_hooks.h>

void
test_pr127006_out()
{
  // PR libstdc++/127006
  char buf[3]{};
  std::ospanstream s(buf, std::ios::openmode{});
  VERIFY( s << 'x' );
  VERIFY( buf[0] == 'x' );
}

void
test_pr127006_in()
{
  char buf[3]{'x'};
  std::ispanstream s(buf, std::ios::openmode{});
  char x;
  VERIFY( s >> x );
  VERIFY( x == 'x' );
}

int main()
{
  test_pr127006_out();
  test_pr127006_in();
}
