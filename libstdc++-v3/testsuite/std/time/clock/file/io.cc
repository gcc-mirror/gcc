// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using namespace std::chrono;

  file_time<file_clock::duration> t = file_clock::now();
  std::ostringstream ss1, ss2;
  ss1 << floor<seconds>(t);
  ss2 << floor<seconds>(clock_cast<system_clock>(t));
  VERIFY( ss1.str() == ss2.str() );
}

int main()
{
  test_ostream();
}
