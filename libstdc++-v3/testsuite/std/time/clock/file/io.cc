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

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 20h + 44min;
  file_time<seconds> tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("002023-08-09 21:44 +01 BST!");
  VERIFY( is >> parse("%6F %R %z %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<file_clock>(expected) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );
}

int main()
{
  test_ostream();
  test_parse();
}
