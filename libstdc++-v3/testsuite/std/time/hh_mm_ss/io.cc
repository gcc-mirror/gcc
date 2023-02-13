// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  using std::ostringstream;
  using std::chrono::hh_mm_ss;
  using namespace std::chrono_literals;

  std::locale::global(std::locale::classic());

  {
    hh_mm_ss hms{-4083007ms};
    ostringstream out;
    out << hms;
    VERIFY( out.str() == "-01:08:03.007" );
  }

  {
    hh_mm_ss hms{4083007ms};
    ostringstream out;
    out << hms;
    VERIFY( out.str() == "01:08:03.007" );
  }

  {
    hh_mm_ss hms{65745123ms};
    ostringstream out;
    out << hms;
    VERIFY( out.str() == "18:15:45.123" );
  }

  ostringstream out;
  out << hh_mm_ss{65745s};
  VERIFY( out.str() == "18:15:45" );
}

int main()
{
  test01();
}
