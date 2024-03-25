// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
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

void
test_format()
{
  using namespace std::chrono;

  auto s = std::format("{}", hh_mm_ss{1h + 23min + 45s});
  VERIFY( s == "01:23:45" );
  s = std::format("{}", hh_mm_ss{-42min});
  VERIFY( s == "-00:42:00" );

  auto ws = std::format(L"{}", hh_mm_ss{1h + 23min + 45s});
  VERIFY( ws == L"01:23:45" );
  ws = std::format(L"{}", hh_mm_ss{-42min});
  VERIFY( ws == L"-00:42:00" );

  // Locale-specific formats:
  auto loc = std::locale::classic();
  s = std::format(loc, "{:%r %OH:%OM:%OS}", hh_mm_ss{123456ms});
  VERIFY( s == "12:02:03 AM 00:02:03" );
  ws = std::format(loc, L"{:%r %OH:%OM:%OS}", hh_mm_ss{123456ms});
  VERIFY( ws == L"12:02:03 AM 00:02:03" );
}

int main()
{
  test_ostream();
  test_format();
}
