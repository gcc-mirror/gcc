// { dg-do run { target c++20 } }

// Bug 126364 - chrono::from_stream %T and %R short circuit on out of range
// values even when it doesn't fail the parse

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

using namespace std::chrono;

static bool check(const char* input, char const* fmt)
{
    std::istringstream is(input);
    year_month_day ymd{};
    return from_stream(is, fmt, ymd).good() && ymd.ok();
}

void
test_pr126364()
{
  // Accept out of range numbers for unused hours, minutes, and seconds.
  VERIFY( check("2026-07-31T25:36:57Z", "%FT%TZ") );  // hour 25 only
  VERIFY( check("2026-07-31T20:99:57Z", "%FT%TZ") );  // minute 99 only
  VERIFY( check("2026-07-31T20:36:99Z", "%FT%TZ") );  // second 99 only
  VERIFY( check("2026-07-31T25:36:57Z", "%FT%T") );   // hour 25 only, no Z
  VERIFY( check("2026-07-31T20:99:57Z", "%FT%T") );   // minute 99 only, no Z
  VERIFY( check("2026-07-31T20:36:99Z", "%FT%T") );   // second 99 only, no Z
  VERIFY( check("2026-07-31 25", "%F %H") );          // %H out of range alone
  VERIFY( check("2026-07-31 99", "%F %M") );          // %M out of range alone
}

void
test_invalid()
{
  // Do not accept non-numeric input for unused hours, minutes, and seconds.
  VERIFY( ! check("2026-07-31 xx", "%F %H") );
  VERIFY( ! check("2026-07-31 xx", "%F %M") );
  VERIFY( ! check("2026-07-31 xx", "%F %S") );
  VERIFY( ! check("2026-07-31 xx:20", "%F %R") );
  VERIFY( ! check("2026-07-31 10:xx", "%F %R") );
  VERIFY( ! check("2026-07-31 xx:20:30", "%F %T") );
  VERIFY( ! check("2026-07-31 10:xx:30", "%F %T") );
  VERIFY( ! check("2026-07-31 10:20:xx", "%F %T") );
}

int main()
{
  test_pr126364();
  test_invalid();
}
