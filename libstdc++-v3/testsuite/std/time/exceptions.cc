// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }

#include <chrono>
#include <testsuite_hooks.h>

void
test_nonexistent()
{
    std::string expected
      = "2016-03-13 02:30:00 is in a gap between\n"
	"2016-03-13 02:00:00 EST and\n"
	"2016-03-13 03:00:00 EDT which are both equivalent to\n"
	"2016-03-13 07:00:00 UTC";

  using namespace std::chrono;
  try {
    auto zt = zoned_time{"America/New_York",
			 local_days{Sunday[2]/March/2016} + 2h + 30min};
    VERIFY(false);
  } catch (const nonexistent_local_time& e) {
    VERIFY( e.what() == expected );
  }
}

void
test_ambiguous()
{
    std::string expected
      = "2016-11-06 01:30:00 is ambiguous.  It could be\n"
	"2016-11-06 01:30:00 EDT == 2016-11-06 05:30:00 UTC or\n"
	"2016-11-06 01:30:00 EST == 2016-11-06 06:30:00 UTC";

  using namespace std::chrono;
  try {
    auto zt = zoned_time{"America/New_York",
			 local_days{Sunday[1]/November/2016} + 1h + 30min};
    VERIFY(false);
  } catch (const ambiguous_local_time& e) {
    VERIFY( e.what() == expected );
  }
}

int main()
{
  test_nonexistent();
  test_ambiguous();
}
