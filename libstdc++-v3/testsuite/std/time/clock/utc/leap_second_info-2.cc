// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

#include <chrono>
#include <fstream>
#include <testsuite_hooks.h>

using namespace std::chrono_literals;

static bool override_used = false;

namespace __gnu_cxx
{
  const char* zoneinfo_dir_override() {
    override_used = true;
    return "./";
  }
}

void
test_known_leaps()
{
  // Test some values within the list of known leap seconds.
  auto s = std::chrono::utc_seconds(-1s);
  auto lsi = get_leap_second_info(s);
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == 0s );

  s = std::chrono::utc_seconds(126230402s); // 1 Jan 1974
  lsi = get_leap_second_info(s);
  VERIFY( lsi.is_leap_second == true );
  VERIFY( lsi.elapsed == 3s );

  s = std::chrono::utc_seconds(1483228826s); // 1 Jan 2017
  lsi = get_leap_second_info(s);
  VERIFY( lsi.is_leap_second == true );
  VERIFY( lsi.elapsed == 27s );

  // We should not have checked the filesystem for the times above.
  VERIFY( ! override_used );
}

void
test_future_leaps()
{
  // XXX adjust this if real leap seconds are added to the hardcoded lists:
  const auto hardcoded_count = 27s;

  // Make sure there's no old file from a previous test run.
  std::ofstream("leapseconds") << "";

  using std::chrono::years;
  auto s = std::chrono::utc_seconds(1483228826s + years(100)); // 2117
  auto lsi = get_leap_second_info(s);
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == hardcoded_count );
  lsi = get_leap_second_info(s + years(10));
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == hardcoded_count );

  std::ofstream("leapseconds") << R"(
# No need to repeat the real leap seconds here, they're hardcoded in the lib.
# These are fake new leap seconds for testing purposes:
Leap	2099	Dec	31	23:59:60	+	S
Leap	2120	Jun	30	23:59:59	-	S
Leap	2120	Dec	30	23:59:59	-	S
)";

  override_used = false;
  std::chrono::reload_tzdb();
  VERIFY( override_used );

  // Check the same dates again using the custom leapseconds file:
  lsi = get_leap_second_info(s);
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == hardcoded_count + 1s ); // One positive leap second.
  lsi = get_leap_second_info(s + years(10));
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == hardcoded_count - 1s ); // Two negative leap seconds.

  // Overwrite the custom file again:
  override_used = false;
  std::ofstream("leapseconds") << "";

  // This should not re-read the custom file, the head of the tzdb_list
  // should already have been populated by calling get_leap_second_info(s):
  auto& tzdb = std::chrono::get_tzdb();
  // The file was not read again:
  VERIFY( ! override_used );
  // The list in the tzdb contains the three fake leap seconds:
  VERIFY( tzdb.leap_seconds.size() == size_t(hardcoded_count.count() + 3) );
  // And repeating the queries above gives the same results:
  lsi = get_leap_second_info(s);
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == hardcoded_count + 1s ); // One positive leap second.
  lsi = get_leap_second_info(s + years(10));
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == hardcoded_count - 1s ); // Two negative leap seconds.
}

int main()
{
  // chrono::reload_tzdb() requires ./tzdata.zi to be present.
  std::ofstream("tzdata.zi") << "# version empty\n";

  test_known_leaps();
  test_future_leaps();
}
