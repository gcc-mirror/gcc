// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

// When two adjacent Zone lines differ in total offset and the new line's
// rule set has a rule firing within |jump| of the boundary (where jump
// is a backward local-time jump), zic.c's writezone folds that rule
// into the boundary, so the new line begins with the post-rule save.
//
// Mirrors America/Argentina/Buenos_Aires around 1999-10-03.

#include <chrono>
#include <fstream>
#include <testsuite_hooks.h>

static bool override_used = false;

namespace __gnu_cxx
{
  const char* zoneinfo_dir_override() {
    override_used = true;
    return "./";
  }
}

int
main()
{
  using namespace std::chrono;

  // stdoff jumps from -3 to -4 at the same instant a save=1 rule fires.
  // In the new (-4) frame the rule fires 1 hour after the boundary at
  // UT 03:00, so the merge folds the rule into the boundary and the
  // new line begins at offset=-3, save=1 (abbrev "-03").
  std::ofstream("tzdata.zi") << R"(# version test_zone_merge
R T 1999 o - O 3 0 1 -
R T 2000 o - Mar 3 0 0 -
Z Test/BA -3 -  %z  1999 O 3
          -4 T  %z  2000 Mar 3
          -3 -  %z
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used );
  VERIFY( db.version == "test_zone_merge" );

  auto* tz = locate_zone("Test/BA");

  // The boundary is the wall UNTIL "1999 O 3" (default time 00:00)
  // interpreted in the prior (-3) frame, i.e. UT 03:00 1999-10-03.
  sys_seconds boundary{sys_days(1999y/October/3) + 3h};

  auto before = tz->get_info(boundary - 1s);
  VERIFY( before.offset == -3h );
  VERIFY( before.save == 0min );
  VERIFY( before.abbrev == "-03" );

  // The new line's first sys_info already has save=1 from the merge,
  // total offset -3h, abbrev "-03".
  auto at_boundary = tz->get_info(boundary);
  VERIFY( at_boundary.offset == -3h );
  VERIFY( at_boundary.save == 60min );
  VERIFY( at_boundary.abbrev == "-03" );

  auto plus_30min = tz->get_info(boundary + 30min);
  VERIFY( plus_30min.offset == -3h );
  VERIFY( plus_30min.save == 60min );
  VERIFY( plus_30min.abbrev == "-03" );

  // Sanity: well after the boundary, still in the merged sys_info
  // until the Mar 3 2000 transition.
  auto winter = tz->get_info(sys_days(2000y/January/15));
  VERIFY( winter.offset == -3h );
  VERIFY( winter.save == 60min );
  VERIFY( winter.abbrev == "-03" );

  // After Mar 3 2000: line 2 ends, line 3 begins.  No DST rule fires
  // at this boundary, so total offset reverts to -3h with save=0.
  auto spring = tz->get_info(sys_days(2000y/April/15));
  VERIFY( spring.offset == -3h );
  VERIFY( spring.save == 0min );
  VERIFY( spring.abbrev == "-03" );
}
