// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

// Africa/Algiers 1977-10-21: a Zone line whose RULES references a
// named Rule and whose UNTIL is a wall-time expression.  The wall
// UNTIL is interpreted using the SAVE value in force just before the
// boundary (the May-6 rule's save=1, not the Oct-21 rule's save=0
// even though the Oct-21 rule fires at the same wall instant).
//
//   Rule d 1977 May  6 0:00 wall  save=1
//   Rule d 1977 Oct 21 0:00 wall  save=0
//   Z A    0 d WE%sT 1977 O 21
//          1 d CE%sT

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

  std::ofstream("tzdata.zi") << R"(# version test_pr116110_named
R d 1977 o - May  6 0 1 S
R d 1977 o - O   21 0 0 -
Z Test/Algiers 0 d WE%sT 1977 O 21
               1 d CE%sT
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used );
  VERIFY( db.version == "test_pr116110_named" );

  auto* tz = locate_zone("Test/Algiers");

  // Just before the boundary: still in the first Zone line under
  // the May-6 rule (save=1, WEST, total +1).
  auto pre = tz->get_info(sys_days{1977y/October/20} + 22h);
  VERIFY( pre.offset == 1h );
  VERIFY( pre.save == 1h );
  VERIFY( pre.abbrev == "WEST" );

  // The boundary is Oct 20 23:00 UTC (= wall 00:00 - stdoff(0) - save(1)).
  // At and after the boundary we are in the second line (CET).
  auto at = tz->get_info(sys_days{1977y/October/20} + 23h);
  VERIFY( at.offset == 1h );    // stdoff 1 + save 0 (CET, second line)
  VERIFY( at.save == 0min );
  VERIFY( at.abbrev == "CET" );

  // A second query inside the second line, well clear of the boundary.
  auto after = tz->get_info(sys_days{1977y/October/21} + 12h);
  VERIFY( after.offset == 1h );
  VERIFY( after.save == 0min );
  VERIFY( after.abbrev == "CET" );

  // A query inside the [Oct 20 23:00, Oct 21 00:00] UTC window must be
  // in the second line, not in a leftover stretch from the first line.
  auto window = tz->get_info(sys_days{1977y/October/20} + 23h + 30min);
  VERIFY( window.offset == 1h );
  VERIFY( window.abbrev == "CET" );
}
