// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

// When a Zone line specifies a numeric value as its RULES field, that
// value is the constant DST save value for that zone line.  Per
// [time.zone.info.sys] sys_info::offset is the total UTC offset
// (stdoff + save).

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

  std::ofstream("tzdata.zi") << R"(# version test_numeric_save
Z Test/Gaborone 2 -  CAT  1943 Sep 19 2
                2 1  CAST 1944 Mar 19 2
                2 -  CAT
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_numeric_save" );

  auto* tz = locate_zone("Test/Gaborone");

  // Sample well inside the CAST (numeric-save) zone line.
  auto info = tz->get_info(sys_days(1943y/December/15));
  VERIFY( info.offset == 3h );        // stdoff +2h + save +1h
  VERIFY( info.save == 60min );
  VERIFY( info.abbrev == "CAST" );

  // Bordering zone lines should report the standard offset with save 0.
  auto before = tz->get_info(sys_days(1943y/September/1));
  VERIFY( before.offset == 2h );
  VERIFY( before.save == 0min );
  VERIFY( before.abbrev == "CAT" );

  auto after = tz->get_info(sys_days(1944y/April/15));
  VERIFY( after.offset == 2h );
  VERIFY( after.save == 0min );
  VERIFY( after.abbrev == "CAT" );
}
