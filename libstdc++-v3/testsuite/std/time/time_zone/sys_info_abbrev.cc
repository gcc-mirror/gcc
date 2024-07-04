// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

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

using namespace std::chrono;

void
test_format()
{
  std::ofstream("tzdata.zi") << R"(# version test_1
Zone Africa/Bissau -1:2:20 - LMT 1912 Ja 1 1u
                   -1      - %z  1975
                   0       - GMT
Zon Some/Zone 1:2:3   - %z 1900
              1:23:45 - %z 1950
Zo Another/Zone 1:2:3 -     AZ0     1901
                1     Roolz A%sZ    2000
                1     Roolz SAZ/DAZ 2005
                1     Roolz %z
Rule Roolz 1950 max - April 1 2 1 D
Rul  Roolz 1950 max - Oct   1 1 0 S
Z Strange/Zone 1       - X%sX    1980
               1       - FOO/BAR 1990
               2:00    - %zzz    1995
               0:9     - %zzz    1996
               0:8:7   - %zzz    1997
               0:6:5.5 - %zzz    1998
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_1" );

  // Test formatting %z as
  auto tz = locate_zone("Africa/Bissau");
  auto inf = tz->get_info(sys_days(1974y/1/1));
  VERIFY( inf.abbrev == "-01" );

  tz = locate_zone("Some/Zone");
  inf = tz->get_info(sys_days(1899y/1/1));
  VERIFY( inf.abbrev == "+010203" );
  inf = tz->get_info(sys_days(1955y/1/1));
  VERIFY( inf.abbrev == "+012345" );

  tz = locate_zone("Another/Zone");
  // Test formatting %s as the LETTER/S field from the active Rule.
  inf = tz->get_info(sys_days(1910y/January/1));
  VERIFY( inf.abbrev == "ASZ" );
  inf = tz->get_info(sys_days(1950y/January/1));
  VERIFY( inf.abbrev == "ASZ" );
  inf = tz->get_info(sys_days(1950y/June/1));
  VERIFY( inf.abbrev == "ADZ" );
  inf = tz->get_info(sys_days(1999y/January/1));
  VERIFY( inf.abbrev == "ASZ" );
  inf = tz->get_info(sys_days(1999y/July/1));
  VERIFY( inf.abbrev == "ADZ" );
  // Test formatting STD/DST according to the active Rule.
  inf = tz->get_info(sys_days(2000y/January/2));
  VERIFY( inf.abbrev == "SAZ" );
  inf = tz->get_info(sys_days(2001y/January/1));
  VERIFY( inf.abbrev == "SAZ" );
  inf = tz->get_info(sys_days(2001y/July/1));
  VERIFY( inf.abbrev == "DAZ" );
  // Test formatting %z as the offset determined by the active Rule.
  inf = tz->get_info(sys_days(2005y/January/2));
  VERIFY( inf.abbrev == "+01" );
  inf = tz->get_info(sys_days(2006y/January/1));
  VERIFY( inf.abbrev == "+01" );
  inf = tz->get_info(sys_days(2006y/July/1));
  VERIFY( inf.abbrev == "+02" );

  // Test formatting %z, %s and S/D for a Zone with no associated Rules.
  tz = locate_zone("Strange/Zone");
  inf = tz->get_info(sys_days(1979y/January/1));
  VERIFY( inf.abbrev == "XX" ); // No Rule means nothing to use for %s.
  inf = tz->get_info(sys_days(1981y/July/1));
  VERIFY( inf.abbrev == "FOO" ); // Always standard time means first string.
  inf = tz->get_info(sys_days(1994y/July/1));
  VERIFY( inf.abbrev == "+02zz" );
  inf = tz->get_info(sys_days(1995y/July/1));
  VERIFY( inf.abbrev == "+0009zz" );
  inf = tz->get_info(sys_days(1996y/July/1));
  VERIFY( inf.abbrev == "+000807zz" );
  inf = tz->get_info(sys_days(1997y/July/1));
  VERIFY( inf.abbrev == "+000606zz" );
}

int main()
{
  test_format();
}
