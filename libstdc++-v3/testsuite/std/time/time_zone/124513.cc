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

void
test_mawson()
{
  using namespace std::chrono;

  std::ofstream("tzdata.zi") << R"(# version test1
Z Antarctica/Mawson 0 - -00 1954 F 13
                    6 - %z  2009 O 18 2
                    5 - %z
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test1" );

  auto zone = locate_zone("Antarctica/Mawson");
  auto info = zone->get_info(sys_days(2009y/October/19));
  VERIFY( info.begin == sys_days(2009y/October/17) + 20h);
}

void
test_custom()
{
  using namespace std::chrono;

  std::ofstream("tzdata.zi") << R"(# version test2
Z Test/Zomg 3:00:00 - LMT 1990 Mar 1 2
            4       - DST 1990 Oct 1 2
            3       - STD 1991 Mar 1 3:30
            3       1 DST 1991 Oct 1 3:45:01
            3       - STD 1992 Mar 1 -
            3       1 DST 1992 Oct 1 -u
            3       - STD 1993 Mar 1 -s
            3       2 DST 1993 Oct 1 -s
            3       - STD

Z Test/Zoinks 0:00  -    LMT 1990 Mar 1 2
              0:00  1:00 DST 1990 Oct 1 3
              0:00  -    STD 1991 Mar 1 1:30
              0:00  1:00 DST 1991 Oct 1 3:30
              0:00  -    STD
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test2" );

  const auto offset = 3h;

  const time_zone* zone;
  sys_info info;

  zone = locate_zone("Test/Zomg");
  info = zone->get_info(sys_days(1990y/June/1));
  VERIFY( info.begin == sys_days(1990y/March/1) + 2h - 3h);
  VERIFY( info.end   == sys_days(1990y/October/1) + 2h - 4h);

  info = zone->get_info(sys_days(1991y/June/1));
  VERIFY( info.begin == sys_days(1991y/March/1) + 3h + 30min - 3h);
  VERIFY( info.end   == sys_days(1991y/October/1) + 3h + 45min + 1s - 4h);

  info = zone->get_info(sys_days(1992y/June/1));
  VERIFY( info.begin == sys_days(1992y/March/1) - 3h);
  VERIFY( info.end   == sys_days(1992y/October/1));   // -u means midnight UTC

  info = zone->get_info(sys_days(1992y/November/1));
  VERIFY( info.begin == sys_days(1992y/October/1));    // -u means midnight UTC
  VERIFY( info.end   == sys_days(1993y/March/1) - 3h); // -s means midnight STD

  info = zone->get_info(sys_days(1993y/June/1));
  VERIFY( info.begin == sys_days(1993y/March/1) -3h);  // -s means midnight STD
  VERIFY( info.end   == sys_days(1993y/October/1) - 3h);


  zone = locate_zone("Test/Zoinks");
  info = zone->get_info(sys_days(1990y/June/1));
  VERIFY( info.begin == sys_days(1990y/March/1) + 2h);
  VERIFY( info.end   == sys_days(1990y/October/1) + 3h - 1h);

  info = zone->get_info(sys_days(1991y/June/1));
  VERIFY( info.begin == sys_days(1991y/March/1) + 1h + 30min);
  VERIFY( info.end   == sys_days(1991y/October/1) + 3h + 30min - 1h);

  info = zone->get_info(sys_days(1992y/June/1));
  VERIFY( info.begin == sys_days(1991y/October/1) + 3h + 30min - 1h);
}

int main()
{
  test_custom();
  test_mawson();
}
