// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

// The DAY portion of a Zone line's UNTIL field accepts not only a
// numeric day-of-month but also "lastXxx" (last weekday in the month)
// and "Xxx<=N" / "Xxx>=N" forms, just like the ON field of a Rule line.
//
// Real-world example: Europe/Simferopol has
//   3 - MSK 1997 Mar lastSu 1u
// which places the boundary on 1997-03-30 (the last Sunday of March).

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
test_lastsu()
{
  using namespace std::chrono;

  std::ofstream("tzdata.zi") << R"(# version test_lastsu
Z Test/LastSu 3 - MSK 1997 Mar lastSu 1u
              3 - X
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_lastsu" );

  auto* tz = locate_zone("Test/LastSu");

  // True boundary: 1997-03-30 01:00 UTC (lastSu of March 1997 is Mar 30,
  // and the indicator is 'u' = Universal so no offset adjustment).
  sys_seconds boundary = sys_days{1997y/March/30} + 1h;

  // Just before: still in the MSK line.
  auto before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "MSK" );
  VERIFY( before.offset == 3h );

  // At/after the boundary: in the X line.
  auto at = tz->get_info(boundary);
  VERIFY( at.abbrev == "X" );

  // Check that the lastSu day is parsed correctly, and not defaulted
  // to the 1st: a March 15 query must still be in the MSK line.
  auto mid_march = tz->get_info(sys_days{1997y/March/15});
  VERIFY( mid_march.abbrev == "MSK" );
  VERIFY( mid_march.offset == 3h );
}

void
test_sun_ge_n()
{
  using namespace std::chrono;

  std::ofstream("tzdata.zi") << R"(# version test_sun_ge_n
Z Test/SunGE 0 - A 1990 Jun Sun>=8 0u
             0 - B
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_sun_ge_n" );

  auto* tz = locate_zone("Test/SunGE");

  // First Sunday >= June 8 1990 = June 10 (June 8 1990 was a Friday).
  sys_seconds boundary = sys_days{1990y/June/10};

  auto before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "A" );
  auto at = tz->get_info(boundary);
  VERIFY( at.abbrev == "B" );

  // Check that Sun>=8 is parsed correctly, and not defaulted to the 1st.
  auto early = tz->get_info(sys_days{1990y/June/1});
  VERIFY( early.abbrev == "A" );
}

void
test_sun_le_n()
{
  using namespace std::chrono;

  std::ofstream("tzdata.zi") << R"(# version test_sun_le_n
Z Test/SunLE 0 - A 1990 Jun Sun<=15 0u
             0 - B
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_sun_le_n" );

  auto* tz = locate_zone("Test/SunLE");

  // Last Sunday <= June 15 1990 = June 10.
  sys_seconds boundary = sys_days{1990y/June/10};

  auto before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "A" );
  auto at = tz->get_info(boundary);
  VERIFY( at.abbrev == "B" );
}

void
test_year_only()
{
  using namespace std::chrono;

  // MONTH, DAY and TIME default to January 1st 00:00 if not specified.
  std::ofstream("tzdata.zi") << R"(# version test_year_only
Z Test/YearOnly 0 - A 1990
                0 - B
Z Test/YearOnlyC 4 - C 1995 # comment
                 4 - D
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_year_only" );

  auto* tz = locate_zone("Test/YearOnly");
  sys_seconds boundary = sys_days{1990y/January/1};
  auto before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "A" );
  auto at = tz->get_info(boundary);
  VERIFY( at.abbrev == "B" );

  tz = locate_zone("Test/YearOnlyC");
  boundary = sys_days{1995y/January/1} - 4h;
  before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "C" );
  at = tz->get_info(boundary);
  VERIFY( at.abbrev == "D" );
}

void
test_year_month_only()
{
  using namespace std::chrono;

  // DAY and TIME default to the 1st 00:00 if not specified.
  std::ofstream("tzdata.zi") << R"(# version test_year_month_only
Z Test/YearMonth 0 - A 1990 Jul
                 0 - B
Z Test/YearMonthC 3 - C 1995 Apr # comment
                  3 - D
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_year_month_only" );

  auto* tz = locate_zone("Test/YearMonth");
  sys_seconds boundary = sys_days{1990y/July/1};
  auto before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "A" );
  auto at = tz->get_info(boundary);
  VERIFY( at.abbrev == "B" );

  tz = locate_zone("Test/YearMonthC");
  boundary = sys_days{1995y/April/1} - 3h;
  before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "C" );
  at = tz->get_info(boundary);
  VERIFY( at.abbrev == "D" );
}

void
test_year_month_day_only()
{
  using namespace std::chrono;

  std::ofstream("tzdata.zi") << R"(# version test_day_only
Z Test/DayOnly 0 - A 1997 Mar 12
               0 - B
Z Test/DayOnlyC 5 - C 1998 Jun 14 # comment
                5 - D
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_day_only" );

  auto* tz = locate_zone("Test/DayOnly");
  sys_seconds boundary = sys_days{1997y/March/12};
  auto before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "A" );
  auto at = tz->get_info(boundary);
  VERIFY( at.abbrev == "B" );

  tz = locate_zone("Test/DayOnlyC");
  boundary = sys_days{1998y/June/14} - 5h;
  before = tz->get_info(boundary - 1s);
  VERIFY( before.abbrev == "C" );
  at = tz->get_info(boundary);
  VERIFY( at.abbrev == "D" );
}

int
main()
{
  test_lastsu();
  test_sun_ge_n();
  test_sun_le_n();
  test_year_only();
  test_year_month_only();
  test_year_month_day_only();
}
