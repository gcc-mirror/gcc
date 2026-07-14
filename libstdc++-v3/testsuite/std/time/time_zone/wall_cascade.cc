// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

// Wall-time rules in the same rule set whose effective firing time
// depends on a prior rule's save (Europe/Paris 1945):
//   1945 Apr 2  02:00 wall  save=2  M
//   1945 Sep 16 03:00 wall  save=0  -
// In the (stdoff=1, save=2) frame the September rule fires at
// Sep 16 00:00 UT, not Sep 16 02:00 UT.

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
test_positive()
{
  // Line 1 ends at "1945 Sep 16 1u" (Universal time, no save shenanigans),
  // so info.begin for line 2 is exactly 1945-09-16 01:00 UT.
  //
  // Two-line zone whose second line begins at 1945 Sep 16 01:00 UT,
  // between the cascaded firing time (Sep 16 00:00 UT) and the
  // non-cascaded firing time (Sep 16 02:00 UT) of the September rule.
  // The seeding must pick the September rule (save=0, CET) at info.begin.
  std::ofstream("tzdata.zi") << R"(# version test_wall_cascade
R Fr 1945 o - Apr 2  2 2 M
R Fr 1945 o - Sep 16 3 0 -
Z Test/Paris 0  -  X     1945 Sep 16 1u
             1  Fr CE%sT
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_wall_cascade" );

  auto* tz = locate_zone("Test/Paris");

  // Line 2 begins at exactly 1945-09-16 01:00 UT.  Sample one second
  // after the boundary, well inside line 2's first sys_info.
  auto info = tz->get_info(sys_seconds{
      sys_days(1945y/September/16) + 1h + 1s});
  VERIFY( info.offset == 1h );
  VERIFY( info.save == 0min );
  VERIFY( info.abbrev == "CET" );

  // The boundary instant itself is in the new line.
  auto at_boundary
    = tz->get_info(sys_seconds{sys_days(1945y/September/16) + 1h});
  VERIFY( at_boundary.offset == 1h );
  VERIFY( at_boundary.save == 0min );

  // Sample later still in line 2 (winter): unchanged.
  auto winter = tz->get_info(sys_days(1945y/December/1));
  VERIFY( winter.offset == 1h );
  VERIFY( winter.save == 0min );
}

void
test_negative()
{
  // This is synthetic version of above example, with negative
  // running save.
  //
  // Two-line zone whose second line begins at 1945 Sep 16 01:00 UT,
  // before the cascaded firing time (Sep 16 02:00 UT), but after
  // non-cascaded firing time (Sep 16 00:00 UT) of the September rule.
  // The seeding must pick the April rule (save=-2, CEST) at info.begin.
  std::ofstream("tzdata.zi") << R"(# version test_negative_cascade
R Fr 1945 o - Apr 2  2 -3 M
R Fr 1945 o - Sep 16 0 0 -
Z Test/Negative 0  -  X     1945 Sep 16 1u
             1  Fr CE%sT
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_negative_cascade" );

  auto* tz = locate_zone("Test/Negative");

  // Line 2 begins at exactly 1945-09-16 01:00 UT, sample
  // one second after.
  auto info = tz->get_info(sys_seconds{
      sys_days(1945y/September/16) + 1h + 1s});
  VERIFY( info.offset == -2h );
  VERIFY( info.save == -3h );
  VERIFY( info.abbrev == "CEMT" );

  // The boundary instant.
  auto at_boundary
    = tz->get_info(sys_seconds{sys_days(1945y/September/16) + 1h});
  VERIFY( at_boundary.offset == -2h );
  VERIFY( at_boundary.save == -3h );

  // Test the firing of Sep 16 rule
  auto at_sep_rule = tz->get_info(sys_seconds{
    sys_days(1945y/September/16) + 2h});
  // The transition_window < 1d condition triggers, and
  // transition is ignored.
  // VERIFY( at_sep_rule.offset == 1h );
  // VERIFY( at_sep_rule.save == 0h );
  // VERIFY( at_sep_rule.abbrev == "CET" );
}

void
test_next_year()
{
  // The NZ 1946 rule triggers at 1946 Jan 1 00:00:00
  // local time, which correspond to 1946 Dec 13 12:00:00 UT.
  std::ofstream("tzdata.zi") << R"(# version test_next_year
R NZ 1934 1940 - Ap lastSu 2 0 M
R NZ 1934 1940 - S lastSu 2 0:30 S
R NZ 1946 o - Ja 1 0 0 S
Z Pacific/Auckland 11:39:4 - LMT 1868 N 2
11:30 NZ NZ%sT 1946
12 NZ NZ%sT
Z Pacific/AucklandUT 11:39:4 - LMT 1868 N 2
11:30 NZ NZ%sT 1945 Dec 31 13u
12 NZ NZ%sT
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_next_year" );

  // Pacific/Auckland requires both PR124854 and PR116110 to work
  // correctly. TODO test it once implemented.
  // The UT version uses 1945-12-31 13:00:00 UT after
  // the rule application change.
  auto* utz = locate_zone("Pacific/AucklandUT");

  // Before the change
  auto before_utboundary
   = utz->get_info(sys_seconds{sys_days(1945y/December/31) + 11h});
  VERIFY( before_utboundary.offset == 12h );
  VERIFY( before_utboundary.save == 30min );
  VERIFY( before_utboundary.abbrev == "NZST" );

  auto at_utboundary
    = utz->get_info(sys_seconds{sys_days(1945y/December/31) + 13h});
  VERIFY( at_utboundary.offset == 12h );
  VERIFY( at_utboundary.save == 0h );
  VERIFY( at_utboundary.abbrev == "NZST" );

  auto after_utboundary
    = utz->get_info(sys_seconds{sys_days(1945y/December/31) + 14h});
  VERIFY( after_utboundary.offset == 12h );
  VERIFY( after_utboundary.save == 0h );
  VERIFY( after_utboundary.abbrev == "NZST" );
}

void
test_prev_year()
{
  // The synthetic version of above, where the local
  // time for rule is moved to previous year.
  // The NZ 1946 rule triggers at 1946 Dec 31 22:00:00
  // local time, which correspond to 1947 Jan 1 10:00:00 UT.
  std::ofstream("tzdata.zi") << R"(# version test_prev_year
R PY 1934 1940 - Ap lastSu 2 0 M
R PY 1934 1940 - S lastSu 2 0:30 S
R PY 1946 o - D 31 22 0 S
Z Test/PrevYear -11:39:4 - LMT 1868 N 2
-12:30 PY PY%sT 1947 Jan 1 11u
-12 PY PY%sT
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_prev_year" );

  // The UT version uses 1945-12-31 13:00:00 UT after
  // the rule application change.
  auto* tz = locate_zone("Test/PrevYear");

  // Before the change
  auto before_boundary
   = tz->get_info(sys_seconds{sys_days(1947y/January/1) + 9h});
  VERIFY( before_boundary.offset == -12h );
  VERIFY( before_boundary.save == 30min );
  VERIFY( before_boundary.abbrev == "PYST" );

  auto at_boundary
    = tz->get_info(sys_seconds{sys_days(1947y/January/1) + 11h});
  VERIFY( at_boundary.offset == -12h );
  VERIFY( at_boundary.save == 0h );
  VERIFY( at_boundary.abbrev == "PYST" );

  auto after_boundary
    = tz->get_info(sys_seconds{sys_days(1947y/January/1) + 12h});
  VERIFY( after_boundary.offset == -12h );
  VERIFY( after_boundary.save == 0h );
  VERIFY( after_boundary.abbrev == "PYST" );
}

void
test_earlier_year()
{
  // Synthetic example where PY 1941 rule is still running.
  std::ofstream("tzdata.zi") << R"(# version test_earlier_year
R EY 1934 1941 - Ap lastSu 2 0 M
R EY 1934 1940 - S lastSu 2 0:30 S
Z Test/EarielYear 11:39:4 - LMT 1868 N 2
11:30 EY EY%sT 1943 Jan 1 12u
12 EY EY%sT
   )";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_earlier_year" );

  auto* utz = locate_zone("Test/EarielYear");
  auto at_boundary
    = utz->get_info(sys_seconds{sys_days(1943y/January/1) + 12h});
  VERIFY( at_boundary.offset == 12h );
  VERIFY( at_boundary.save == 0min );
  VERIFY( at_boundary.abbrev == "EYMT" );
}

void
test_at_boundary()
{
  std::ofstream("tzdata.zi") << R"(# version test_at_boundary
R p 1947 1966 - Ap Su>=1 2s 1 S
R p 1947 1965 - O Su>=1 2s 0 -
R p 1976 o - S lastSu 1 0 -
R p 1977 o - Mar lastSu 0s 1 S
R p 1977 o - S lastSu 0s 0 -
Z Europe/Lisbon -0:36:45 - LMT 1884
1 - CET 1976 S 26 1
0 p WE%sT 1986
   )";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_at_boundary" );

  // The change from CET to WE%sT line happens 1976 Sep 26 00:00:00 UT,
  // should take into consideration 1976 lastSu rule that fires at the
  // same time (running save is 1h from 1966 Ap rule application),
  // and start in standard time (WET period).
  auto* utz = locate_zone("Europe/Lisbon");
  auto at_boundary
    = utz->get_info(sys_seconds{sys_days(1976y/September/26) + 0h});
  VERIFY( at_boundary.offset == 0h );
  VERIFY( at_boundary.save == 0min );
  VERIFY( at_boundary.abbrev == "WET" );
}

void
test_last_transition()
{
   std::ofstream("tzdata.zi") << R"(# version test_bishkek
R R 1984 1995 - S lastSu 2s 0 -
R R 1985 2010 - Mar lastSu 2s 1 S
R R 1996 2010 - O lastSu 2s 0 -
R KG 1992 1996 - Ap Su>=7 0s 1 -
R KG 1992 1996 - S lastSu 0 0 -
R KG 1997 2005 - Mar lastSu 2:30 1 -
R KG 1997 2004 - O lastSu 2:30 0 -
Z Test/Bishkek 4:58:24 - LMT 1924 May 2
5 R %z 1991 Au 31 2
5 KG %z 2005 Au 12
   )";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_bishkek" );

  auto* tz = locate_zone("Test/Bishkek");

  sys_seconds transitions[]{
    sys_seconds{sys_days{1991y/August/30}} + 20h, // from 5 R %z 1991 Au 31 2
    sys_seconds{sys_days{1992y/April/11}} + 19h,
    sys_seconds{sys_days{1992y/September/26}} + 18h,
    sys_seconds{sys_days{1993y/April/10}} + 19h,
    sys_seconds{sys_days{1993y/September/25}} + 18h,
    sys_seconds{sys_days{1994y/April/9}} + 19h,
    sys_seconds{sys_days{1994y/September/24}} + 18h,
    sys_seconds{sys_days{1995y/April/8}} + 19h,
    sys_seconds{sys_days{1995y/September/23}} + 18h,
    sys_seconds{sys_days{1996y/April/6}} + 19h,
    sys_seconds{sys_days{1996y/September/28}} + 18h,
    sys_seconds{sys_days{1997y/March/29}} + 21h + 30min,
    sys_seconds{sys_days{1997y/October/25}} + 20h + 30min,
    sys_seconds{sys_days{1998y/March/28}} + 21h + 30min,
    sys_seconds{sys_days{1998y/October/24}} + 20h + 30min,
    sys_seconds{sys_days{1999y/March/27}} + 21h + 30min,
    sys_seconds{sys_days{1999y/October/30}} + 20h + 30min,
    sys_seconds{sys_days{2000y/March/25}} + 21h + 30min,
    sys_seconds{sys_days{2000y/October/28}} + 20h + 30min,
    sys_seconds{sys_days{2001y/March/24}} + 21h + 30min,
    sys_seconds{sys_days{2001y/October/27}} + 20h + 30min,
    sys_seconds{sys_days{2002y/March/30}} + 21h + 30min,
    sys_seconds{sys_days{2002y/October/26}} + 20h + 30min,
    sys_seconds{sys_days{2003y/March/29}} + 21h + 30min,
    sys_seconds{sys_days{2003y/October/25}} + 20h + 30min,
    sys_seconds{sys_days{2004y/March/27}} + 21h + 30min,
    sys_seconds{sys_days{2004y/October/30}} + 20h + 30min,
    sys_seconds{sys_days{2005y/March/26}} + 21h + 30min,
    sys_seconds{sys_days{2005y/August/11}} + 18h, // 5 KG %z 2005 Au 12 
  };

  for (size_t i = 1; i < std::size(transitions); ++i)
    {
      const minutes save = (i % 2) ? 0h : 1h;
      const sys_seconds t = transitions[i-1];
      const sys_info info = tz->get_info(t);
      VERIFY( info.begin == t );
      VERIFY( info.offset == 5h + save );
      VERIFY( info.save == save );
      VERIFY( info.end == transitions[i] );
    }

  // The transition 2005 Au 12 has total offset 6 (5h + 1h).
  sys_seconds t = transitions[std::size(transitions)-1];
  const sys_info info = tz->get_info(t);
  VERIFY( info.offset == 6h );
  VERIFY( info.save == 1h );
}

int
main()
{
  test_positive();
  test_negative();
  test_next_year();
  test_prev_year();
  test_earlier_year();
  test_at_boundary();
  test_last_transition();
}
