// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

#include <chrono>
#include <fstream>
#include <format>
#include <testsuite_hooks.h>
#include <iostream>

static bool override_used = false;

namespace __gnu_cxx
{
  const char* zoneinfo_dir_override() {
    override_used = true;
    return "./";
  }
}

using namespace std::chrono;

template<typename Functor, typename... Args>
  void
  test_combinations(Functor&& func, std::string_view txt, size_t min, Args&&... args)
  {
    std::string lower(txt);
    std::string upper(txt);
    for (char& c : upper)
      c = std::toupper(c);

    func(lower.substr(0, min), args...);
    func(upper.substr(0, min), args...);
    func(lower, args...);
    func(upper, args...);
  }

void
test_day(std::string_view name, weekday wd)
{
  constexpr const char* templ = R"(# version test_day_{0}
 # Zone  NAME  STDOFF  RULES  FORMAT  [UNTIL]
 Zone    Test  0       -      Z1      2000 May last{0}
               0       -      Z2      2010 May {0}>=10
               0       -      Z3
  )";

  std::string input = std::format(templ, name);
  std::ofstream("tzdata.zi") << input;

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.

  std::string_view ver = db.version;
  VERIFY( ver.starts_with("test_day_") );
  ver.remove_prefix(9);
  VERIFY( ver == name );

  sys_info info = locate_zone("Test")->get_info(sys_days(2005y/January/1));
  VERIFY( info.begin == sys_days(2000y/May/wd[last]) );
  year_month_day ymd(ceil<days>(info.end));
  VERIFY( ymd.year() == 2010y );
  VERIFY( ymd.month() == May );
  VERIFY( ymd.day() >= day(10) );
  VERIFY( weekday(ymd) == wd );
}

void
test_last(std::string_view name)
{
  constexpr const char* templ = R"(# version test_{0}
 # Zone  NAME  STDOFF  RULES  FORMAT  [UNTIL]
 Zone    Test  0       -      Z1      2000 July {0}Mon
               0       -      Z3
  )";

  std::string input = std::format(templ, name);
  std::ofstream("tzdata.zi") << input;

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.

  std::string_view ver = db.version;
  VERIFY( ver.starts_with("test_") );
  ver.remove_prefix(5);
  VERIFY( ver == name );

  sys_info info = locate_zone("Test")->get_info(sys_days(2005y/January/1));
  VERIFY( info.begin == sys_days(2000y/July/Monday[last]) );
}

void
test_days()
{
  test_combinations(test_day, "monday", 1, Monday);
  test_combinations(test_day, "tuesday", 2, Tuesday);
  test_combinations(test_day, "wednesday", 1, Wednesday);
  test_combinations(test_day, "thursday", 2, Thursday);
  test_combinations(test_day, "friday", 1, Friday);
  test_combinations(test_day, "saturday", 2, Saturday);
  test_combinations(test_day, "sunday", 2, Sunday);

  test_last("last");
  test_last("LAST");
}

void
test_month(std::string_view name, month m)
{
  constexpr const char* templ = R"(# version test_month_{0}
 # Zone  NAME  STDOFF  RULES  FORMAT  [UNTIL]
 Zone    Test  0       -      Z1      2000 {0} 13
               0       -      Z2      2010 {0}
               0       -      Z3
  )";

  std::string input = std::format(templ, name);
  std::ofstream("tzdata.zi") << input;

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.

  std::string_view ver = db.version;
  VERIFY( ver.starts_with("test_month_") );
  ver.remove_prefix(11);
  VERIFY( ver == name );

  sys_info info = locate_zone("Test")->get_info(sys_days(2005y/January/1));
  VERIFY( info.begin == sys_days(2000y/m/13) );
  VERIFY( info.end == sys_days(2010y/m/1) );
}

void
test_months()
{
  test_combinations(test_month, "january", 2, January);
  test_combinations(test_month, "february", 1, February);
  test_combinations(test_month, "march", 3, March);
  test_combinations(test_month, "april", 2, April);
  test_combinations(test_month, "may", 3, May);
  test_combinations(test_month, "june", 3, June);
  test_combinations(test_month, "july", 3, July);
  test_combinations(test_month, "august", 3, August);
  test_combinations(test_month, "september", 1, September);
  test_combinations(test_month, "october", 1, October);
  test_combinations(test_month, "november", 1, November);
  test_combinations(test_month, "december", 1, December);
}

void
test_year(std::string_view min, std::string_view max, std::string_view only)
{
  constexpr const char* templ = R"(# version {0}
 # Rule  NAME  FROM  TO    -  IN   ON       AT    SAVE  LETTER/S
 Rule    Rule  {1}   2020  -  Jan  10       0u    0     S
 Rule    Rule  2000  {2}   -  Nov  12       0u    1     D
 Rule    Rule  2010  {3}   -  Aug  11       0u    2     O
 Rule    Rule  2020  {2}  -   Jan  13       0u    0     S
 # Zone  NAME  STDOFF  RULES  FORMAT  [UNTIL]
 Zone    Test  0       -      Z1      1800 Jan 1 0u
               0       Rule   Z%s     3001 Jan 1 0u
               0       -      Zl
  )";

  const std::string ver = std::format("test_{}_{}_{}", min, max, only);
  std::string input = std::format(templ, ver, min, max, only);
  std::ofstream("tzdata.zi") << input;

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == ver );

  const time_zone* zone  = locate_zone("Test");

  // min applies since 1900, check first transition
  sys_info info = zone->get_info(sys_days(1900y/January/1));
  VERIFY( info.begin == sys_days(1800y/January/1) );
  VERIFY( info.end == sys_days(1900y/January/10) );

  // Check 2010 only rule does not apply in 2009
  info = zone->get_info(sys_days(2009y/February/1));
  VERIFY( info.begin == sys_days(2009y/January/10) );
  VERIFY( info.end == sys_days(2009y/November/12) );

  // Check 2010 only rule in 2010
  info = zone->get_info(sys_days(2010y/February/1));
  VERIFY( info.begin == sys_days(2010y/January/10) );
  VERIFY( info.end == sys_days(2010y/August/11) );

  info = zone->get_info(sys_days(2010y/October/1));
  VERIFY( info.begin == sys_days(2010y/August/11) );
  VERIFY( info.end == sys_days(2010y/November/12) );

  // Check 2010 only rule does not apply in 2011
  info = zone->get_info(sys_days(2011y/February/1));
  VERIFY( info.begin == sys_days(2011y/January/10) );
  VERIFY( info.end == sys_days(2011y/November/12) );

  // max rule applies forever
  info = zone->get_info(sys_days(2500y/February/1));
  VERIFY( info.begin == sys_days(2500y/January/13) );
  VERIFY( info.end == sys_days(2500y/November/12) );

  info = zone->get_info(sys_days(3000y/December/1));
  VERIFY( info.begin == sys_days(3000y/November/12) );
  VERIFY( info.end == sys_days(3001y/January/1) );
}

void
test_years()
{
  test_year("min", "max", "only");
  test_year("m", "m", "o");
  test_year("MIN", "MAX", "ONLY");
  test_year("M", "M", "O");
}

void
test_time(char s, char u, char w, char d)
{
  constexpr const char* templ = R"(# version test_time_{0}{1}{2}{3}
 # Zone  NAME  STDOFF  RULES  FORMAT  [UNTIL]
 Zone    Test  1       0      Zs    2000 Jan 11 10{0}
               3       1      Zu    2005 Feb 12 11{1}
               5       1      Zw    2010 Mar 13 12{2}
               7       1      Zd    2015 Apr 14 13{3}
               9       0      Su    2020 May 15 14{1}
               10      0      Sw    2025 Jun 16 15{2}
               11      -      Zl
  )";

  std::string input = std::format(templ, s, u, w, d);
  std::ofstream("tzdata.zi") << input;

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.

  std::string_view ver = db.version;
  VERIFY( ver.starts_with("test_time_") );
  VERIFY( ver[10] == s );
  VERIFY( ver[11] == u );
  VERIFY( ver[12] == w );
  VERIFY( ver[13] == d );

  const time_zone* zone  = locate_zone("Test");
  sys_info info = zone->get_info(sys_days(2002y/January/1));
  VERIFY( info.begin == sys_days(2000y/January/11) + 10h - 1h );
  VERIFY( info.end == sys_days(2005y/February/12) + 11h );

  info = zone->get_info(sys_days(2012y/January/1));
  VERIFY( info.begin == sys_days(2010y/March/13) + 12h - 6h );
  VERIFY( info.end == sys_days(2015y/April/14) + 13h - 8h );

  info = zone->get_info(sys_days(2022y/January/1));
  VERIFY( info.begin == sys_days(2020y/May/15) + 14h );
  VERIFY( info.end == sys_days(2025y/June/16) + 15h - 10h );
}

void
test_times()
{
  test_time('s', 'u', 'w', 'd');
  test_time('S', 'U', 'W', 'D');
  // Alternate spelling for Universal
  test_time('s', 'g', 'w', 'd');
  test_time('s', 'G', 'w', 'd');
  test_time('s', 'z', 'w', 'd');
  test_time('s', 'Z', 'w', 'd');
}

void
test_line(std::string_view rule, std::string_view zone, std::string_view link)
{
  constexpr const char* templ = R"(# version {0}
 # Rule  NAME  FROM  TO   -  IN   ON       AT    SAVE  LETTER/S
 {1}     Rule  min   max  -  Jan  13       0u    0     D
 {1}     Rule  min   max  -  Oct  12       0u    1     S
 # Zone  NAME  STDOFF  RULES  FORMAT  [UNTIL]
 {2}     Test  0       -      Z1      2000 Jan 13
               0       Rule   Z%s
 # Link  TARGET  LINK-NAME
 {3}     Test    Link
  )";

  const std::string ver = std::format("test_{}_{}_{}", rule, zone, link);
  std::string input = std::format(templ, ver, rule, zone, link);
  std::ofstream("tzdata.zi") << input;

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == ver );

  sys_info info = locate_zone("Test")->get_info(sys_days(2005y/February/1));
  VERIFY( info.begin == sys_days(2005y/January/13) );
  VERIFY( info.end == sys_days(2005y/October/12) );

  const time_zone* target  = locate_zone("Link");
  VERIFY( target->name() == "Test" );
}

void
test_lines()
{
  test_line("rule", "zone", "link");
  test_line("r", "z", "l");
  test_line("RULE", "ZONE", "LINK");
  test_line("R", "Z", "L");
}

int main()
{
  test_days();
  test_months();
  test_years();
  test_times();
  test_lines();
}
