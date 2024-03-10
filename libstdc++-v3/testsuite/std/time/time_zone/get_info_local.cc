// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }

#include <chrono>
#include <testsuite_hooks.h>

using namespace std::chrono;

struct empty_tag { } empty;

bool operator==(const sys_info& info, empty_tag)
{
  return info.begin == sys_seconds() && info.end == info.begin
    && info.offset == 0s && info.save == 0min && info.abbrev.empty();
}

void
test_utc()
{
  auto tz = locate_zone("UTC");
  auto now = time_point_cast<seconds>(system_clock::now());
  local_info info = tz->get_info(local_seconds(now.time_since_epoch()));
  VERIFY( info.result == local_info::unique );
  VERIFY( info.first.begin < now );
  VERIFY( info.first.end > now );
  VERIFY( info.first.offset == 0h );
  VERIFY( info.first.save == 0h );
  VERIFY( info.first.abbrev == "UTC" );
  VERIFY( info.second == empty );
}

auto dst_start = March/Sunday[last];
auto dst_end = October/Sunday[last];

void
test_unique()
{
  auto tz = locate_zone("Europe/London");
  local_days feb1(sys_days(2022y/February/1).time_since_epoch());
  local_info info;

  info = tz->get_info(feb1);
  VERIFY( info.result == local_info::unique );
  VERIFY( info.first.begin == sys_days(2021y/dst_end) + 1h );
  VERIFY( info.first.end == sys_days(2022y/dst_start) + 1h );
  VERIFY( info.first.offset == 0h );
  VERIFY( info.first.save == 0h );
  VERIFY( info.first.abbrev == "GMT" );
  VERIFY( info.second == empty );

  info = tz->get_info(feb1 + months(4));
  VERIFY( info.result == local_info::unique );
  VERIFY( info.first.begin == sys_days(2022y/dst_start) + 1h );
  VERIFY( info.first.end == sys_days(2022y/dst_end) + 1h );
  VERIFY( info.first.offset == 1h );
  VERIFY( info.first.save == 1h );
  VERIFY( info.first.abbrev == "BST" );
  VERIFY( info.second == empty );
}

void
test_nonexistent()
{
  auto tz = locate_zone("Europe/Helsinki");
  sys_time<hours> change = sys_days(2022y/dst_start) + 1h;
  local_seconds nonesuch(change.time_since_epoch() + 2h + 30min);
  local_info info;

  info = tz->get_info(nonesuch);
  VERIFY( info.result == local_info::nonexistent );
  VERIFY( info.first.end == change );
  VERIFY( info.first.offset == 2h );
  VERIFY( info.first.save == 0h );
  VERIFY( info.first.abbrev == "EET" );
  VERIFY( info.second.begin == info.first.end );
  VERIFY( info.second.offset == 3h );
  VERIFY( info.second.save == 1h );
  VERIFY( info.second.abbrev == "EEST" );

  tz = locate_zone("America/New_York");
  nonesuch = local_days(Sunday[2]/March/2016) + 2h + 30min;
  info = tz->get_info(nonesuch);
  VERIFY( info.result == local_info::nonexistent );
  VERIFY( info.first.end == sys_days(Sunday[2]/March/2016) + 5h + 2h );
  VERIFY( info.first.offset == -5h );
  VERIFY( info.first.save == 0h );
  VERIFY( info.first.abbrev == "EST" );
  VERIFY( info.second.begin == info.first.end );
  VERIFY( info.second.offset == -4h );
  VERIFY( info.second.save == 1h );
  VERIFY( info.second.abbrev == "EDT" );
}

void
test_ambiguous()
{
  auto tz = locate_zone("Europe/Helsinki");
  sys_time<hours> change = sys_days(2022y/dst_end) + 1h;
  local_seconds twix(change.time_since_epoch() + 2h + 30min);
  local_info info;

  info = tz->get_info(twix);
  VERIFY( info.result == local_info::ambiguous );
  VERIFY( info.first.end == change );
  VERIFY( info.first.offset == 3h );
  VERIFY( info.first.save == 1h );
  VERIFY( info.first.abbrev == "EEST" );
  VERIFY( info.second.begin == info.first.end );
  VERIFY( info.second.offset == 2h );
  VERIFY( info.second.save == 0h );
  VERIFY( info.second.abbrev == "EET" );

  tz = locate_zone("America/New_York");
  twix = local_days(Sunday[2]/March/2016) + 2h + 30min;
  info = tz->get_info(twix);
  VERIFY( info.result == local_info::nonexistent );
  VERIFY( info.first.end == sys_days(Sunday[2]/March/2016) + 5h + 2h );
  VERIFY( info.first.offset == -5h );
  VERIFY( info.first.save == 0h );
  VERIFY( info.first.abbrev == "EST" );
  VERIFY( info.second.begin == info.first.end );
  VERIFY( info.second.offset == -4h );
  VERIFY( info.second.save == 1h );
  VERIFY( info.second.abbrev == "EDT" );
}

void
test_egypt()
{
  local_days d(2010y/May/1);
  auto tz = locate_zone("Egypt");
  local_info info = tz->get_info(d);
  VERIFY( info.result == local_info::unique );
  VERIFY( info.first.begin == sys_days(2010y/April/29) + 22h );
  VERIFY( info.first.offset == 3h );
  VERIFY( info.first.save == 1h );
  VERIFY( info.first.abbrev == "EEST" );

  info = tz->get_info(d - 24h);
  VERIFY( info.result == local_info::nonexistent );
  VERIFY( info.first.begin == sys_days(2009y/August/20) + 21h );
  VERIFY( info.first.offset == 2h );
  VERIFY( info.first.save == 0h );
  VERIFY( info.first.abbrev == "EET" );
  VERIFY( info.second.begin == sys_days(2010y/April/29) + 22h );
  VERIFY( info.second.offset == 3h );
  VERIFY( info.second.save == 1h );
  VERIFY( info.second.abbrev == "EEST" );

  std::ostringstream out;
  local_seconds lt(local_days(2001y/January/1));
  const local_days end(2014y/September/1);

  while (lt < end)
  {
    local_info i = tz->get_info(lt);

    out << '\n' << i;

    auto next = i.first.end;
    if (i.result != local_info::unique)
      next = i.second.begin + 24h;
    lt = zoned_time(tz, next).get_local_time();
  }
  out << '\n';

  std::string expected = R"(
[[2000-09-28 21:00:00,2001-04-26 22:00:00,02:00:00,0min,EET]]
[[2001-04-26 22:00:00,2001-09-27 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2001-04-26 22:00:00,2001-09-27 21:00:00,03:00:00,60min,EEST] and [2001-09-27 21:00:00,2002-04-25 22:00:00,02:00:00,0min,EET]]
[[2001-09-27 21:00:00,2002-04-25 22:00:00,02:00:00,0min,EET]]
[[2002-04-25 22:00:00,2002-09-26 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2002-04-25 22:00:00,2002-09-26 21:00:00,03:00:00,60min,EEST] and [2002-09-26 21:00:00,2003-04-24 22:00:00,02:00:00,0min,EET]]
[[2002-09-26 21:00:00,2003-04-24 22:00:00,02:00:00,0min,EET]]
[[2003-04-24 22:00:00,2003-09-25 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2003-04-24 22:00:00,2003-09-25 21:00:00,03:00:00,60min,EEST] and [2003-09-25 21:00:00,2004-04-29 22:00:00,02:00:00,0min,EET]]
[[2003-09-25 21:00:00,2004-04-29 22:00:00,02:00:00,0min,EET]]
[[2004-04-29 22:00:00,2004-09-30 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2004-04-29 22:00:00,2004-09-30 21:00:00,03:00:00,60min,EEST] and [2004-09-30 21:00:00,2005-04-28 22:00:00,02:00:00,0min,EET]]
[[2004-09-30 21:00:00,2005-04-28 22:00:00,02:00:00,0min,EET]]
[[2005-04-28 22:00:00,2005-09-29 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2005-04-28 22:00:00,2005-09-29 21:00:00,03:00:00,60min,EEST] and [2005-09-29 21:00:00,2006-04-27 22:00:00,02:00:00,0min,EET]]
[[2005-09-29 21:00:00,2006-04-27 22:00:00,02:00:00,0min,EET]]
[[2006-04-27 22:00:00,2006-09-21 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2006-04-27 22:00:00,2006-09-21 21:00:00,03:00:00,60min,EEST] and [2006-09-21 21:00:00,2007-04-26 22:00:00,02:00:00,0min,EET]]
[[2006-09-21 21:00:00,2007-04-26 22:00:00,02:00:00,0min,EET]]
[[2007-04-26 22:00:00,2007-09-06 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2007-04-26 22:00:00,2007-09-06 21:00:00,03:00:00,60min,EEST] and [2007-09-06 21:00:00,2008-04-24 22:00:00,02:00:00,0min,EET]]
[[2007-09-06 21:00:00,2008-04-24 22:00:00,02:00:00,0min,EET]]
[[2008-04-24 22:00:00,2008-08-28 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2008-04-24 22:00:00,2008-08-28 21:00:00,03:00:00,60min,EEST] and [2008-08-28 21:00:00,2009-04-23 22:00:00,02:00:00,0min,EET]]
[[2008-08-28 21:00:00,2009-04-23 22:00:00,02:00:00,0min,EET]]
[[2009-04-23 22:00:00,2009-08-20 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2009-04-23 22:00:00,2009-08-20 21:00:00,03:00:00,60min,EEST] and [2009-08-20 21:00:00,2010-04-29 22:00:00,02:00:00,0min,EET]]
[[2009-08-20 21:00:00,2010-04-29 22:00:00,02:00:00,0min,EET]]
[[2010-04-29 22:00:00,2010-08-10 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2010-04-29 22:00:00,2010-08-10 21:00:00,03:00:00,60min,EEST] and [2010-08-10 21:00:00,2010-09-09 22:00:00,02:00:00,0min,EET]]
[[2010-08-10 21:00:00,2010-09-09 22:00:00,02:00:00,0min,EET]]
[[2010-09-09 22:00:00,2010-09-30 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2010-09-09 22:00:00,2010-09-30 21:00:00,03:00:00,60min,EEST] and [2010-09-30 21:00:00,2014-05-15 22:00:00,02:00:00,0min,EET]]
[[2010-09-30 21:00:00,2014-05-15 22:00:00,02:00:00,0min,EET]]
[[2014-05-15 22:00:00,2014-06-26 21:00:00,03:00:00,60min,EEST]]
[ambiguous local time between [2014-05-15 22:00:00,2014-06-26 21:00:00,03:00:00,60min,EEST] and [2014-06-26 21:00:00,2014-07-31 22:00:00,02:00:00,0min,EET]]
[[2014-06-26 21:00:00,2014-07-31 22:00:00,02:00:00,0min,EET]]
[[2014-07-31 22:00:00,2014-09-25 21:00:00,03:00:00,60min,EEST]]
)";
  VERIFY( out.str() == expected );
}

int main()
{
  test_utc();
  test_unique();
  test_nonexistent();
  test_ambiguous();
  test_egypt();
}
