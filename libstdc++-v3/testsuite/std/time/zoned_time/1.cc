// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }

#include <chrono>
#include <testsuite_hooks.h>

void
test_members()
{
  using namespace std::chrono;

  const time_zone* const zone = locate_zone("Europe/London");

  sys_time<minutes> t = sys_days(2022y/February/1) + 1h + 23min;
  zoned_time<minutes> zt("Europe/London", t);
  VERIFY( zt.get_time_zone() == zone );
  VERIFY( zt.get_sys_time() == t);
  VERIFY( zt.get_local_time().time_since_epoch() == t.time_since_epoch() );
  VERIFY( zt.get_info().offset == 0h );
  VERIFY( zt.get_info().abbrev == "GMT" );
  VERIFY( static_cast<sys_seconds>(zt) == t );
  VERIFY( static_cast<local_seconds>(zt) == zt.get_local_time() );

  t = sys_days(2022y/June/1);
  zt = t;
  VERIFY( zt.get_time_zone() == zone );
  VERIFY( zt.get_sys_time() == t);
  VERIFY( zt.get_local_time().time_since_epoch() == t.time_since_epoch() + 1h );
  VERIFY( zt.get_info().offset == 1h );
  VERIFY( zt.get_info().abbrev == "BST" );
  VERIFY( static_cast<sys_seconds>(zt) == t );
  VERIFY( static_cast<local_seconds>(zt) == zt.get_local_time() );

  zoned_seconds zs(zt);
  VERIFY( zs == zt );

  local_time<seconds> local(zt.get_local_time() + days(1) + hours(2));
  zt = time_point_cast<minutes>(local);
  VERIFY( zt.get_sys_time() == zs.get_sys_time() + days(1) + hours(2) );
}

void
test_zurich()
{
  using namespace std::chrono;

  const time_zone* const zurich = locate_zone("Europe/Zurich");

  {
    sys_days d = 1853y/July/16;

    auto z = zoned_seconds(zurich, sys_seconds(d) - 1s);
    auto info = z.get_info();
    VERIFY( info.offset == (34min + 8s) );
    VERIFY( info.abbrev == "LMT" );

    z = zoned_seconds(zurich, d);
    info = z.get_info();
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );

    z = zoned_seconds(zurich, d + 1s);
    info = z.get_info();
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );

    auto z2 = zoned_time(zurich, d + 0.001s);
    info = z2.get_info();
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );
  }

  {
    sys_days d = 1894y/June/1;

    auto z = zoned_seconds(zurich, sys_seconds(d) - 1s);
    auto info = z.get_info();
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );

    z = zoned_seconds(zurich, d);
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1941y/May/Monday[1];

    auto z = zoned_seconds(zurich, d - 1s);
    auto info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    // CEST daylight savings time starts at 1am local time (UTC+1).
    z = zoned_seconds(zurich, d);
    info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );
  }

  {
    sys_days d = 1941y/October/Monday[1];

    auto z = zoned_seconds(zurich, d - 1s);
    auto info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CET standard time starts at 2am local time (UTC+2).
    z = zoned_seconds(zurich, d);
    info = z.get_info();
    VERIFY( info.offset == 1h  );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1942y/May/Monday[1];

    auto z = zoned_seconds(zurich, d - 1s);
    auto info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    // CEST daylight savings time starts at 1am local time (UTC+1).
    z = zoned_seconds(zurich, d);
    info = z.get_info();
    VERIFY( info.offset == 2h  );
    VERIFY( info.abbrev == "CEST" );
  }

  {
    sys_days d = 1942y/October/Monday[1];

    auto z = zoned_seconds(zurich, d - 1s);
    auto info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CET standard time starts at 2am local time (UTC+2).
    z = zoned_seconds(zurich, d);
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1943y/May/Monday[1];

    // No daylight savings from 1943 until 1981.
    auto z = zoned_seconds(zurich, d);
    auto info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    z = zoned_seconds(zurich, d + days(60));
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    z = zoned_seconds(zurich, d + years(10));
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    z = zoned_seconds(zurich, sys_days(1979y/June/3));
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    // Switzerland uses EU rules from 1981
    sys_days d = 1981y/March/Sunday[last];

    auto z = zoned_seconds(zurich, d);
    auto info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    z = zoned_seconds(zurich, d + 59min + 59s);
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    // CEST begins at 1am UTC
    z = zoned_seconds(zurich, d + 1h);
    info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );
  }

  {
    sys_days d = 1981y/September/Sunday[last];

    auto z = zoned_seconds(zurich, d + 59min + 59s);
    auto info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CEST ends at 1am UTC
    z = zoned_seconds(zurich, d + 1h);
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1994y/September/Sunday[last];

    auto z = zoned_seconds(zurich, d + 59min + 59s);
    auto info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CEST ends at 1am UTC
    z = zoned_seconds(zurich, d + 1h);
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    d = 1995y/September/Sunday[last];
    z = zoned_seconds(zurich, d + 59min + 59s);
    info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );
    // CEST ends at 1am UTC
    z = zoned_seconds(zurich, d + 1h);
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    d = 1996y/September/Sunday[last];
    // CEST ends in October since 1996
    z = zoned_seconds(zurich, d + 1h);
    info = z.get_info();
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    d = 1996y/October/Sunday[last];
    // CEST ends at 1am UTC
    z = zoned_seconds(zurich, d + 1h);
    info = z.get_info();
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }
}

int main()
{
  test_members();
  test_zurich();
}
