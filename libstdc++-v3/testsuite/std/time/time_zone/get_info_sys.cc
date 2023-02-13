// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }

#include <chrono>
#include <testsuite_hooks.h>

void
test_zurich()
{
  using namespace std::chrono;

  const time_zone* const tz = locate_zone("Europe/Zurich");

  {
    sys_days d = 1853y/July/16;

    auto info = tz->get_info(d - 1s);
    VERIFY( info.offset == (34min + 8s) );
    VERIFY( info.abbrev == "LMT" );

    info = tz->get_info(d);
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );

    info = tz->get_info(d + 1s);
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );

    info = tz->get_info(d + 0.001s);
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );
  }

  {
    sys_days d = 1894y/June/1;

    auto info = tz->get_info(d - 1s);
    VERIFY( info.offset == (29min + 46s) );
    VERIFY( info.abbrev == "BMT" );

    info = tz->get_info(d);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1941y/May/Monday[1];

    auto info = tz->get_info(d - 1s);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    // CEST daylight savings time starts at 1am local time (UTC+1).
    info = tz->get_info(d);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );
  }

  {
    sys_days d = 1941y/October/Monday[1];

    auto info = tz->get_info(d - 1s);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CET standard time starts at 2am local time (UTC+2).
    info = tz->get_info(d);
    VERIFY( info.offset == 1h  );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1942y/May/Monday[1];

    auto info = tz->get_info(d - 1s);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    // CEST daylight savings time starts at 1am local time (UTC+1).
    info = tz->get_info(d);
    VERIFY( info.offset == 2h  );
    VERIFY( info.abbrev == "CEST" );
  }

  {
    sys_days d = 1942y/October/Monday[1];

    auto info = tz->get_info(d - 1s);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CET standard time starts at 2am local time (UTC+2).
    info = tz->get_info(d);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1943y/May/Monday[1];

    // No daylight savings from 1943 until 1981.
    auto info = tz->get_info(d);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    info = tz->get_info(d + days(60));
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    info = tz->get_info(d + years(10));
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    info = tz->get_info(sys_days(1979y/June/3));
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    // Switzerland uses EU rules from 1981
    sys_days d = 1981y/March/Sunday[last];

    auto info = tz->get_info(d);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    info = tz->get_info(d + 59min + 59s);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    // CEST begins at 1am UTC
    info = tz->get_info(d + 1h);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );
  }

  {
    sys_days d = 1981y/September/Sunday[last];

    auto info = tz->get_info(d + 59min + 59s);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CEST ends at 1am UTC
    info = tz->get_info(d + 1h);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }

  {
    sys_days d = 1994y/September/Sunday[last];

    auto info = tz->get_info(d + 59min + 59s);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CEST ends at 1am UTC
    info = tz->get_info(d + 1h);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    d = 1995y/September/Sunday[last];
    info = tz->get_info(d + 59min + 59s);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    // CEST ends at 1am UTC
    info = tz->get_info(d + 1h);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );

    d = 1996y/September/Sunday[last];
    // CEST ends in October since 1996
    info = tz->get_info(d + 1h);
    VERIFY( info.offset == 2h );
    VERIFY( info.abbrev == "CEST" );

    d = 1996y/October/Sunday[last];
    // CEST ends at 1am UTC
    info = tz->get_info(d + 1h);
    VERIFY( info.offset == 1h );
    VERIFY( info.abbrev == "CET" );
  }
}

void
test_iterate()
{
  using namespace std::chrono;
  auto tz = locate_zone("Europe/Zurich");
  sys_seconds start(sys_days(1850y/January/1));
  const sys_seconds finish(sys_days(1982y/January/1));
  long count = 0;
  do
  {
    VERIFY(++count < 100); // Fail if we get stuck in a loop.
    auto info = tz->get_info(start);
    start = info.end;
  } while (start < finish);

  VERIFY(count == 10); // Would be 9 if identical adjacent sys_info get merged.
}

void
test_shanghai()
{
  using namespace std::chrono;
  auto tz = locate_zone("Asia/Shanghai");
  sys_info info = tz->get_info(sys_days(1949y/January/1));
  VERIFY( info.abbrev == "CST" );
}

int main()
{
  test_zurich();
  test_iterate();
  test_shanghai();
}
