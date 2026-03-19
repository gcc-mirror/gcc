// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }

#include <chrono>
#include <testsuite_hooks.h>

using namespace std::chrono;

void
test_broken_hill()
{
  /*
  R AN 1996 2005 - Mar lastSun 2s 0 S
  R AN 2000 o    - Aug lastSun 2s 1 D
  Z Australia/Broken_Hill 9:25:48 -  LMT  1895 Feb
                          10      -  AEST 1896 Aug 23
                          9       -  ACST 1899 May
                          9:30    AU AC%sT 1971
                          9:30    AN AC%sT 2000
                          9:30    AS AC%sT
  */
  auto* tz = locate_zone("Australia/Broken_Hill");
  auto info = tz->get_info(sys_days(2000y/February/29d) + 23h + 23min + 23s);
  VERIFY( info.offset == 630min );
  VERIFY( info.save == 60min );
  VERIFY( info.abbrev == "ACDT" );
}

void
test_kiritimati()
{
  /* No named rules involved here, just change of fixed STDOFF at 1994-12-31:
  Z Pacific/Kiritimati -10:29:20 - LMT 1901
                       -10:40    - %z  1979 Oct
                       -10       - %z  1994 Dec 31
                        14       - %z
  */
  auto* tz = locate_zone("Pacific/Kiritimati");
  local_seconds t = local_days(1994y/December/31);

  sys_seconds ut(t.time_since_epoch() + 10h);
  sys_info info;
  info = tz->get_info(ut - 1s);
  VERIFY( info.offset == -10h );
  VERIFY( info.save == 0h );
  VERIFY( info.abbrev == "-10" );
  info = tz->get_info(ut);
  VERIFY( info.offset == 14h );
  VERIFY( info.save == 0h );
  VERIFY( info.abbrev == "+14" );
}

void
test_apia()
{
  /* Refers to same named rule before/after change at 2011-12-29 24:00
     but the offset changed from -11h+1h to +13h+1h
  Z Pacific/Apia 12:33:4  -  LMT 1892 Jul 5
                -11:26:56 -  LMT 1911
                -11:30    -  %z  1950
                -11       WS %z  2011 Dec 29 24
                 13       WS %z
  */
  auto* tz = locate_zone("Pacific/Apia");
  local_seconds t = local_days(2011y/December/29) + 24h;

  // FIXME: this should be + 10h but we do not account for DST yet, so + 11h.
  sys_seconds ut(t.time_since_epoch() + 11h );
  sys_info info;
  info = tz->get_info(ut - 1s);
  VERIFY( info.offset == (-11h + info.save) );
  VERIFY( info.save == 1h );
  VERIFY( info.abbrev == "-10" );
  info = tz->get_info(ut);
  VERIFY( info.offset == (13h + info.save) );
  VERIFY( info.save == 1h );
  VERIFY( info.abbrev == "+14" );
}

int main()
{
  test_broken_hill();
  test_kiritimati();
  test_apia();
}
