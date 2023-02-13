// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target cxx11_abi }
// { dg-additional-options "-DHAVE_TZDB" { target tzdb } }

#include <chrono>
#include <testsuite_hooks.h>

using namespace std::chrono;

void
test_version()
{
  const tzdb& db = get_tzdb();
  VERIFY( &db == &get_tzdb_list().front() );

#ifdef HAVE_TZDB
  VERIFY( db.version == remote_version() );
  const tzdb& reloaded = reload_tzdb();
  if (reloaded.version == db.version)
    VERIFY( &reloaded == &db );
#endif
}

void
test_current()
{
#ifdef HAVE_TZDB
  const tzdb& db = get_tzdb();
  const time_zone* tz = db.current_zone();
  VERIFY( tz == std::chrono::current_zone() );
#endif
}

void
test_locate()
{
  const tzdb& db = get_tzdb();
  const time_zone* tz = db.locate_zone("GMT");
  VERIFY( tz != nullptr );
  VERIFY( tz->name() == "Etc/GMT" );
  VERIFY( tz == std::chrono::locate_zone("GMT") );
  VERIFY( tz == db.locate_zone("Etc/GMT") );
  VERIFY( tz == db.locate_zone("Etc/GMT+0") );

#ifdef HAVE_TZDB
  VERIFY( db.locate_zone(db.current_zone()->name()) == db.current_zone() );
#endif
}

int main()
{
  test_version();
  test_current();
  test_locate();
}
