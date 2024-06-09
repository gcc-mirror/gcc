// { dg-do run { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <chrono>
#include <testsuite_hooks.h>

using namespace std::chrono;

void
test_version()
{
  const tzdb& db = get_tzdb();
  VERIFY( &db == &get_tzdb_list().front() );

  const char* func;
  try {
    func = "remote_version";
    VERIFY( db.version == remote_version() );
    func = "reload_tzdb";
    const tzdb& reloaded = reload_tzdb();
    if (reloaded.version == db.version)
      VERIFY( &reloaded == &db );
  } catch (const std::exception&) {
    std::printf("std::chrono::%s() failed\n", func);
  }
}

void
test_current()
{
  const tzdb& db = get_tzdb();
  const time_zone* tz = db.current_zone();
  VERIFY( tz == std::chrono::current_zone() );
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

  VERIFY( db.locate_zone(db.current_zone()->name()) == db.current_zone() );
}

void
test_all_zones()
{
  const tzdb& db = get_tzdb();

  for (const auto& zone : db.zones)
    VERIFY( locate_zone(zone.name())->name() == zone.name() );

  for (const auto& link : db.links)
    VERIFY( locate_zone(link.name()) == locate_zone(link.target()) );
}

int main()
{
  test_version();
  test_current();
  test_locate();
}
