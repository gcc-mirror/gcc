// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }

#include <chrono>
#include <fstream>
#include <cstdio>
#include <testsuite_hooks.h>

static bool override_used = false;

namespace __gnu_cxx
{
  const char* zoneinfo_dir_override() {
    override_used = true;
    return "./";
  }
}

std::string tzdata_zi = R"(
 # version test1
 # Rule  NAME  FROM  TO    TYPE  IN   ON       AT    SAVE  LETTER/S
 Rule    Swiss 1941  1942  -     May  Mon>=1   1:00  1:00  S
 Rule    Swiss 1941  1942  -     Oct  Mon>=1   2:00  0     -
 Rule    EU    1977  1980  -     Apr  Sun>=1   1:00u 1:00  S
 Rule    EU    1977  only  -     Sep  lastSun  1:00u 0     -
 Rule    EU    1978  only  -     Oct   1       1:00u 0     -
 Rule    EU    1979  1995  -     Sep  lastSun  1:00u 0     -
 Rule    EU    1981  max   -     Mar  lastSun  1:00u 1:00  S
 Rule    EU    1996  max   -     Oct  lastSun  1:00u 0     -

 # Zone  NAME           STDOFF      RULES  FORMAT  [UNTIL]
 Zone    Europe/Zurich  0:34:08     -      LMT     1853 Jul 16
                        0:29:45.50  -      BMT     1894 Jun
                        1:00        Swiss  CE%sT   1981
                        1:00        EU     CE%sT

 Link    Europe/Zurich  Europe/Vaduz

)";

using namespace std::chrono;

void
test_access()
{
  tzdb_list& list = get_tzdb_list();
  tzdb_list::const_iterator first = list.begin();
  tzdb_list::const_iterator last = list.end();
  VERIFY( list.cbegin() == first );
  VERIFY( list.cend() == last );
  VERIFY( first != last );
  VERIFY( &*first == &get_tzdb() );
  VERIFY( &*first == &list.front() );
  VERIFY( std::next(first) == last );
  first++;
  VERIFY( first == last );
}

void
test_reload()
{
  tzdb_list& list = get_tzdb_list();
  tzdb_list::const_iterator test1 = list.begin();
  reload_tzdb();
  VERIFY( list.begin() == test1 );
  VERIFY( std::distance(list.begin(), list.end()) == 1 );

  std::string new_tzdata_zi = tzdata_zi;
  auto pos = new_tzdata_zi.find("test");
  new_tzdata_zi[pos + 4] = '2';
  std::ofstream("tzdata.zi") << new_tzdata_zi;
  VERIFY( remote_version() == "test2" );

  // List doesn't reload until requested to.
  VERIFY( get_tzdb_list().begin() == test1 );
  VERIFY( &get_tzdb() == &*test1 );
  reload_tzdb();
  VERIFY( list.begin() != test1 );
  VERIFY( std::distance(list.begin(), list.end()) == 2 );
  VERIFY( test1 == std::next(list.begin()) );
  VERIFY( &get_tzdb() == &*list.begin() );
  VERIFY( list.begin()->version == "test2" );
  VERIFY( test1->version == "test1" );
}

void
test_erase()
{
  tzdb_list& list = get_tzdb_list();
  const int count = std::distance(list.begin(), list.end());
  tzdb_list::const_iterator test2 = list.begin();

  std::string new_tzdata_zi = tzdata_zi;
  auto pos = new_tzdata_zi.find("test");
  new_tzdata_zi[pos + 4] = '3';
  std::ofstream("tzdata.zi") << new_tzdata_zi;

  reload_tzdb();
  VERIFY( std::distance(list.begin(), list.end()) == count + 1 );
  VERIFY( list.begin()->version == "test3" );
  list.erase_after(list.begin());
  VERIFY( std::distance(list.begin(), list.end()) == count );
  VERIFY( list.begin()->version == "test3" );
  VERIFY( std::next(list.begin())->version == "test1" );

  // As a GCC extension, the erased node is not destroyed
  // while there are iterators referring to it.
  VERIFY( test2->version == "test2" );
  VERIFY( test2->leap_seconds == list.begin()->leap_seconds );
  // But the iterator points to an unlinked list node now:
  VERIFY( std::next(test2) == tzdb_list::const_iterator() );
}

int main()
{
  std::ofstream("leapseconds") << '\n';
  std::ofstream("tzdata.zi") << tzdata_zi;

  test_access();

  if (override_used)
  {
    test_reload();
    test_erase();
  }
  else
    std::puts("__gnu_cxx::zoneinfo_dir_override() doesn't work on this target");
}
