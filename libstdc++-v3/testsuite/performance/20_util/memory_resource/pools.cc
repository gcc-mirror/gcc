// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// Override the -std flag in the check_performance script: STD=gnu++17

#include <memory_resource>
#include <list>
#include <string>
#include <testsuite_performance.h>

struct size16 { char c[16]; };
struct size32 { char c[32]; };
struct size64 { char c[64]; };
struct size128 { char c[128]; };

// Insert and remove elements of various sizes in std::list containers.
// If report=true the function will measure and report the total performance
// including the time taken to destroy the lists and deallocate everything.
// If dest=false the function will measure and report the performance of
// insert/remove operations only, not the destruction of the lists.
void
populate_lists(std::pmr::memory_resource* r, std::string name, bool dest,
	       int kmax = 100)
{
  name += " std::list push/pop";
  if (dest)
    name += "/destroy";

  std::pmr::list<int> l4(r);
  std::pmr::list<size16> l16(r);
  std::pmr::list<size32> l32(r);
  std::pmr::list<size64> l64(r);
  std::pmr::list<size128> l128(r);

  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;
  start_counters(time, resource);

  const int imax = 1000;
  const int jmax = 100;
  for (int k = 0; k < kmax; ++k)
  {
    for (int i = 0; i < imax; ++i)
    {
      for (int j = 0; j < jmax; ++j)
      {
        l4.emplace_back();
        l16.emplace_back();
        l32.emplace_back();
        l64.emplace_back();
        l128.emplace_back();
      }
      l4.pop_front();
      l16.pop_front();
      l32.pop_front();
      l64.pop_front();
      l128.pop_front();
    }

    if (!dest)
      time.stop();

    // Deallocate everything:
    l4.clear();
    l16.clear();
    l32.clear();
    l64.clear();
    l128.clear();

    if (!dest)
      time.restart();
  }

  stop_counters(time, resource);

  report_performance(__FILE__, name.c_str(), time, resource);
  clear_counters(time, resource);
}

int main()
{
  std::pmr::memory_resource* newdel = std::pmr::new_delete_resource();
  std::pmr::unsynchronized_pool_resource pool;

  for (auto b : { false, true })
  {
    // Start with an empty set of pools:
    pool.release();

    populate_lists(newdel, "new_delete 1", b);
    populate_lists(newdel, "new_delete 2", b);
    populate_lists(newdel, "new_delete 3", b);

    populate_lists(&pool, "unsync pool 1", b);
    // Destroy pools and start fresh:
    pool.release();
    populate_lists(&pool, "unsync pool 2", b);
    // Do not destroy pools, reuse allocated memory:
    populate_lists(&pool, "unsync pool 3", b);
  }
}
