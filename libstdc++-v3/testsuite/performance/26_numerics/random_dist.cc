// Copyright (C) 2020-2025 Free Software Foundation, Inc.
//
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


#include <random>
#include <testsuite_performance.h>

namespace counters
{
  __gnu_test::time_counter time;
  __gnu_test::resource_counter resource;
}


template<typename Dist, typename Engine>
void do_fill_with_uniform_ints(std::string name, Dist& d, Engine& e)
{
  using counters::time;
  using counters::resource;

  std::vector<typename Dist::result_type> r;
  int n = 10000000;
  {
    const auto suffix = "-e" + std::to_string((int)std::log10(n));
    r.resize(n);

    start_counters(time, resource);
    for (auto& x : r)
      x = d(e);
    stop_counters(time, resource);
    report_performance(__FILE__, name+suffix, time, resource);
    clear_counters(time, resource);

    d.reset();

    start_counters(time, resource);
    d.__generate(begin(r), end(r), e);
    stop_counters(time, resource);
    report_performance(__FILE__, name+"-range"+suffix, time, resource);
    clear_counters(time, resource);
  }
}

template<typename Engine>
void fill_with_uniform_ints(std::string name, Engine& e)
{
  using Dist = std::uniform_int_distribution<typename Engine::result_type>;
  using param_type = typename Dist::param_type;

  unsigned maxima[]{6, 10, 32, 100, 1000, 1024, (1<<16)-1, 1<<16, 1<<20, -1u};
  for (auto hi : maxima)
  {
    Dist dist(param_type{0, hi});
    std::ostringstream s;
    s << name << "-uniform_int-" << (dist.max() - dist.min());
    do_fill_with_uniform_ints(s.str(), dist, e);
  }
}

int main()
{
  using namespace std;

  std::mt19937 mt;
  fill_with_uniform_ints("mt19937", mt);
  std::mt19937_64 mt64;
  fill_with_uniform_ints("mt19937_64", mt64);

  // Same as std::mt19937 but using uint32_t not uint_fast32_t for result_type
  using mt19937_32 = std::mersenne_twister_engine<uint32_t, 32, 624, 397, 31,
                                                  0x9908b0df, 11, 0xffffffff,
                                                  7, 0x9d2c5680, 15,
                                                  0xefc60000, 18, 1812433253>;
  mt19937_32 mt32;
  fill_with_uniform_ints("mt19937_32", mt32);

  std::minstd_rand0 lcg;
  fill_with_uniform_ints("minstd_rand0", lcg);

  // Same as std::minstd_rand0 but using uint32_t not uint_fast32_t
  using minstd_rand0_32 = std::linear_congruential_engine<uint32_t, 48271, 0,
                                                          2147483647>;
  minstd_rand0_32 lcg_32;
  fill_with_uniform_ints("minstd_rand0_32", lcg_32);

  return 0;
}

