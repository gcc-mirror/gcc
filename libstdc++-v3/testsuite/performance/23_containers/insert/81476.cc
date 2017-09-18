// Copyright (C) 2012-2017 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <random>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_performance.h>

int main()
{
  using namespace __gnu_test;

  std::default_random_engine eng;
  std::uniform_int_distribution<unsigned> r(0, 127);

  time_counter time;
  resource_counter resource;

  std::vector<std::vector<char>> vecs(10000);
  for (auto& v : vecs)
  {
    v.resize(1000);
    for (auto& c : v)
      c = r(eng);
  }

  start_counters(time, resource);
  std::vector<char> res;
  for (auto& v : vecs)
    res.insert(res.begin(), v.begin(), v.end());
  stop_counters(time, resource);
  report_performance(__FILE__, "insert pointers", time, resource);

  struct input_iterator : std::vector<char>::iterator
  {
    using iterator_category = std::input_iterator_tag;
    using base = std::vector<char>::iterator;

    input_iterator(base it) : base(it) { }
  };

  start_counters(time, resource);
  std::vector<char> res2;
  for (auto& v : vecs)
  {
    auto begin = input_iterator(v.begin());
    auto end = input_iterator(v.end());
    res2.insert(res2.begin(), begin, end);
  }
  stop_counters(time, resource);
  report_performance(__FILE__, "insert input iterators", time, resource);

  start_counters(time, resource);
  std::vector<char> res3;
  for (auto rev = vecs.rbegin(); rev != vecs.rend(); ++rev)
    res3.insert(res3.end(), rev->begin(), rev->end());
  stop_counters(time, resource);
  report_performance(__FILE__, "insert pointers end", time, resource);

  start_counters(time, resource);
  std::vector<char> res4;
  for (auto rev = vecs.rbegin(); rev != vecs.rend(); ++rev)
    res4.insert(res4.end(), rev->begin(), rev->end());
  stop_counters(time, resource);
  report_performance(__FILE__, "insert input iterators end", time, resource);

  VERIFY(res2 == res);
  VERIFY(res3 == res);
  VERIFY(res4 == res);
}
