// { dg-do run { target c++11 } }

// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

#include <string>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <testsuite_performance.h>

namespace
{
  const int nb_elements = 20;
  const int nb_insts = 15000;

  template<typename _ElemType>
    void bench(const char* desc, const std::vector<_ElemType>& elems, bool with_copy)
    {
      using namespace __gnu_test;

      time_counter time;
      resource_counter resource;

      std::vector<std::unordered_set<_ElemType>> insts(nb_insts);
      for (int j = 0; j != nb_insts; ++j)
	insts.emplace_back();

      start_counters(time, resource);

      for (auto& us : insts)
	for (int i = 0; i != nb_elements; ++i)
	  us.insert(elems[i]);

      stop_counters(time, resource);

      std::ostringstream ostr;
      ostr << desc << " 1st insert";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      if (with_copy)
	{
	  start_counters(time, resource);

	  std::vector<std::unordered_set<_ElemType>>(insts).swap(insts);

	  stop_counters(time, resource);

	  ostr.str("");
	  ostr << desc << " copy";
	  report_performance(__FILE__, ostr.str().c_str(), time, resource);
	}

      start_counters(time, resource);

      for (auto& us : insts)
	for (int i = nb_elements - 1; i >= 0; --i)
	  {
	    auto it = us.find(elems[i]);
	    if (it != us.end())
	      us.erase(it);
	  }

      stop_counters(time, resource);

      ostr.str("");
      ostr << desc << " find/erase";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      for (auto& us : insts)
	{
	  us.insert(elems[0]);
	  for (int i = nb_elements - 1; i >= 0; --i)
	    us.insert(elems[i]);
	}

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << " 2nd insert";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      for (auto& us : insts)
	for (int j = nb_elements - 1; j >= 0; --j)
	  us.erase(elems[j]);

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << " erase key ";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);
    }
}

int main()
{
  {
    std::vector<int> elems;
    elems.reserve(nb_elements);
    for (int i = 0; i != nb_elements; ++i)
      elems.push_back(i);

    bench("std::unordered_set<int>:    ", elems, false);
    bench("std::unordered_set<int>:    ", elems, true);
  }

  {
    std::vector<std::string> elems;
    {
      elems.reserve(nb_elements);
      for (int i = 0; i != nb_elements; ++i)
	{
	  std::ostringstream ostr;
	  ostr << "string #" << i << ' ' << std::string(1000, 'a' + i);
	  elems.push_back(ostr.str());
	}
    }

    bench("std::unordered_set<string>: ", elems, false);
    bench("std::unordered_set<string>: ", elems, true);
  }

  return 0;
}
