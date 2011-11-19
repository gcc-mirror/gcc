// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011 Free Software Foundation, Inc.
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

#include <sstream>
#include <unordered_set>
#include <testsuite_performance.h>

namespace
{
  // Bench using an unordered_set<int>. Hash functor for int is quite
  // predictable so it helps bench very specific use cases.
  template<bool use_cache>
    void bench()
    {
      using namespace __gnu_test;
      std::ostringstream ostr;
      ostr << "unordered_set<int> " << (use_cache ? "with" : "without")
	   << " cache";
      const std::string desc = ostr.str();

      time_counter time;
      resource_counter resource;

      const int nb = 200000;
      start_counters(time, resource);

      std::__unordered_set<int, std::hash<int>, std::equal_to<int>,
			   std::allocator<int>, use_cache> us;
      for (int i = 0; i != nb; ++i)
	us.insert(i);

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": first insert";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      // Here is the worst erase use case when hashtable implementation was
      // something like vector<forward_list<>>. Erasing from the end was very
      // costly because we need to return the iterator following the erased
      // one, as the hashtable is getting emptier at each step there are
      // more and more empty bucket to loop through to reach the end of the
      // container and find out that it was in fact the last element.
      for (int j = nb - 1; j >= 0; --j)
	{
	  auto it = us.find(j);
	  if (it != us.end())
	    us.erase(it);
	}

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": erase from iterator";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      // This is a worst insertion use case for the current implementation as
      // we insert an element at the begining of the hashtable and then we
      // insert starting at the end so that each time we need to seek up to the
      // first bucket to find the first non-empty one.
      us.insert(0);
      for (int i = nb - 1; i >= 0; --i)
	us.insert(i);

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": second insert";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      for (int j = nb - 1; j >= 0; --j)
	us.erase(j);

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": erase from key";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);
    }

  // Bench using unordered_set<string> that show how important it is to cache
  // hash code as computing string hash code is quite expensive compared to
  // computing it for int.
  template<bool use_cache>
    void bench_str()
    {
      using namespace __gnu_test;
      std::ostringstream ostr;
      ostr << "unordered_set<string> " << (use_cache ? "with" : "without")
	   << " cache";
      const std::string desc = ostr.str();

      time_counter time;
      resource_counter resource;

      const int nb = 200000;
      // First generate once strings that are going to be used throughout the
      // bench:
      std::vector<std::string> strs;
      strs.reserve(nb);
      for (int i = 0; i != nb; ++i)
      {
	ostr.str("");
	ostr << "string #" << i;
	strs.push_back(ostr.str());
      }

      start_counters(time, resource);

      std::__unordered_set<std::string, std::hash<std::string>,
			   std::equal_to<std::string>,
			   std::allocator<std::string>, use_cache> us;
      for (int i = 0; i != nb; ++i)
	us.insert(strs[i]);

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": first insert";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      for (int j = nb - 1; j >= 0; --j)
	{
	  auto it = us.find(strs[j]);
	  if (it != us.end())
	    us.erase(it);
	}

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": erase from iterator";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      us.insert(strs[0]);
      for (int i = nb - 1; i >= 0; --i)
	us.insert(strs[i]);

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": second insert";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);

      start_counters(time, resource);

      for (int j = nb - 1; j >= 0; --j)
	us.erase(strs[j]);

      stop_counters(time, resource);
      ostr.str("");
      ostr << desc << ": erase from key";
      report_performance(__FILE__, ostr.str().c_str(), time, resource);
    }
}

int main()
{
  bench<false>();
  bench<true>();
  bench_str<false>();
  bench_str<true>();
  return 0;
}
