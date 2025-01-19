// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

#include <testsuite_performance.h>

#include <sstream>
#include <string>
#include <vector>
#include <unordered_set>

namespace
{
  const int sz = 2000000;
  const std::string pattern = "long enough test string #";

  // Perfect std::string hasher knowing how string instances have been
  // generated. It is not tag as slow so that hash code is not cached.
  // It is easier to show impact of hint in this context.
  struct hasher
  {
    std::size_t
    operator()(const std::string& str) const noexcept
    {
      std::hash<std::string> std_hasher;
      auto hash = std_hasher(pattern);
      std::istringstream isstr(str.substr(pattern.length()));
      int idx;
      isstr >> idx;
      return (std::size_t)(hash / sz) * sz + idx;
    }
  };

  // Like previous hasher but tagged as slow.
  struct slow_hasher
  {
    std::size_t
    operator()(const std::string& str) const noexcept
    { return hasher{}(str); }
  };

  template<typename _Hash>
    using ums_t = std::unordered_multiset<std::string, _Hash>;

  template<typename _Hash>
    void
    insert_with_perfect_hint(const std::vector<std::string>& strs,
			     ums_t<_Hash>& ms)
    {
      auto hint = ms.end();
      for (auto str : strs)
	{
	  auto insert_pos = ms.insert(hint, str);
	  if (std::next(insert_pos) == ms.end())
	    hint = insert_pos;
	}
    }

  template<typename _Hash>
    void
    insert_with_bad_hint(const std::vector<std::string>& strs,
			 ums_t<_Hash>& ms)
    {
      auto hint = ms.begin();
      for (auto str : strs)
	{
	  auto insert_pos = ms.insert(hint, str);
	  if (std::next(insert_pos) == hint)
	    hint = ms.begin();
	}
    }

  template<typename _Hash>
    void
    insert_without_hint(const std::vector<std::string>& strs,
			ums_t<_Hash>& ms)
    {
      for (auto str : strs)
	ms.insert(str);
    }

  template<typename _Hash>
    void
    insert_range(const std::vector<std::string>& strs,
		 ums_t<_Hash>& ms)
    { ms.insert(strs.begin(), strs.end()); }
}

template<typename _Hash>
  void bench(const char* ctx)
  {
    using namespace __gnu_test;

    const int nb_iter = 10;

    std::vector<std::string> strs;
    strs.reserve(sz);

    for (int i = 0; i != sz; ++i)
      {
	std::ostringstream osstr;
	osstr << pattern << i;
	strs.push_back(osstr.str());
      }

    ums_t<_Hash> ms;
    ms.reserve(sz);

    // Warm up.
    {
      for (auto str : strs)
	ms.insert(str);
    }

    time_counter time_no_hint, time_bad_hint, time_perfect_hint,
      time_range;
    resource_counter resource_no_hint, resource_bad_hint, resource_perfect_hint,
      resource_range;

    for (int i = 0; i != nb_iter; ++i)
      {
	// Bad hint
	{
	  ms.clear();
	  start_counters(time_bad_hint, resource_bad_hint);
	  insert_with_bad_hint(strs, ms);
	  stop_counters(time_bad_hint, resource_bad_hint);
	}

	// No hint
	{
	  ms.clear();
	  start_counters(time_no_hint, resource_no_hint);
	  insert_without_hint(strs, ms);
	  stop_counters(time_no_hint, resource_no_hint);
	}

	// Perfect hint
	{
	  ms.clear();
	  start_counters(time_perfect_hint, resource_perfect_hint);
	  insert_with_perfect_hint(strs, ms);
	  stop_counters(time_perfect_hint, resource_perfect_hint);
	}

	// Range insert
	{
	  ms.clear();
	  start_counters(time_range, resource_range);
	  insert_range(strs, ms);
	  stop_counters(time_range, resource_range);
	}
      }

    std::ostringstream ostr;
    ostr << ctx << ' ' << sz << " insertions w/o hint";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_no_hint, resource_no_hint);

    ostr.str("");
    ostr << ctx << ' ' << sz << " insertions with perfect hint";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_perfect_hint, resource_perfect_hint);

    ostr.str("");
    ostr << ctx << ' ' << sz << " insertions with bad hint";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_bad_hint, resource_bad_hint);

    ostr.str("");
    ostr << ctx << ' ' << sz << " range insertions";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_range, resource_range);
  }

namespace std
{
  template<>
    struct __is_fast_hash<slow_hasher> : std::false_type
    { };
}

int main()
{
  bench<hasher>("hash code NOT cached");
  bench<slow_hasher>("hash code cached");
  return 0;
}
