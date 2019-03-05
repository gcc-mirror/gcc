// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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
  const std::string pattern = "test string #";
  const int nb_copies = 100;

  // Special std::string hasher based on std::hash<std::string> but not tag as
  // slow so that hash code is not cached. It is easier to show impact of
  // hinting in this context.
  struct hasher
  {
    std::size_t
    operator()(const std::string& str) const noexcept
    {
      //std::istringstream istr(str.substr(pattern.size()));
      //std::size_t str_index;
      //istr >> str_index;
      //return str_index;
      std::hash<std::string> std_hasher;
      return std_hasher(str);
    }
  };

  using ums_t = std::unordered_multiset<std::string, hasher>;

  void
  insert_with_perfect_hint1(const std::vector<std::string>& strs,
			    ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(str));

    for (int j = 1; j != nb_copies; ++j)
      for (std::size_t i = 0; i != strs.size(); ++i)
	ms.insert(hints[i], strs[i]);
  }

  void
  insert_with_perfect_hint2(const std::vector<std::string>& strs,
			    ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(str));

    for (std::size_t i = 0; i != strs.size(); ++i)
      for (int j = 1; j != nb_copies; ++j)
	ms.insert(hints[i], strs[i]);
  }

  // Always insert with the result of the previous insertion. The result of
  // the previous insertion will never be followed by an equivalent node
  // resulting in a re-computation of its hash code which is expensive.
  void
  insert_with_good_hint(const std::vector<std::string>& strs,
			ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(str));

    for (int j = 1; j != nb_copies; ++j)
      for (std::size_t i = 0; i != strs.size(); ++i)
	hints[i] = ms.insert(hints[i], strs[i]);
  }

  // Note that the following use case is particularly bad especially compared to
  // the solution without hint because without hint the first insertion will put
  // it first in the bucket and following insertions will detect it and insert
  // just before. By giving a hint insertion will be done just after forcing to
  // check if it has no impact on the following bucket.
  void
  insert_with_bad_hint(const std::vector<std::string>& strs,
		       ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(str));

    for (std::size_t i = 0; i != strs.size(); ++i)
      for (int j = 1; j != nb_copies; ++j)
	hints[i] = ms.insert(hints[i], strs[i]);
  }

  void
  insert_without_hint1(const std::vector<std::string>& strs,
		       ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(str));

    for (int j = 1; j != nb_copies; ++j)
      for (std::size_t i = 0; i != strs.size(); ++i)
	hints[i] = ms.insert(strs[i]);
  }

  // This version is the best one if you insert all equivalent elements at the
  // same time. It demonstrates that most of the time it is better not to give
  // any hint unless you have written a benchmark for your application showing
  // that it does have a positive effect.
  void
  insert_without_hint2(const std::vector<std::string>& strs,
		       ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(str));

    for (std::size_t i = 0; i != strs.size(); ++i)
      for (int j = 1; j != nb_copies; ++j)
	hints[i] = ms.insert(strs[i]);
  }

  void
  insert_with_any_hint1(const std::vector<std::string>& strs,
			ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(ms.begin(), str));

    std::size_t offset = strs.size() / 2;
    for (int j = 1; j != nb_copies; ++j)
      for (std::size_t i = 0; i != strs.size(); ++i)
	{
	  ms.insert(hints[(i + offset) % hints.size()], strs[i]);
	  ++offset;
	}
  }

  void
  insert_with_any_hint2(const std::vector<std::string>& strs,
			ums_t& ms)
  {
    std::vector<typename ums_t::iterator> hints;
    hints.reserve(strs.size());
    for (auto str : strs)
      hints.push_back(ms.insert(ms.begin(), str));

    std::size_t offset = strs.size() / 2;
    for (std::size_t i = 0; i != strs.size(); ++i)
      for (int j = 1; j != nb_copies; ++j)
	{
	  ms.insert(hints[(i + offset) % hints.size()], strs[i]);
	  ++offset;
	}
  }
}

int main()
{
  using namespace __gnu_test;

  const int nb_iter = 10;

  std::vector<std::string> strs;
  strs.reserve(sz / nb_copies);

  for (int i = 0; i != sz / nb_copies; ++i)
    {
      std::ostringstream osstr;
      osstr << pattern << i;
      strs.push_back(osstr.str());
    }

  ums_t ms;
  // Use a large load factor to make the context ideal for using hint because we
  // will have many elements per bucket.
  ms.max_load_factor(10.0f);
  ms.reserve(sz);

  // Warm up.
  {
    for (auto str : strs)
      for (int j = 0; j != nb_copies; ++j)
	ms.insert(str);
  }

  time_counter time_no_hint1, time_any_hint1, time_bad_hint, time_perfect_hint1;
  time_counter time_no_hint2, time_any_hint2, time_good_hint, time_perfect_hint2;
  resource_counter resource_no_hint1, resource_any_hint1, resource_bad_hint,
    resource_perfect_hint1;
  resource_counter resource_no_hint2, resource_any_hint2, resource_good_hint,
    resource_perfect_hint2;

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
	start_counters(time_no_hint1, resource_no_hint1);
	insert_without_hint1(strs, ms);
	stop_counters(time_no_hint1, resource_no_hint1);
      }

      // Any hint
      {
	ms.clear();
	start_counters(time_any_hint1, resource_any_hint1);
	insert_with_any_hint1(strs, ms);
	stop_counters(time_any_hint1, resource_any_hint1);
      }

      // Good hint
      {
	ms.clear();
	start_counters(time_good_hint, resource_good_hint);
	insert_with_good_hint(strs, ms);
	stop_counters(time_good_hint, resource_good_hint);
      }

      // No hint
      {
	ms.clear();
	start_counters(time_no_hint2, resource_no_hint2);
	insert_without_hint2(strs, ms);
	stop_counters(time_no_hint2, resource_no_hint2);
      }

      // Perfect hint
      {
	ms.clear();
	start_counters(time_perfect_hint2, resource_perfect_hint2);
	insert_with_perfect_hint2(strs, ms);
	stop_counters(time_perfect_hint2, resource_perfect_hint2);
      }

      // Any hint
      {
	ms.clear();
	start_counters(time_any_hint2, resource_any_hint2);
	insert_with_any_hint2(strs, ms);
	stop_counters(time_any_hint2, resource_any_hint2);
      }

      // Perfect hint
      {
	ms.clear();
	start_counters(time_perfect_hint1, resource_perfect_hint1);
	insert_with_perfect_hint1(strs, ms);
	stop_counters(time_perfect_hint1, resource_perfect_hint1);
      }
    }

  std::ostringstream ostr;
  ostr << "unordered_set " << nb_copies << " X " << sz / nb_copies
       << " insertions w/o hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_no_hint1, resource_no_hint1);

  ostr.str("");
  ostr << "unordered_set " << nb_copies << " X " << sz / nb_copies
       << " insertions with any hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_any_hint1, resource_any_hint1);

  ostr.str("");
  ostr << "unordered_set " << nb_copies << " X " << sz / nb_copies
       << " insertions with good hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_good_hint, resource_good_hint);

  ostr.str("");
  ostr << "unordered_set " << nb_copies << " X " << sz / nb_copies
       << " insertions with perfect hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_perfect_hint1, resource_perfect_hint1);

  ostr.str("");
  ostr << "unordered_set " << sz / nb_copies << " X " << nb_copies
       << " insertions w/o hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_no_hint2, resource_no_hint2);

  ostr.str("");
  ostr << "unordered_set " << sz / nb_copies << " X " << nb_copies
       << " insertions with any hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_any_hint2, resource_any_hint2);

  ostr.str("");
  ostr << "unordered_set " << sz / nb_copies << " X " << nb_copies
       << " insertions with bad hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_bad_hint, resource_bad_hint);

  ostr.str("");
  ostr << "unordered_set " << sz / nb_copies << " X " << nb_copies
       << " insertions with perfect hint";
  report_performance(__FILE__, ostr.str().c_str(),
		     time_perfect_hint2, resource_perfect_hint2);
  return 0;
}
