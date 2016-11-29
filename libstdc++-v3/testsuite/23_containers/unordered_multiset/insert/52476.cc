// { dg-do run { target c++11 } }
//
// Copyright (C) 2012-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <unordered_set>
#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>

namespace
{
  struct pair_hash
  {
    std::size_t
    operator()(const std::pair<int, int>& p) const noexcept
    { return std::hash<int>()(p.first); }
  };

  struct pair_equal_to
  {
    bool
    operator()(const std::pair<int, int>& x,
	       const std::pair<int, int>& y) const noexcept
    { return x.first == y.first; }
  };
}

void test01()
{
  using namespace std;
 
  unordered_multiset<pair<int, int>, pair_hash, pair_equal_to> mset;
  vector<int> values;

  size_t nb_bkts = mset.bucket_count();
  int i = 0;
  for (;; ++i)
    {
      mset.insert(make_pair(0, i));
      if (mset.bucket_count() != nb_bkts)
	// Container got rehash
	break;
      values.clear();
      transform(mset.begin(), mset.end(), back_inserter(values),
		[](const pair<int, int>& p) { return p.second; });
    }

  vector<int> rehash_values;
  transform(mset.begin(), mset.end(), back_inserter(rehash_values),
	    [](const pair<int, int>& p) { return p.second; });
  // Remove the value that result in a rehash
  rehash_values.erase(remove(rehash_values.begin(), rehash_values.end(), i));

  VERIFY( rehash_values == values );
}
  
int main()
{
  test01();
  return 0;
}
