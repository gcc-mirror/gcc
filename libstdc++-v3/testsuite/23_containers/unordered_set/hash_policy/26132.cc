// { dg-do run { target c++11 } }

// 2010-08-13  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

// libstdc++/26132
template<typename _USet>
  void test()
  {
    for (float lf = 1.0; lf < 101.0; lf *= 10.0)
      for (int size = 1; size <= 6561; size *= 3)
	{
	  _USet us1;
	  typedef typename _USet::size_type size_type;

	  us1.max_load_factor(10.0);

	  for (int i = 0; i < size; ++i)
	    us1.insert(i);

	  us1.max_load_factor(lf);

	  for (int i = 1; i <= 6561; i *= 81)
	    {
	      const size_type n = size * 81 / i;
	      us1.rehash(n);
	      VERIFY( us1.bucket_count() > us1.size() / us1.max_load_factor() );
	      VERIFY( us1.bucket_count() >= n );
	    }
	}
  }

template<typename _Value>
  using unordered_set_power2_rehash =
  std::_Hashtable<_Value, _Value, std::allocator<_Value>,
		  std::__detail::_Identity,
		  std::equal_to<_Value>,
		  std::hash<_Value>,
		  std::__detail::_Mask_range_hashing,
		  std::__detail::_Default_ranged_hash,
		  std::__detail::_Power2_rehash_policy,
		  std::__detail::_Hashtable_traits<false, true, true>>;

int main()
{
  test<std::unordered_set<int>>();
  test<unordered_set_power2_rehash<int>>();
  return 0;
}
