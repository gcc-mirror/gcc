// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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
//
// { dg-do run { target c++11 } }

#include <unordered_set>

#include <testsuite_hooks.h>

template<typename _USet>
  void test()
  {
    _USet us;
    typedef typename _USet::size_type size_type;
    bool rehashed = false;
    for (int i = 0; i != 100000; ++i)
      {
	size_type bkt_count = us.bucket_count();
	us.insert(i);
	if (bkt_count != us.bucket_count())
	  {
	    // Container has been rehashed, lets check that it won't be rehash
	    // again if we remove and restore the last 2 inserted elements:
	    rehashed = true;
	    bkt_count = us.bucket_count();
	    VERIFY( us.erase(i) == 1 );
	    VERIFY( bkt_count == us.bucket_count() );
	    if (i > 0)
	      {
		VERIFY( us.erase(i - 1) == 1 );
		VERIFY( bkt_count == us.bucket_count() );

		VERIFY( us.insert(i - 1).second );
		VERIFY( bkt_count == us.bucket_count() );
	      }
	    VERIFY( us.insert(i).second );
	    VERIFY( bkt_count == us.bucket_count() );
	  }
      }

    // At lest we check a rehash once:
    VERIFY( rehashed );
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
