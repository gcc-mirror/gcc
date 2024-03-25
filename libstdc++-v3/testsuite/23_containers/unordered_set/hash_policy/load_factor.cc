// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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
    {
      _USet us;
      for (int i = 0; i != 100000; ++i)
	{
	  us.insert(i);
	  VERIFY( us.load_factor() <= us.max_load_factor() );
	}
    }
    {
      _USet us;
      us.max_load_factor(3.f);
      for (int i = 0; i != 100000; ++i)
	{
	  us.insert(i);
	  VERIFY( us.load_factor() <= us.max_load_factor() );
	}
    }
    {
      _USet us;
      us.max_load_factor(.3f);
      for (int i = 0; i != 100000; ++i)
	{
	  us.insert(i);
	  VERIFY( us.load_factor() <= us.max_load_factor() );
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
