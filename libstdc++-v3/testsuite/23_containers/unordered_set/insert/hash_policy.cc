// { dg-do run { target c++11 } }

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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
#include <limits>

#include <ext/throw_allocator.h>

#include <testsuite_hooks.h>

template<template<typename _Value, typename _Hash,
		  typename _Pred, typename _Alloc>
	   typename _USet>
  void test01()
  {
    // Make sure whatever happen we restore throw allocator limit at exit.
    __gnu_cxx::limit_condition::adjustor_base adj;

    typedef std::numeric_limits<std::size_t> nl_size_t;
    _USet<int, std::hash<int>, std::equal_to<int>,
	  __gnu_cxx::throw_allocator_limit<int> > us;
    const int nb = 100;
    int scheduled_throw_counter = 0;
    std::size_t thrown_exceptions = 0;
    for (int i = 0; i != nb; ++i)
      {
	if ((float)(us.size() + 1)
	    / (float)us.bucket_count() >= us.max_load_factor())
	  {
	    // We are going to need a rehash, lets introduce allocation issues:
	    __gnu_cxx::limit_condition::set_limit(scheduled_throw_counter++);
	  }
	try
	  {
	    VERIFY(us.insert(i).second);
	    scheduled_throw_counter = 0;
	  }
	catch (const __gnu_cxx::forced_error&)
	  {
	    ++thrown_exceptions;
	    --i;
	  }
	VERIFY( us.load_factor() <= us.max_load_factor() );
	__gnu_cxx::limit_condition::set_limit(nl_size_t::max());
      }

    VERIFY( thrown_exceptions != 0 );
    // Check that all values have been inserted:
    for (int i = 0; i != nb; ++i)
      {
	VERIFY( us.count(i) == 1 );
      }
  }

template<template<typename _Value, typename _Hash,
		  typename _Pred, typename _Alloc>
	   typename _USet>
  void test02()
  {
    // Make sure whatever happen we restore throw allocator limit at exit.
    __gnu_cxx::limit_condition::adjustor_base adj;

    typedef std::numeric_limits<std::size_t> nl_size_t;
    _USet<int, std::hash<int>, std::equal_to<int>,
		       __gnu_cxx::throw_allocator_limit<int> > us;
    const int nb = 100;
    int scheduled_throw_counter = 0;
    std::size_t thrown_exceptions = 0;
    for (int i = 0; i != nb; ++i)
      {
	if ((float)(us.size() + 2)
	    / (float)us.bucket_count() >= us.max_load_factor())
	  {
	    // We are going to need a rehash, lets introduce allocation issues:
	    __gnu_cxx::limit_condition::set_limit(scheduled_throw_counter++);
	  }
	try
	  {
	    std::vector<int> v = { i, i };
	    // Check the insert range robustness
	    us.insert(v.begin(), v.end());
	    scheduled_throw_counter = 0;
	  }
	catch (const __gnu_cxx::forced_error&)
	  {
	    ++thrown_exceptions;
	    --i;
	  }
	VERIFY( us.load_factor() <= us.max_load_factor() );
	__gnu_cxx::limit_condition::set_limit(nl_size_t::max());
      }

    VERIFY( thrown_exceptions != 0 );
    // Check that all values have been inserted:
    for (int i = 0; i != nb; ++i)
      {
	VERIFY( us.count(i) == 1 );
      }
  }

template<typename _Value, typename _Hash,
	 typename _Pred, typename _Alloc>
  using unordered_set_power2_rehash =
  std::_Hashtable<_Value, _Value, _Alloc,
		  std::__detail::_Identity,
		  _Pred,
		  _Hash,
		  std::__detail::_Mask_range_hashing,
		  std::__detail::_Default_ranged_hash,
		  std::__detail::_Power2_rehash_policy,
		  std::__detail::_Hashtable_traits<false, true, true>>;

int main()
{
  test01<std::unordered_set>();
  test01<unordered_set_power2_rehash>();
  test02<std::unordered_set>();
  test02<unordered_set_power2_rehash>();
  return 0;
}
