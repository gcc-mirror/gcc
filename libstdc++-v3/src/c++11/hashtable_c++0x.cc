// std::__detail definitions -*- C++ -*-

// Copyright (C) 2007-2013 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#if __cplusplus < 201103L
# error "hashtable_c++0x.cc must be compiled with -std=gnu++0x"
#endif

#include <initializer_list>
#include <tuple>
#include <bits/hashtable_policy.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
#include "../shared/hashtable-aux.cc"

namespace __detail
{
  _GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Return a prime no smaller than n.
  std::size_t
  _Prime_rehash_policy::_M_next_bkt(std::size_t __n) const
  {
    // Optimize lookups involving the first elements of __prime_list.
    // (useful to speed-up, eg, constructors)
    static const unsigned char __fast_bkt[12]
      = { 2, 2, 2, 3, 5, 5, 7, 7, 11, 11, 11, 11 };

    if (__n <= 11)
      {
	_M_next_resize =
	  __builtin_ceil(__fast_bkt[__n] * (long double)_M_max_load_factor);
	return __fast_bkt[__n];
      }

    const unsigned long* __next_bkt =
      std::lower_bound(__prime_list + 5, __prime_list + _S_n_primes, __n);
    _M_next_resize =
      __builtin_ceil(*__next_bkt * (long double)_M_max_load_factor);
    return *__next_bkt;
  }

  // Finds the smallest prime p such that alpha p > __n_elt + __n_ins.
  // If p > __n_bkt, return make_pair(true, p); otherwise return
  // make_pair(false, 0).  In principle this isn't very different from
  // _M_bkt_for_elements.

  // The only tricky part is that we're caching the element count at
  // which we need to rehash, so we don't have to do a floating-point
  // multiply for every insertion.

  std::pair<bool, std::size_t>
  _Prime_rehash_policy::
  _M_need_rehash(std::size_t __n_bkt, std::size_t __n_elt,
		 std::size_t __n_ins) const
  {
    if (__n_elt + __n_ins >= _M_next_resize)
      {
	long double __min_bkts = (__n_elt + __n_ins)
				   / (long double)_M_max_load_factor;
	if (__min_bkts >= __n_bkt)
	  return std::make_pair(true,
	    _M_next_bkt(std::max<std::size_t>(__builtin_floor(__min_bkts) + 1,
					      __n_bkt * _S_growth_factor)));

	_M_next_resize
	  = __builtin_floor(__n_bkt * (long double)_M_max_load_factor);
	return std::make_pair(false, 0);
      }
    else
      return std::make_pair(false, 0);
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std
