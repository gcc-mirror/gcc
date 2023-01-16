// std::__detail definitions -*- C++ -*-

// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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
#include <ext/aligned_buffer.h>
#include <ext/alloc_traits.h>
#include <bits/functional_hash.h>
#include <bits/hashtable_policy.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#include "../shared/hashtable-aux.cc"

namespace __detail
{
  // Return a prime no smaller than n.
  std::size_t
  _Prime_rehash_policy::_M_next_bkt(std::size_t __n) const
  {
    // Optimize lookups involving the first elements of __prime_list.
    // (useful to speed-up, eg, constructors)
    static const unsigned char __fast_bkt[]
      = { 2, 2, 2, 3, 5, 5, 7, 7, 11, 11, 11, 11, 13, 13 };

    if (__n < sizeof(__fast_bkt))
      {
	if (__n == 0)
	  // Special case on container 1st initialization with 0 bucket count
	  // hint. We keep _M_next_resize to 0 to make sure that next time we
	  // want to add an element allocation will take place.
	  return 1;

	_M_next_resize =
	  __builtin_floor(__fast_bkt[__n] * (double)_M_max_load_factor);
	return __fast_bkt[__n];
      }

    // Number of primes (without sentinel).
    constexpr auto __n_primes
      = sizeof(__prime_list) / sizeof(unsigned long) - 1;

    // Don't include the last prime in the search, so that anything
    // higher than the second-to-last prime returns a past-the-end
    // iterator that can be dereferenced to get the last prime.
    constexpr auto __last_prime = __prime_list + __n_primes - 1;

    const unsigned long* __next_bkt =
      std::lower_bound(__prime_list + 6, __last_prime, __n);

    if (__next_bkt == __last_prime)
      // Set next resize to the max value so that we never try to rehash again
      // as we already reach the biggest possible bucket number.
      // Note that it might result in max_load_factor not being respected.
      _M_next_resize = size_t(-1);
    else
      _M_next_resize =
	__builtin_floor(*__next_bkt * (double)_M_max_load_factor);

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
    if (__n_elt + __n_ins > _M_next_resize)
      {
	// If _M_next_resize is 0 it means that we have nothing allocated so
	// far and that we start inserting elements. In this case we start
	// with an initial bucket size of 11.
	double __min_bkts
	  = std::max<std::size_t>(__n_elt + __n_ins, _M_next_resize ? 0 : 11)
	  / (double)_M_max_load_factor;
	if (__min_bkts >= __n_bkt)
	  return { true,
	    _M_next_bkt(std::max<std::size_t>(__builtin_floor(__min_bkts) + 1,
					      __n_bkt * _S_growth_factor)) };

	_M_next_resize
	  = __builtin_floor(__n_bkt * (double)_M_max_load_factor);
	return { false, 0 };
      }
    else
      return { false, 0 };
  }
} // namespace __detail

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
