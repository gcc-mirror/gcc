// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#ifndef SIMD_TESTS_BITS_MAKE_VEC_H_
#define SIMD_TESTS_BITS_MAKE_VEC_H_
#include <experimental/simd>

template <class M>
  inline M
  make_mask(const std::initializer_list<bool> &init)
  {
    std::size_t i = 0;
    M r = {};
    for (;;)
      {
	for (bool x : init)
	  {
	    r[i] = x;
	    if (++i == M::size())
	      {
		return r;
	      }
	  }
      }
  }

template <class M>
  M
  make_alternating_mask()
  {
    return make_mask<M>({false, true});
  }

template <class V>
  inline V
  make_vec(const std::initializer_list<typename V::value_type> &init,
	   typename V::value_type inc = 0)
  {
    std::size_t i = 0;
    V r = {};
    typename V::value_type base = 0;
    for (;;)
      {
	for (auto x : init)
	  {
	    r[i] = base + x;
	    if (++i == V::size())
	      {
		return r;
	      }
	  }
	base += inc;
      }
  }
#endif  // SIMD_TESTS_BITS_MAKE_VEC_H_
