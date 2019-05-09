// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

#include <limits>
#include <unordered_set>

#include <testsuite_hooks.h>

void test01()
{
  std::__detail::_Power2_rehash_policy policy;
  VERIFY( policy._M_next_bkt(1) == 2 );
  VERIFY( policy._M_next_bkt(2) == 2 );
  VERIFY( policy._M_next_bkt(3) == 4 );
  VERIFY( policy._M_next_bkt(5) == 8 );
  VERIFY( policy._M_next_bkt(16) == 16 );
  VERIFY( policy._M_next_bkt(33) == 64 );
  VERIFY( policy._M_next_bkt((std::size_t(1) << (sizeof(std::size_t) * 8 - 2)) + 1)
	  == (std::size_t(1) << (sizeof(std::size_t) * 8 - 1)) );
}

void test02()
{
  std::__detail::_Power2_rehash_policy policy;

  for (std::size_t i = 3;;)
    {
      auto nxt = policy._M_next_bkt(i);

      if (nxt <= i)
	{
	  // Lower or equal only when reaching max.
	  constexpr auto mx = std::numeric_limits<std::size_t>::max();
	  VERIFY( nxt == policy._M_next_bkt(mx) );
	  break;
	}

      VERIFY( nxt >= i );
      i = nxt + 1;
    }
}

int main()
{
  test01();
  test02();
  return 0;
}
