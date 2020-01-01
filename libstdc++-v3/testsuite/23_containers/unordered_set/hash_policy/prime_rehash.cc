// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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
  std::__detail::_Prime_rehash_policy policy;

  // Starts at 4 because 2 & 3 are two consecutives primes that make this test
  // fail.
  for (std::size_t i = 4;;)
    {
      auto nxt = policy._M_next_bkt(i);

      if (nxt <= i)
	{
	  // Lower or equals only when reaching max prime.
	  constexpr auto mx = std::numeric_limits<std::size_t>::max();
	  VERIFY( nxt == policy._M_next_bkt(mx) );
	  break;
	}

      VERIFY( nxt > i );
      i = nxt + 1;
    }
}

int main()
{
  test01();
  return 0;
}
