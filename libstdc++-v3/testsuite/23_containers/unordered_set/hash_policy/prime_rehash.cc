// Copyright (C) 2016 Free Software Foundation, Inc.
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
// { dg-options "-std=gnu++11" }

#include <limits>
#include <unordered_set>

#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  std::__detail::_Prime_rehash_policy policy;

  for (std::size_t i = 1;;)
    {
      auto nxt = policy._M_next_bkt(i);

      if (nxt == i)
	{
	  // Equals only when reaching max.
	  constexpr auto mx = std::numeric_limits<std::size_t>::max() - 1;
	  VERIFY( nxt == policy._M_next_bkt(mx) );
	  break;
	}

      VERIFY( nxt > i );
      i = nxt;
    }
}

int main()
{
  test01();
  return 0;
}
