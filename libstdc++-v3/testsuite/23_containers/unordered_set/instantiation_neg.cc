// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-normal-mode "" }

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-error "with noexcept" "" { target *-*-* } 270 }

#include <unordered_set>

namespace
{
  struct hash_without_noexcept
  {
    std::size_t operator() (int) const
    { return 0; }
  };
}

void
test01()
{
  using traits = std::__detail::_Hashtable_traits<false, true, true>;
  using hashtable = std::__uset_hashtable<int, hash_without_noexcept,
					  std::equal_to<int>,
					  std::allocator<int>, traits>;

  hashtable ht;
}
