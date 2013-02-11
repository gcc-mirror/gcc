// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

#include <string>
#include <unordered_set>

namespace
{
  template<typename _Tp>
  struct final_hash final
  {
    std::size_t operator() (const _Tp&) const noexcept
    { return 0; }
  };
}

// A non-integral type:
template class std::unordered_multiset<std::string, final_hash<std::string>>;

// An integral type;
template class std::unordered_multiset<int, final_hash<int>>;

