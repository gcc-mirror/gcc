// { dg-do compile { target c++11 } }
// { dg-excess-errors "XFAIL because of PR libstdc++/55043 fix" }

// Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

#include <unordered_map>

struct LinkedHashMap
{
  struct Entry;

  typedef std::unordered_multimap<int, Entry> Storage;
  typedef Storage::iterator EntryPtr;

  struct Entry
  {
    EntryPtr prev, next;
  };
};
