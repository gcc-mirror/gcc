// { dg-do compile }

// Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

// 6.3 Unordered associative containers

#include <tr1/unordered_set>

void
test01()
{
  std::tr1::unordered_set<int>::iterator                it;
  std::tr1::unordered_set<int>::const_iterator         cit;
  std::tr1::unordered_set<int>::local_iterator         lit;
  std::tr1::unordered_set<int>::const_local_iterator  clit;
}
