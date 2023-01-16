// { dg-options "-std=gnu++98" }
// { dg-do compile }

// Copyright (C) 2009-2023 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// This file tests explicit instantiation of library containers.

#include <queue>

template class std::priority_queue<int>;

struct Cmp : std::less<int> {
  Cmp(int) { }
};
template class std::priority_queue<int, std::vector<int>, Cmp>;

#ifndef _GLIBCXX_CONCEPT_CHECKS
struct NonDefaultConstructible : std::vector<int> {
  NonDefaultConstructible(int) { }
};
template class std::priority_queue<int, NonDefaultConstructible>;
template class std::priority_queue<int, NonDefaultConstructible, Cmp>;
#endif
