// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

// This file tests explicit instantiation of library containers
// with an allocator for a different value_type (which is a GNU extension).

#include <list>

// { dg-do compile }
// The extension that implicitly rebinds allocators is in gnu++98/11/14/17 only
// { dg-skip-if "" { *-*-* } { "-std=c++*" "-std=gnu++2*" "-D_GLIBCXX_CONCEPT_CHECKS*" } }

// libstdc++/21770
template class std::list<int, std::allocator<char> >;
