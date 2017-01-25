// { dg-do compile { target c++11 } }
// { dg-require-normal-mode "" }

// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// Test that the C++11 variants have an ABI tag

#include <map>

using container = std::map<int, int>;
using iterator = typename container::iterator;
using const_iterator = typename container::const_iterator;

// { dg-final { scan-assembler "_ZNSt3mapIiiSt4lessIiESaISt4pairIKiiEEE5eraseB5cxx11ESt17_Rb_tree_iteratorIS4_E" } }
iterator (container::*p1)(iterator) = &container::erase;
