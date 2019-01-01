// { dg-do compile { target c++11 } }

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

#include <iterator>
#include <type_traits>
#include <vector>

typedef std::vector<bool>	Vec;
typedef Vec::reference		Ref;
typedef Vec::const_reference	CRef;
typedef Vec::iterator		It;
typedef Vec::const_iterator	CIt;
typedef std::move_iterator<It>	MIt;
typedef std::move_iterator<CIt>	MCIt;
static_assert(std::is_same<MIt::reference, Ref>::value,"");
static_assert(std::is_same<MCIt::reference, CRef>::value,"");
