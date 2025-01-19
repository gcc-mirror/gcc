// Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

#include <deque>
#include <testsuite_api.h>

// { dg-options "-std=gnu++98" }
// { dg-do compile { target c++98_only } }

// N.B. Since C++11 we cannot instantiate with T == NonDefaultConstructible
// because of [deque.cons] p4: "Requires: T shall be DefaultConstructible."
template class std::deque<__gnu_test::NonDefaultConstructible>;
