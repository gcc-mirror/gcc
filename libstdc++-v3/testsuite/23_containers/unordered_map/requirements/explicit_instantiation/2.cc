// { dg-do compile { target c++11 } }

// Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <testsuite_hooks.h>
#include <testsuite_api.h>

typedef __gnu_test::NonDefaultConstructible      inst_type;
typedef __gnu_test::NonDefaultConstructible_hash hash_type;

// N.B. We cannot instantiate with mapped_type == NonDefaultConstructible
// because of 23.5.1.2.
template class std::unordered_map<inst_type, double, hash_type>;
