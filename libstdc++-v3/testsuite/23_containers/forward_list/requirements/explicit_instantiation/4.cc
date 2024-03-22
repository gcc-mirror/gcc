
// 2010-05-20  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

#include <forward_list>
#include <testsuite_hooks.h>
#include <testsuite_api.h>

// { dg-do compile { target c++11 } }

// libstdc++/41792
template class std::forward_list<__gnu_test::OverloadedAddress>;
