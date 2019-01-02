// { dg-do compile { target c++11 } }

// 2011-05-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

#include <tuple>
#include <testsuite_tr1.h>

using namespace __gnu_test;

typedef std::tuple<int>                                        tt1;
typedef std::tuple<int, double>                                tt2;
typedef std::tuple<short, double, int>                         tt3;
typedef std::tuple<short, NoexceptMoveAssignClass, double>     tt4;
typedef std::tuple<NoexceptMoveAssignClass,
		   NoexceptMoveAssignClass, double>            tt5;
typedef std::tuple<NoexceptMoveAssignClass,
		   NoexceptMoveAssignClass,
		   NoexceptMoveAssignClass>                    tt6;
typedef std::tuple<ExceptMoveAssignClass>                      tt7;
typedef std::tuple<ExceptMoveAssignClass, double>              tt8;
typedef std::tuple<short, double, ExceptMoveAssignClass>       tt9;
typedef std::tuple<ExceptMoveAssignClass, double,
		   ExceptMoveAssignClass>                      tt10;
typedef std::tuple<NoexceptMoveAssignClass,
		   ExceptMoveAssignClass>                      tt11;
typedef std::tuple<int,
		   NoexceptMoveAssignClass,
		   ExceptMoveAssignClass>                      tt12;

static_assert(std::is_nothrow_move_assignable<tt1>::value, "Error");
static_assert(std::is_nothrow_move_assignable<tt2>::value, "Error");
static_assert(std::is_nothrow_move_assignable<tt3>::value, "Error");
static_assert(std::is_nothrow_move_assignable<tt4>::value, "Error");
static_assert(std::is_nothrow_move_assignable<tt5>::value, "Error");
static_assert(std::is_nothrow_move_assignable<tt6>::value, "Error");
static_assert(!std::is_nothrow_move_assignable<tt7>::value, "Error");
static_assert(!std::is_nothrow_move_assignable<tt8>::value, "Error");
static_assert(!std::is_nothrow_move_assignable<tt9>::value, "Error");
static_assert(!std::is_nothrow_move_assignable<tt10>::value, "Error");
static_assert(!std::is_nothrow_move_assignable<tt11>::value, "Error");
static_assert(!std::is_nothrow_move_assignable<tt12>::value, "Error");
