// { dg-do compile { target c++11 } }

// 2011-05-20  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

typedef std::tuple<int>                                      tt1;
typedef std::tuple<int, double>                              tt2;
typedef std::tuple<short, double, int>                       tt3;
typedef std::tuple<short, NoexceptMoveConsClass, double>     tt4;
typedef std::tuple<NoexceptMoveConsClass,
		   NoexceptMoveConsClass, double>            tt5;
typedef std::tuple<NoexceptMoveConsClass,
		   NoexceptMoveConsClass,
		   NoexceptMoveConsClass>                    tt6;
typedef std::tuple<ExceptMoveConsClass>                      tt7;
typedef std::tuple<ExceptMoveConsClass, double>              tt8;
typedef std::tuple<short, double, ExceptMoveConsClass>       tt9;
typedef std::tuple<ExceptMoveConsClass, double,
		   ExceptMoveConsClass>                      tt10;
typedef std::tuple<NoexceptMoveConsClass,
		   ExceptMoveConsClass>                      tt11;
typedef std::tuple<int,
		   NoexceptMoveConsClass,
		   ExceptMoveConsClass>                      tt12;

static_assert(std::is_nothrow_move_constructible<tt1>::value, "Error");
static_assert(std::is_nothrow_move_constructible<tt2>::value, "Error");
static_assert(std::is_nothrow_move_constructible<tt3>::value, "Error");
static_assert(std::is_nothrow_move_constructible<tt4>::value, "Error");
static_assert(std::is_nothrow_move_constructible<tt5>::value, "Error");
static_assert(std::is_nothrow_move_constructible<tt6>::value, "Error");
static_assert(!std::is_nothrow_move_constructible<tt7>::value, "Error");
static_assert(!std::is_nothrow_move_constructible<tt8>::value, "Error");
static_assert(!std::is_nothrow_move_constructible<tt9>::value, "Error");
static_assert(!std::is_nothrow_move_constructible<tt10>::value, "Error");
static_assert(!std::is_nothrow_move_constructible<tt11>::value, "Error");
static_assert(!std::is_nothrow_move_constructible<tt12>::value, "Error");
