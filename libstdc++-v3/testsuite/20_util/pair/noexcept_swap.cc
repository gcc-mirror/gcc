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

#include <utility>
#include <testsuite_tr1.h>

using namespace __gnu_test;

typedef std::pair<int, int>                                   tt1;
typedef std::pair<int, double>                                tt2;
typedef std::pair<short, NoexceptMoveAssignClass>             tt4;
typedef std::pair<ExceptMoveAssignClass, double>              tt6;
typedef std::pair<int, ExceptMoveConsClass>                   tt9;
typedef std::pair<ExceptMoveAssignClass, short>               tt10;
typedef std::pair<short, NoexceptMoveConsClass>               tt11;
typedef std::pair<NoexceptMoveConsClass,
		  NoexceptMoveConsClass>                      tt12;
typedef std::pair<NoexceptMoveConsNoexceptMoveAssignClass,
		  NoexceptMoveConsNoexceptMoveAssignClass>    tt13;
typedef std::pair<ExceptMoveConsNoexceptMoveAssignClass,
		  ExceptMoveConsNoexceptMoveAssignClass>      tt14;
typedef std::pair<NoexceptMoveConsExceptMoveAssignClass,
		  NoexceptMoveConsExceptMoveAssignClass>      tt15;
typedef std::pair<ExceptMoveConsExceptMoveAssignClass,
		  ExceptMoveConsExceptMoveAssignClass>        tt16;
typedef std::pair<NoexceptMoveConsNoexceptMoveAssignClass,
		  double>                                     tt17;
typedef std::pair<NoexceptMoveConsNoexceptMoveAssignClass,
		  NoexceptMoveConsNoexceptMoveAssignClass>    tt19;
typedef std::pair<NoexceptMoveConsNoexceptMoveAssignClass,
		  ExceptMoveConsNoexceptMoveAssignClass>      tt21;

static_assert(noexcept(std::declval<tt1&>().swap(std::declval<tt1&>())),
	      "Error");
static_assert(noexcept(std::declval<tt2&>().swap(std::declval<tt2&>())),
	      "Error");
static_assert(noexcept(std::declval<tt4&>().swap(std::declval<tt4&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt6&>().swap(std::declval<tt6&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt9&>().swap(std::declval<tt9&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt10&>().swap(std::declval<tt10&>())),
	      "Error");
static_assert(noexcept(std::declval<tt11&>().swap(std::declval<tt11&>())),
	      "Error");
static_assert(noexcept(std::declval<tt12&>().swap(std::declval<tt12&>())),
	      "Error");
static_assert(noexcept(std::declval<tt13&>().swap(std::declval<tt13&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt14&>().swap(std::declval<tt14&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt15&>().swap(std::declval<tt15&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt16&>().swap(std::declval<tt16&>())),
	      "Error");
static_assert(noexcept(std::declval<tt17&>().swap(std::declval<tt17&>())),
	      "Error");
static_assert(noexcept(std::declval<tt19&>().swap(std::declval<tt19&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt21&>().swap(std::declval<tt21&>())),
	      "Error");
