// { dg-do compile { target c++11 } }

// 2011-05-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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
typedef std::tuple<ExceptMoveAssignClass>                      tt5;
typedef std::tuple<ExceptMoveAssignClass, double>              tt6;
typedef std::tuple<short, double, ExceptMoveAssignClass>       tt7;
typedef std::tuple<ExceptMoveConsClass>                        tt8;
typedef std::tuple<int, ExceptMoveConsClass>                   tt9;
typedef std::tuple<ExceptMoveConsClass, short, double>         tt10;
typedef std::tuple<short, NoexceptMoveConsClass, double>       tt11;
typedef std::tuple<NoexceptMoveConsClass>                      tt12;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass>    tt13;
typedef std::tuple<ExceptMoveConsNoexceptMoveAssignClass>      tt14;
typedef std::tuple<NoexceptMoveConsExceptMoveAssignClass>      tt15;
typedef std::tuple<ExceptMoveConsExceptMoveAssignClass>        tt16;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   double>                                     tt17;
typedef std::tuple<double,
		   NoexceptMoveConsNoexceptMoveAssignClass,
		   short>                                      tt18;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   NoexceptMoveConsNoexceptMoveAssignClass,
		   char>                                       tt19;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   NoexceptMoveConsNoexceptMoveAssignClass,
		   NoexceptMoveConsNoexceptMoveAssignClass>    tt20;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   ExceptMoveConsNoexceptMoveAssignClass,
		   NoexceptMoveConsNoexceptMoveAssignClass>    tt21;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   ExceptMoveConsNoexceptMoveAssignClass,
		   NoexceptMoveConsExceptMoveAssignClass>      tt22;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   NoexceptMoveConsNoexceptMoveAssignClass,
		   ExceptMoveConsExceptMoveAssignClass>        tt23;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   short, ExceptMoveConsExceptMoveAssignClass> tt24;
typedef std::tuple<NoexceptMoveConsNoexceptMoveAssignClass,
		   short, ExceptMoveConsExceptMoveAssignClass> tt25;

static_assert(noexcept(std::declval<tt1&>().swap(std::declval<tt1&>())),
	      "Error");
static_assert(noexcept(std::declval<tt2&>().swap(std::declval<tt2&>())),
	      "Error");
static_assert(noexcept(std::declval<tt3&>().swap(std::declval<tt3&>())),
	      "Error");
static_assert(noexcept(std::declval<tt4&>().swap(std::declval<tt4&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt5&>().swap(std::declval<tt5&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt6&>().swap(std::declval<tt6&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt7&>().swap(std::declval<tt7&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt8&>().swap(std::declval<tt8&>())),
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
static_assert(noexcept(std::declval<tt18&>().swap(std::declval<tt18&>())),
	      "Error");
static_assert(noexcept(std::declval<tt19&>().swap(std::declval<tt19&>())),
	      "Error");
static_assert(noexcept(std::declval<tt20&>().swap(std::declval<tt20&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt21&>().swap(std::declval<tt21&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt22&>().swap(std::declval<tt22&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt23&>().swap(std::declval<tt23&>())),
	      "Error");
static_assert(!noexcept(std::declval<tt24&>().swap(std::declval<tt24&>())),
	      "Error");
