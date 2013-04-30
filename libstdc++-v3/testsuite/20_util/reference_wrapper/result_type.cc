// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// 2010-10-06  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

#include <functional>

using namespace std;

struct T;

reference_wrapper<int(float, ...)>::result_type                       i01;
// reference_wrapper<int(float, ...) const>::result_type                 i02;
// reference_wrapper<int(float, ...) volatile>::result_type              i03;
// reference_wrapper<int(float, ...) const volatile>::result_type        i04;

reference_wrapper<int(*)(float, ...)>::result_type                    i05;
reference_wrapper<int(* const)(float, ...)>::result_type              i06;
reference_wrapper<int(* volatile)(float, ...)>::result_type           i07;
reference_wrapper<int(* const volatile)(float, ...)>::result_type     i08;

reference_wrapper<int(T::*)(float, ...)>::result_type                 i09;
reference_wrapper<int(T::*)(float, ...) const>::result_type           i10;
reference_wrapper<int(T::*)(float, ...) volatile>::result_type        i11;
reference_wrapper<int(T::*)(float, ...) const volatile>::result_type  i12;
