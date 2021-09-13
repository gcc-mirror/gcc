// { dg-do compile { target c++11 } }
// { dg-skip-if "argument_type removed for C++20" { c++2a } }

// 2010-10-06  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

reference_wrapper<int(float)>::argument_type                               i01;
// reference_wrapper<int(float) const>::argument_type                         i02;
// reference_wrapper<int(float) volatile>::argument_type                      i03;
// reference_wrapper<int(float) const volatile>::argument_type                i04;
reference_wrapper<int(float)>::result_type                                 i05;
// reference_wrapper<int(float) const>::result_type                           i06;
// reference_wrapper<int(float) volatile>::result_type                        i07;
// reference_wrapper<int(float) const volatile>::result_type                  i08;

reference_wrapper<int(*)(float)>::argument_type                            i09;
reference_wrapper<int(* const)(float)>::argument_type                      i10;
reference_wrapper<int(* volatile)(float)>::argument_type                   i11;
reference_wrapper<int(* const volatile)(float)>::argument_type             i12;
reference_wrapper<int(*)(float)>::result_type                              i13;
reference_wrapper<int(* const)(float)>::result_type                        i14;
reference_wrapper<int(* volatile)(float)>::result_type                     i15;
reference_wrapper<int(* const volatile)(float)>::result_type               i16;

reference_wrapper<int(float, char)>::first_argument_type                   i17;
// reference_wrapper<int(float, char) const>::first_argument_type             i18;
// reference_wrapper<int(float, char) volatile>::first_argument_type          i19;
// reference_wrapper<int(float, char) const volatile>::first_argument_type    i20;
reference_wrapper<int(float, char)>::second_argument_type                  i21;
// reference_wrapper<int(float, char) const>::second_argument_type            i22;
// reference_wrapper<int(float, char) volatile>::second_argument_type         i23;
// reference_wrapper<int(float, char) const volatile>::second_argument_type   i24;
reference_wrapper<int(float, char)>::result_type                           i25;
// reference_wrapper<int(float, char) const>::result_type                     i26;
// reference_wrapper<int(float, char) volatile>::result_type                  i27;
// reference_wrapper<int(float, char) const volatile>::result_type            i28;

reference_wrapper<int(*)(float, char)>::first_argument_type                i29;
reference_wrapper<int(* const)(float, char)>::first_argument_type          i30;
reference_wrapper<int(* volatile)(float, char)>::first_argument_type       i31;
reference_wrapper<int(* const volatile)(float, char)>::first_argument_type i32;
reference_wrapper<int(*)(float, char)>::second_argument_type               i33;
reference_wrapper<int(* const)(float, char)>::second_argument_type         i34;
reference_wrapper<int(* volatile)(float, char)>::second_argument_type      i35;
reference_wrapper<int(*const volatile)(float, char)>::second_argument_type i36;
reference_wrapper<int(*)(float, char)>::result_type                        i37;
reference_wrapper<int(* const)(float, char)>::result_type                  i38;
reference_wrapper<int(* volatile)(float, char)>::result_type               i39;
reference_wrapper<int(* const volatile)(float, char)>::result_type         i40;
