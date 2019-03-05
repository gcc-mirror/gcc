// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <tuple>

// PR libstdc++/78939

std::tuple_size<const int> ic;		  // { dg-error "incomplete" }
std::tuple_size<volatile int> iv;	  // { dg-error "incomplete" }
std::tuple_size<const volatile int> icv;  // { dg-error "incomplete" }

struct A { };
std::tuple_size<const A> ac;		  // { dg-error "incomplete" }
std::tuple_size<volatile A> av;		  // { dg-error "incomplete" }
std::tuple_size<const volatile A> acv;	  // { dg-error "incomplete" }

std::tuple_size<const void> vc;		  // { dg-error "incomplete" }
std::tuple_size<volatile void> vv;	  // { dg-error "incomplete" }
std::tuple_size<const volatile void> vcv; // { dg-error "incomplete" }
