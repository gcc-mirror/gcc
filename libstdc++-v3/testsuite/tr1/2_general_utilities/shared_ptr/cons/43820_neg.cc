// { dg-do compile }

// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

// { dg-add-options using-deprecated }
// { dg-warning "auto_ptr. is deprecated" "" { target c++11 } 0 }

// 2.2.3 Class template shared_ptr [tr.util.smartptr.shared]

#include <tr1/memory>

// incomplete type
struct X;

std::auto_ptr<X>& ap();

void test01()
{
  X* px = 0;
  std::tr1::shared_ptr<X> p1(px);   // { dg-error "here" }
  // { dg-error "incomplete" "" { target *-*-* } 559 }

  std::tr1::shared_ptr<X> p9(ap());  // { dg-error "here" }
  // { dg-error "incomplete" "" { target *-*-* } 600 }
}

// Ignore additional diagnostic given with -Wsystem-headers:
// { dg-prune-output "has incomplete type" }
// { dg-prune-output "possible problem detected" }
