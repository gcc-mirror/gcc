// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006, 2007, 2009 Free Software Foundation, Inc.
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

// { dg-do compile }
// { dg-options "-D_GLIBCXX_CONCEPT_CHECKS" }
// { dg-error "not a valid type" "" { target *-*-* } 30 }
// { dg-error "invalid type"     "" { target *-*-* } 30 }

// 5.1.4.1 class template linear_congruential [tr.rand.eng.lcong]
// 5.1.4.1 [4]

#include <tr1/random>
 
std::tr1::linear_congruential<double, 48271, 0, 2147483647> x;

