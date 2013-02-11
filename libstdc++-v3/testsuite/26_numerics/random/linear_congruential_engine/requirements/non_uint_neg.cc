//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2013 Free Software Foundation, Inc.
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
// { dg-options "-std=c++0x" }
// { dg-require-cstdint "" }
// { dg-error "not a valid type" "" { target *-*-* } 32 }
// { dg-error "invalid type"     "" { target *-*-* } 32 }

// 26.4.3.1 class template linear_congruential_engine [rand.eng.lcong]
// 26.4.2.2 Concept RandomNumberEngine [rand.concept.eng]

#include <random>

std::linear_congruential_engine<double, 48271, 0, 2147483647> x;

