// { dg-do compile { target c++14 } }

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

// 20.9.5-20.9.9

#include <functional>

static_assert(std::plus<int>()(1,2)==3, "");
static_assert(std::minus<int>()(3,2)==1, "");
static_assert(std::multiplies<int>()(3,2)==6, "");
static_assert(std::divides<int>()(6,2)==3, "");
static_assert(std::modulus<int>()(7,2)==1, "");
static_assert(std::negate<int>()(-5)==5, "");

static_assert(std::plus<void>()(1,2)==3, "");
static_assert(std::minus<void>()(3,2)==1, "");
static_assert(std::multiplies<void>()(3,2)==6, "");
static_assert(std::divides<void>()(6,2)==3, "");
static_assert(std::modulus<void>()(7,2)==1, "");
static_assert(std::negate<void>()(-5)==5, "");

static_assert(std::equal_to<int>()(2,2), "");
static_assert(std::not_equal_to<int>()(1,2), "");
static_assert(std::greater<int>()(2,1), "");
static_assert(std::less<int>()(1,2), "");
static_assert(std::greater_equal<int>()(2,2), "");
static_assert(std::less_equal<int>()(2,2), "");

static_assert(std::equal_to<void>()(2,2), "");
static_assert(std::not_equal_to<void>()(1,2), "");
static_assert(std::greater<void>()(2,1), "");
static_assert(std::less<void>()(1,2), "");
static_assert(std::greater_equal<void>()(2,2), "");
static_assert(std::less_equal<void>()(2,2), "");

static_assert(std::logical_and<int>()(1,1), "");
static_assert(std::logical_or<int>()(0,1), "");
static_assert(std::logical_not<int>()(0), "");

static_assert(std::logical_and<void>()(1,1), "");
static_assert(std::logical_or<void>()(0,1), "");
static_assert(std::logical_not<void>()(0), "");

static_assert(std::bit_and<int>()(3,2)==2, "");
static_assert(std::bit_or<int>()(1,2)==3, "");
static_assert(std::bit_xor<int>()(1,1)==0, "");
static_assert(std::bit_not<int>()(std::bit_not<int>()(0))==0, "");

static_assert(std::bit_and<void>()(3,2)==2, "");
static_assert(std::bit_or<void>()(1,2)==3, "");
static_assert(std::bit_xor<void>()(1,1)==0, "");
static_assert(std::bit_not<void>()(std::bit_not<void>()(0))==0, "");

static_assert(std::unary_negate< // { dg-warning "is deprecated" "" { target c++17 } }
	      std::logical_not<int>
	      >(std::logical_not<int>())(1), "");
static_assert(std::not1(std::logical_not<int>())(1), ""); // { dg-warning "is deprecated" "" { target c++17 } }

static_assert(std::binary_negate< // { dg-warning "is deprecated" "" { target c++17 } }
	      std::logical_and<int>
	      >(std::logical_and<int>())(0,0), "");
static_assert(std::not2(std::logical_and<int>())(0,0), ""); // { dg-warning "is deprecated" "" { target c++17 } }
