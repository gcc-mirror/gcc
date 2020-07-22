// Copyright (C) 2020 Free Software Foundation, Inc.
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

// LWG 3466
// Specify the requirements for promise/future/shared_future consistently

#include <future>

std::promise<int(&)[1]> good;
std::promise<int(&)()> good2;

std::promise<int[1]> bad; // { dg-error "here" }
// { dg-error "result type is not an array" "" { target *-*-* } 0 }
// { dg-prune-output {request for member '~int \[1\]'} }

std::promise<int()> bad2; // { dg-error "here" }
// { dg-error "result type is not a function" "" { target *-*-* } 0 }
// { dg-prune-output {'sizeof \(int\(\)\)'} }
