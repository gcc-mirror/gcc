// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// LWG 3458
// Is shared_future intended to work with arrays or function types?

#include <future>

std::future<int(&)[1]> good;
std::future<int(&)()> good2;

std::future<int[1]> bad; // { dg-error "here" }
// { dg-error "result type must not be an array" "" { target *-*-* } 0 }
// { dg-prune-output "function returning an array" }

std::future<int()> bad2; // { dg-error "here" }
// { dg-error "result type must not be a function" "" { target *-*-* } 0 }
// { dg-prune-output "function returning a function" }

struct Indestructible { ~Indestructible() = delete; };
std::future<Indestructible> bad3; // { dg-error "here" }
// { dg-error "result type must be destructible" "" { target *-*-* } 0 }
// { dg-prune-output {deleted function} }

class PrivateDtor { public: PrivateDtor(); private: ~PrivateDtor(); };
std::future<PrivateDtor> bad4; // { dg-error "here" }
// { dg-prune-output {is private} }
