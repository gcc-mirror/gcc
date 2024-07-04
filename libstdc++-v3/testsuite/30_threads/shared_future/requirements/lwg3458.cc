// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

std::shared_future<int(&)[1]> good;
std::shared_future<int(&)()> good2;

std::shared_future<int[1]> bad; // { dg-error "here" }
// { dg-error "result type must not be an array" "" { target *-*-* } 0 }

std::shared_future<int()> bad2; // { dg-error "here" }
// { dg-error "result type must not be a function" "" { target *-*-* } 0 }

struct Indestructible { ~Indestructible() = delete; };
std::shared_future<Indestructible> bad3; // { dg-error "here" }
// { dg-error "result type must be destructible" "" { target *-*-* } 0 }
// { dg-prune-output {deleted function} }

class PrivateDtor { public: PrivateDtor(); private: ~PrivateDtor(); };
std::shared_future<PrivateDtor> bad4; // { dg-error "here" }
// { dg-prune-output {is private} }
