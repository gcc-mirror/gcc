// { dg-options "-std=gnu++11" }
// { dg-do compile }

// Copyright (C) 2014 Free Software Foundation, Inc.
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

#include <tuple>

// A type that is contextually convertible to bool but cannot be used with
// the usual logical operators, and/or/not.
struct TwistedLogic {
  bool value;

  explicit operator bool() const noexcept { return value; }
};

template<typename T>
bool operator&&(const T&, TwistedLogic) = delete;

template<typename T>
bool operator&&(TwistedLogic, const T&) = delete;

template<typename T>
bool operator||(const T&, TwistedLogic) = delete;

template<typename T>
bool operator||(TwistedLogic, const T&) = delete;

bool operator!(TwistedLogic) noexcept = delete;

struct Compares {};

TwistedLogic operator==(const Compares&, const Compares&) { return {true}; }
TwistedLogic operator<(const Compares&, const Compares&) { return {false}; }

auto a = std::make_tuple(nullptr, Compares{}, 2, 'U');
auto b = a == a;
auto c = a < a;
