// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }
// { dg-require-effective-target hosted }

// PR 91456
// std::function and std::is_invocable_r do not understand guaranteed elision

#include <functional>

struct Immovable {
  Immovable() = default;
  Immovable(const Immovable&) = delete;
  Immovable& operator=(const Immovable&) = delete;
};

Immovable get() { return {}; }
const Immovable i = get();                      // OK
std::function<const Immovable()> f{&get};       // fails
const Immovable i2 = f();

const Immovable cget() { return {}; }
Immovable ci = cget();                          // OK
std::function<Immovable()> cf{&cget};           // fails
Immovable ci2 = cf();
