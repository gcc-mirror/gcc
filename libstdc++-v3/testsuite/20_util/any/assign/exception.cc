// { dg-do run { target c++17 } }

// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

#include <any>
#include <testsuite_hooks.h>

using std::any;

bool should_throw = false;
struct Bad
{
  Bad() = default;
  Bad(const Bad&) {if (should_throw) throw 666;}
};

struct Bad2
{
  Bad2() = default;
  Bad2(const Bad2&) {if (should_throw) throw 666;}
  Bad2(Bad2&&) noexcept {}
};

int del_count = 0;
struct Good
{
  Good() = default;
  Good(const Good&) = default;
  Good(Good&&) = default;
  ~Good() {++del_count;}
};

int main()
{
    std::any a1 = Good();
    del_count = 0;
    try {
        Bad b;
        std::any a2 = b;
        should_throw = true;
        a1 = a2;
    } catch (...) {
        auto x = std::any_cast<Good>(a1);
        VERIFY( del_count == 0 );
        VERIFY( a1.has_value() );
        std::any_cast<Good>(a1);
    }
    std::any a3 = Good();
    del_count = 0;
    try {
        Bad2 b;
        std::any a4 = b;
        should_throw = true;
        a3 = a4;
    } catch (...) {
        auto x = std::any_cast<Good>(a1);
        VERIFY( del_count == 0 );
        VERIFY( a1.has_value() );
        std::any_cast<Good>(a1);
    }
}
