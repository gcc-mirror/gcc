// { dg-do run { target c++17 }  }

// Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

#include <optional>
#include <testsuite_hooks.h>

int counter = 0;

struct mixin_counter
{
  mixin_counter() { ++counter; }
  mixin_counter(mixin_counter const&) { ++counter; }
  ~mixin_counter() { --counter; }
};

struct value_type : private mixin_counter
{
  value_type() = default;
  value_type(int) : state(1) { }
  value_type(std::initializer_list<char>, const char*) : state(2) { }
  int state = 0;
};

int main()
{
  using O = std::optional<value_type>;

  // Check emplace

  {
    O o;
    o.emplace();
    VERIFY( o && o->state == 0 );
  }
  {
    O o { std::in_place, 0 };
    o.emplace();
    VERIFY( o && o->state == 0 );
  }

  {
    O o;
    o.emplace(0);
    VERIFY( o && o->state == 1 );
  }
  {
    O o { std::in_place };
    o.emplace(0);
    VERIFY( o && o->state == 1 );
  }

  {
    O o;
    o.emplace({ 'a' }, "");
    VERIFY( o && o->state == 2 );
  }
  {
    O o { std::in_place };
    o.emplace({ 'a' }, "");
    VERIFY( o && o->state == 2 );
  }
  {
    O o;
    VERIFY(&o.emplace(0) == &*o);
    VERIFY(&o.emplace({ 'a' }, "") == &*o);
  }

  static_assert( !std::is_constructible<O, std::initializer_list<int>, int>(), "" );

  VERIFY( counter == 0 );
}
