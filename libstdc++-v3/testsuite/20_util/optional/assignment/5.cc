// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <optional>
#include <vector>
#include <testsuite_hooks.h>

int counter = 0;

struct mixin_counter
{
  mixin_counter() { ++counter; }
  mixin_counter(mixin_counter const&) { ++counter; }
  ~mixin_counter() { --counter; }
};

struct value_type : private mixin_counter { };

int main()
{
  using O = std::optional<value_type>;

  // Check std::nullopt_t and 'default' (= {}) assignment

  {
    O o;
    o = std::nullopt;
    VERIFY( !o );
  }

  {
    O o { std::in_place };
    o = std::nullopt;
    VERIFY( !o );
  }

  {
    O o;
    o = {};
    VERIFY( !o );
  }

  {
    O o { std::in_place };
    o = {};
    VERIFY( !o );
  }
  {
    std::optional<std::vector<int>> ovi{{1, 2, 3}};
    VERIFY(ovi->size() == 3);
    VERIFY((*ovi)[0] == 1 && (*ovi)[1] == 2 && (*ovi)[2] == 3);
    ovi = {4, 5, 6, 7};
    VERIFY(ovi->size() == 4);
    VERIFY((*ovi)[0] == 4 && (*ovi)[1] == 5 &&
	   (*ovi)[2] == 6 && (*ovi)[3] == 7);
  }
  VERIFY( counter == 0 );
}
