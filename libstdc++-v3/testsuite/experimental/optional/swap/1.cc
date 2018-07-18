// { dg-do run { target c++14 } }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <experimental/optional>
#include <testsuite_hooks.h>

struct exception {};

int counter = 0;

struct mixin_counter
{
  mixin_counter() { ++counter; }
  mixin_counter(mixin_counter const&) { ++counter; }
  ~mixin_counter() { --counter; }
};

namespace ns
{

struct value_type : private mixin_counter
{
  explicit value_type(int state) : state(state) { }
  int state;
};

int swaps = 0;

void
swap(value_type& lhs, value_type& rhs)
{
  ++swaps;
  using std::swap;
  swap(lhs.state, rhs.state);
}

} // namespace ns

int main()
{
  using O = std::experimental::optional<ns::value_type>;

  VERIFY( ns::swaps == 0 );

  {
    O o, p;
    swap(o, p);
    VERIFY( !o );
    VERIFY( !p );
  }

  {
    O o { std::experimental::in_place, 45 }, p;
    swap(o, p);
    VERIFY( !o );
    VERIFY( p && p->state == 45 );
  }

  {
    O o, p { std::experimental::in_place, 45 };
    swap(o, p);
    VERIFY( o && o->state == 45 );
    VERIFY( !p );
  }

  {
    O o { std::experimental::in_place, 167 }, p { std::experimental::in_place, 999 };
    VERIFY( ns::swaps == 0 );

    swap(o, p);

    VERIFY( o && o->state == 999 );
    VERIFY( p && p->state == 167 );
    VERIFY( ns::swaps == 1 );
  }

  VERIFY( counter == 0 );
}
