// { dg-do run { target c++17 }  }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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

struct exception {};

int counter = 0;

struct mixin_counter
{
  mixin_counter() { ++counter; }
  mixin_counter(mixin_counter const&) { ++counter; }
  ~mixin_counter() { --counter; }
};

struct value_type : private mixin_counter
{
  enum state_type
  {
    zero,
    moved_from,
    throwing_construction,
    throwing_copy,
    throwing_copy_assignment,
    throwing_move,
    throwing_move_assignment,
    threw,
  };

  value_type() = default;

  explicit value_type(state_type state_)
  : state(state_)
  {
    throw_if(throwing_construction);
  }

  value_type(value_type const& other)
  : state(other.state)
  {
    throw_if(throwing_copy);
  }

  value_type&
  operator=(value_type const& other)
  {
    state = other.state;
    throw_if(throwing_copy_assignment);
    return *this;
  }

  value_type(value_type&& other)
  : state(other.state)
  {
    other.state = moved_from;
    throw_if(throwing_move);
  }

  value_type&
  operator=(value_type&& other)
  {
    state = other.state;
    other.state = moved_from;
    throw_if(throwing_move_assignment);
    return *this;
  }

  void throw_if(state_type match)
  {
    if(state == match)
    {
      state = threw;
      throw exception {};
    }
  }

  state_type state = zero;
};

int main()
{
  using O = std::optional<value_type>;
  using S = value_type::state_type;
  auto const make = [](S s = S::zero) { return value_type { s }; };

  enum outcome_type { nothrow, caught, bad_catch };

  // Check value assignment for engaged optional

  {
    O o = make();
    value_type v = make(S::throwing_copy);
    o = v;
    VERIFY( o && o->state == S::throwing_copy);
  }

  {
    O o = make();
    value_type v = make(S::throwing_move);
    o = std::move(v);
    VERIFY( o && o->state == S::throwing_move);
  }

  {
    outcome_type outcome {};
    O o = make();
    value_type v = make(S::throwing_copy_assignment);

    try
    {
      o = v;
    }
    catch(exception const&)
    { outcome = caught; }
    catch(...)
    { outcome = bad_catch; }

    VERIFY( o && o->state == S::threw );
  }

  {
    outcome_type outcome {};
    O o = make();
    value_type v = make(S::throwing_move_assignment);

    try
    {
      o = std::move(v);
    }
    catch(exception const&)
    { outcome = caught; }
    catch(...)
    { outcome = bad_catch; }

    VERIFY( o && o->state == S::threw );
  }

  VERIFY( counter == 0 );
}
