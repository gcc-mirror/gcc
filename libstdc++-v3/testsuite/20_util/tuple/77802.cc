// { dg-do compile { target c++11 } }

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// This testcase instantiates a tuple with an incomplete type.
// That is undefined behavior, but our tuple implementation manages
// to cope with this particular case. The attempt to provide
// a tuple implementation that yields the right result for
// traits like is_copy_constructible and is_move_constructible as
// per LWG 2729 results in ill-formed copy/move constructors being
// generated for a tuple that contains an incomplete type.
// Once we get concepts, we can solve that problem much easier.

template <typename... Args> struct execution_context
{
  typedef std::tuple<Args...> args_tpl_t;
  typedef std::tuple<execution_context, typename std::decay<Args>::type...>
  ret_tpl_t;
  execution_context();
  execution_context(execution_context &&);
  ret_tpl_t operator()() {
    args_tpl_t data;
    return tuple_cat(std::forward_as_tuple(execution_context()), data);
  }
};

void fn1()
{
  execution_context<int> cc;
  cc();
}

