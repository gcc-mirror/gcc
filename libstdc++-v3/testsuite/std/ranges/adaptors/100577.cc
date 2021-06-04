// Copyright (C) 2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// PR libstdc++/100577

#include <ranges>

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  // Verify all multi-argument adaptors except for views::split are denoted
  // to have simple extra arguments.
  using views::__adaptor::__adaptor_has_simple_extra_args;
  static_assert(__adaptor_has_simple_extra_args<decltype(views::transform)>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::filter)>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::drop)>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::take)>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::take_while)>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::drop_while)>);
  static_assert(!__adaptor_has_simple_extra_args<decltype(views::split)>);

  // Verify all adaptor closures except for views::split(pattern) have a simple
  // operator().
  using views::__adaptor::__closure_has_simple_call_op;
  __closure_has_simple_call_op auto a00 = views::all;
  __closure_has_simple_call_op auto a01 = views::transform(std::identity{});
  __closure_has_simple_call_op auto a02 = views::filter(std::identity{});
  __closure_has_simple_call_op auto a03 = views::drop(42);
  __closure_has_simple_call_op auto a04 = views::take(42);
  __closure_has_simple_call_op auto a05 = views::take_while(std::identity{});
  __closure_has_simple_call_op auto a06 = views::drop_while(std::identity{});
  __closure_has_simple_call_op auto a07 = views::join;
  __closure_has_simple_call_op auto a08 = views::common;
  __closure_has_simple_call_op auto a09 = views::reverse;
  __closure_has_simple_call_op auto a10 = views::keys;
  // Verify composition of simple closures is simple.
  __closure_has_simple_call_op auto b
    = (a00 | a01) | (a02 | a03) | (a04 | a05 | a06) | (a07 | a08 | a09 | a10);

  // Verify views::split is the exception.
  auto a11 = views::split(' ');
  static_assert(!__closure_has_simple_call_op<decltype(a11)>);
  static_assert(!__closure_has_simple_call_op<decltype(a11 | a00)>);
  static_assert(!__closure_has_simple_call_op<decltype(a00 | a11)>);
}

void
test02()
{
  // Range adaptor closures with a simple operator() aren't implemented using a
  // fallback deleted overload, so when a call is ill-formed overload resolution
  // fails.
  extern int x[10];
  auto badarg = nullptr;
  views::transform(badarg)(x); // { dg-error "no match" }
  views::filter(badarg)(x); // { dg-error "no match" }
  views::take(badarg)(x); // { dg-error "no match" }
  views::drop(badarg)(x); // { dg-error "no match" }
  views::take_while(badarg)(x); // { dg-error "no match" }
  views::drop_while(badarg)(x); // { dg-error "no match" }

  (views::transform(badarg) | views::all)(x); // { dg-error "no match" }
  (views::filter(badarg) | views::all)(x); // { dg-error "no match" }
  (views::take(badarg) | views::all)(x); // { dg-error "no match" }
  (views::drop(badarg) | views::all)(x); // { dg-error "no match" }
  (views::take_while(badarg) | views::all)(x); // { dg-error "no match" }
  (views::drop_while(badarg) | views::all)(x); // { dg-error "no match" }

  // In practice, range adaptor closures with non-simple operator() are
  // implemented using a fallback deleted overload, so when a call is
  // ill-formed overload resolution succeeds but selects the deleted overload
  // (but only when the closure is invoked as an rvalue).
  views::split(badarg)(x); // { dg-error "deleted function" }
  (views::split(badarg) | views::all)(x); // { dg-error "deleted function" }
  auto a0 = views::split(badarg);
  a0(x); // { dg-error "no match" };
  auto a1 = a0 | views::all;
  a1(x); // { dg-error "no match" }
}

// { dg-prune-output "in requirements" }
