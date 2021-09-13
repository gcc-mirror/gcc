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
#include <functional>

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  // Verify adaptors are deemed to have simple extra arguments when appropriate.
  using views::__adaptor::__adaptor_has_simple_extra_args;
  using std::identity;
  static_assert(__adaptor_has_simple_extra_args<decltype(views::transform), identity>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::filter), identity>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::drop), int>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::take), int>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::take_while), identity>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::drop_while), identity>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::lazy_split), std::string_view>);
  static_assert(__adaptor_has_simple_extra_args<decltype(views::lazy_split), char>);
  static_assert(!__adaptor_has_simple_extra_args<decltype(views::lazy_split), std::string>);

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
  __closure_has_simple_call_op auto a11 = views::lazy_split(' ');
  __closure_has_simple_call_op auto a11a = views::split(' ');
  // Verify composition of simple closures is simple.
  __closure_has_simple_call_op auto b
    = (a00 | a01) | (a02 | a03) | (a04 | a05 | a06) | (a07 | a08 | a09 | a10) | a11;

  // Verify views::lazy_split(non_view_range) is an exception.
  extern std::string s;
  auto a12 = views::lazy_split(s);
  static_assert(!__closure_has_simple_call_op<decltype(a12)>);
  static_assert(!__closure_has_simple_call_op<decltype(a12 | a00)>);
  static_assert(!__closure_has_simple_call_op<decltype(a00 | a12)>);

  // Likewise views::split(non_view_range).
  auto a12a = views::split(s);
  static_assert(!__closure_has_simple_call_op<decltype(a12a)>);
  static_assert(!__closure_has_simple_call_op<decltype(a12a | a00)>);
  static_assert(!__closure_has_simple_call_op<decltype(a00 | a12a)>);
}

void
test02()
{
  // Range adaptor closures with a simple operator() aren't implemented using a
  // fallback deleted overload, so when a call is ill-formed overload resolution
  // fails.
  extern int x[10];
  struct { } badarg;
  views::transform(badarg)(x); // { dg-error "no match" }
  views::filter(badarg)(x); // { dg-error "no match" }
  views::take_while(badarg)(x); // { dg-error "no match" }
  views::drop_while(badarg)(x); // { dg-error "no match" }

  (views::transform(badarg) | views::all)(x); // { dg-error "no match" }
  (views::filter(badarg) | views::all)(x); // { dg-error "no match" }
  (views::take_while(badarg) | views::all)(x); // { dg-error "no match" }
  (views::drop_while(badarg) | views::all)(x); // { dg-error "no match" }

  // In practice, range adaptor closures with non-simple operator() are
  // implemented using a fallback deleted overload, so when a call is
  // ill-formed overload resolution succeeds but selects the deleted overload
  // (but only when the closure is invoked as an rvalue).
  views::lazy_split(badarg)(x); // { dg-error "deleted function" }
  (views::lazy_split(badarg) | views::all)(x); // { dg-error "deleted function" }
  auto a0 = views::lazy_split(badarg);
  a0(x); // { dg-error "no match" };
  auto a1 = a0 | views::all;
  a1(x); // { dg-error "no match" }

  views::split(badarg)(x); // { dg-error "deleted function" }
  (views::split(badarg) | views::all)(x); // { dg-error "deleted function" }
  auto a0a = views::split(badarg);
  a0a(x); // { dg-error "no match" };
  auto a1a = a0a | views::all;
  a1a(x); // { dg-error "no match" }

  views::take(badarg)(x); // { dg-error "deleted" }
  views::drop(badarg)(x); // { dg-error "deleted" }
  (views::take(badarg) | views::all)(x); // { dg-error "deleted" }
  (views::drop(badarg) | views::all)(x); // { dg-error "deleted" }
}

void
test03()
{
  // PR libstdc++/100940
  extern int x[10];
  struct S { operator int() && { return 5; }; };
  x | std::views::take(S{});
  x | std::views::drop(S{});
}

void
test04()
{
  // Non-trivially-copyable extra arguments make a closure not simple.
  using F = std::function<bool(bool)>;
  static_assert(!std::is_trivially_copyable_v<F>);
  using views::__adaptor::__closure_has_simple_call_op;
  static_assert(!__closure_has_simple_call_op<decltype(views::take_while(std::declval<F>()))>);
  static_assert(!__closure_has_simple_call_op<decltype(views::drop_while(std::declval<F>()))>);
  static_assert(!__closure_has_simple_call_op<decltype(views::filter(std::declval<F>()))>);
  static_assert(!__closure_has_simple_call_op<decltype(views::transform(std::declval<F>()))>);
}

// { dg-prune-output "in requirements" }
