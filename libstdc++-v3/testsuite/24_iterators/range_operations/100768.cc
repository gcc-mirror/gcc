// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

// PR libstdc++/100768 - Range iterator operations should be function objects

#include <iterator>
#include <ranges>

namespace ns1
{
  struct R { };
  void check_adl(R) { }
}

namespace ns2
{
  using ns1::R;

  struct A { };

  template<typename I>
    R advance(I, ...) { return R{}; }

  template<typename I>
    R distance(I, ...) { return R{}; }

  template<typename I>
    R next(I, ...) { return R{}; }

  template<typename I>
    R prev(I, ...) { return R{}; }
}

template<typename T, typename U> struct associated { };

void
test02()
{
  // This type has both ns2 and std::ranges as associated namespaces.
  using X = associated<ns2::A, std::ranges::dangling>;

  X range[1];
  X* iter = range;
  X* const sentinel = iter + 1;

  // [range.iter.op.general] p2 says: "The function templates defined in
  // [range.iter.ops] are not found by argument-dependent name lookup."
  //
  // If we do not meet that requirement then the following will find those
  // function templates (because std::ranges is an associated namespace),
  // and the calls to check_adl will be ill-formed.
  check_adl( advance(iter, 1) );
  check_adl( advance(iter, 1, sentinel) );
  check_adl( distance(iter, sentinel) );
  check_adl( distance(range) );
  check_adl( next(iter) );
  check_adl( next(iter, 1) );
  check_adl( next(iter, sentinel) );
  check_adl( next(iter, 1, sentinel) );
  check_adl( prev(iter) );
  check_adl( prev(iter, 1) );
  check_adl( prev(iter, 1, sentinel) );
}

namespace ns3
{
  struct A { };

  void advance(A*, int) = delete;
  void advance(A*, int, A*) = delete;

  void distance(A*, A*) = delete;
  void distance(A(&)[1]) = delete;

  void next(A*) = delete;
  void next(A*, int) = delete;
  void next(A*, A*) = delete;
  void next(A*, int, A*) = delete;

  void prev(A*) = delete;
  void prev(A*, int) = delete;
  void prev(A*, int, A*) = delete;
}

void
test01()
{
  ns3::A range[1];
  ns3::A* iter = range;
  ns3::A* const sentinel = iter + 1;

  // [range.iter.op.general] p2 also says: "When found by unqualified name
  // lookup for the postfix-expression in a function call, they inhibit
  // argument-dependent name lookup."
  //
  // If we do not meet that requirement then the following will find the
  // deleted overloads in namespace ns3 (because it is an associated namespace
  // and those functions are exact matches for the arguments).
  using namespace std::ranges;
  (void) advance(iter, 1);
  (void) advance(iter, 3, sentinel);
  (void) distance(iter, sentinel);
  (void) distance(range);
  (void) next(iter);
  (void) next(iter, -1);
  (void) next(iter, sentinel);
  (void) next(iter, 5, sentinel);
  (void) prev(iter);
  (void) prev(iter, 0);
  (void) prev(iter, 0, sentinel);
}
