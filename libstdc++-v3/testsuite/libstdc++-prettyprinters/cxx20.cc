// { dg-options "-g -O0 -std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

#include <array>
#include <compare>
#include <iostream>
#include <memory>
#include <span>

struct X
{
  int i;

  std::weak_ordering operator<=>(X x) const noexcept { return i <=> x.i; }
};

int
main()
{
  auto c1 = 1 <=> 1;
// { dg-final { note-test c1 "std::strong_ordering::equal" } }
  auto c2 = 1 <=> 2;
// { dg-final { note-test c2 "std::strong_ordering::less" } }
  auto c3 = 4 <=> 3;
// { dg-final { note-test c3 "std::strong_ordering::greater" } }

  auto c4 = X{1} <=> X{1};
// { dg-final { note-test c4 "std::weak_ordering::equivalent" } }
  auto c5 = X{2} <=> X{3};
// { dg-final { note-test c5 "std::weak_ordering::less" } }
  auto c6 = X{2} <=> X{1};
// { dg-final { note-test c6 "std::weak_ordering::greater" } }

  auto c7 = 0.2 <=> 0.2;
// { dg-final { note-test c7 "std::partial_ordering::equivalent" } }
  auto c8 = 2.5 <=> 5.5;
// { dg-final { note-test c8 "std::partial_ordering::less" } }
  auto c9 = 7.5 <=> -2.2;
// { dg-final { note-test c9 "std::partial_ordering::greater" } }
  auto c10 = 0.0 <=> __builtin_nan("");
// { dg-final { note-test c10 "std::partial_ordering::unordered" } }

  auto il = {1, 2};
  auto s1 = std::span(il);
  static_assert(s1.extent == std::size_t(-1));
// { dg-final { note-test s1 {std::span of length 2 = {1, 2}} } }
  auto a = std::array{3, 4};
  auto s2 = std::span(a);
  static_assert(s2.extent == std::size_t(2));
// { dg-final { note-test s2 {std::span of length 2 = {3, 4}} } }

  std::atomic<std::shared_ptr<int>> spe;
// { dg-final { note-test spe {std::atomic<std::shared_ptr<int>> (empty) = {get() = 0x0}} } }
  std::atomic<std::shared_ptr<int>> sp1 = std::make_shared<int>(1);
  std::atomic<std::shared_ptr<int>> sp2 = sp1.load();
  std::atomic<std::weak_ptr<int>> wp{sp2.load()};
// { dg-final { regexp-test sp1 {std::atomic.std::shared_ptr.int.. \(use count 2, weak count 1\) = {get\(\) = 0x.*}} } }
// { dg-final { regexp-test wp {std::atomic.std::weak_ptr.int.. \(use count 2, weak count 1\) = {get\(\) = 0x.*}} } }

  std::cout << "\n";
  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
