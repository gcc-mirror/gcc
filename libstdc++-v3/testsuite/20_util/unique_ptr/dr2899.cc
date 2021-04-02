// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <memory>

struct Del {
  Del() = default;
  Del(Del&&) = delete;

  void operator()(int*) const;
};

static_assert(!std::is_move_constructible<std::unique_ptr<int, Del>>::value);
static_assert(std::is_move_constructible<std::unique_ptr<int, Del&>>::value);

struct Del2 {
  Del2() = default;
  Del2(Del2&&) = default;
  Del2& operator=(Del2&&) = delete;
  Del2& operator=(const Del2&) = default;

  void operator()(int*) const;
};

static_assert(!std::is_move_assignable<std::unique_ptr<int, Del2>>::value);
static_assert(std::is_move_assignable<std::unique_ptr<int, Del2&>>::value);

struct Del3 {
  Del3() = default;
  Del3(Del3&&) = default;
  Del3& operator=(Del3&&) = default;
  Del3& operator=(const Del3&) = delete;

  void operator()(int*) const;
};

static_assert(std::is_move_assignable<std::unique_ptr<int, Del3>>::value);
static_assert(!std::is_move_assignable<std::unique_ptr<int, Del3&>>::value);
