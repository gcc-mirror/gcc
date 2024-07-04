// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <stop_token>

struct F
{
  F();

  void operator()() const { }

private:
  ~F();
};

auto
test01(std::stop_token& tok, F& f)
{
  auto ok = sizeof(std::stop_callback<F&>);
  auto bad = sizeof(std::stop_callback<F>); // { dg-error "here" }
  return ok + bad;
}

struct G
{
  G();
  ~G() noexcept(false);

  void operator()() const { }
};

auto
test02(std::stop_token& tok, G& g)
{
  auto ok = sizeof(std::stop_callback<G&>);
  auto bad = sizeof(std::stop_callback<G>); // { dg-error "here" }
  return ok + bad;
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
