// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <utility>
#include <tuple>

struct NoCon
{
  NoCon() = delete;
  NoCon(const NoCon&) = delete;
};

struct RefCheck1
{
  RefCheck1(NoCon&, NoCon&&) { }
  RefCheck1() = delete;
  RefCheck1(const RefCheck1&) = delete;
};

struct RefCheck2
{
  RefCheck2(const NoCon&, const NoCon&&, NoCon&) { }
  RefCheck2() = delete;
  RefCheck2(const RefCheck2&) = delete;
};

struct Default
{
  Default();
  Default(const Default&) = delete;
};

// libstdc++/51183
void test01(std::tuple<NoCon&, NoCon&&> t1,
            std::tuple<NoCon&, NoCon&&, NoCon&> t2)
{
  std::pair<RefCheck1, RefCheck2>(std::piecewise_construct,
				  std::move(t1), std::move(t2));
}

void test02(std::tuple<> t1, std::tuple<int> t2)
{
  std::pair<Default, int> A(std::piecewise_construct, t1, t2);
}
