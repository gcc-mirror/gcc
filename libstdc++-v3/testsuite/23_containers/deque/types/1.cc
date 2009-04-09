// 2005-12-18  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

// { dg-do compile }

#include <deque>

namespace N
{
  struct X { };

  template<typename T>
    X operator+(T, std::size_t)
    { return X(); }

  template<typename T>
    X operator-(T, T)
    { return X(); }
}

int main()
{
  std::deque<N::X> d(5);
  const std::deque<N::X> e(1);

  d[0];
  e[0];
  d.size();
  d.erase(d.begin());
  d.resize(1);
  d.assign(1, N::X());
  d.insert(d.begin(), N::X());
  d.insert(d.begin(), 1, N::X());
  d.insert(d.begin(), e.begin(), e.end());
  d = e;

  return 0;
}
