// 2005-12-18  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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
// { dg-options "-Wno-unused-result" }

#include <deque>
#include <testsuite_greedy_ops.h>

int main()
{
  std::deque<greedy_ops::X> d(5);
  const std::deque<greedy_ops::X> e(1);

  d[0];
  e[0];
  d.size();
  d.erase(d.begin());
  d.resize(1);
  d.assign(1, greedy_ops::X());
  d.insert(d.begin(), greedy_ops::X());
  d.insert(d.begin(), 1, greedy_ops::X());
  d.insert(d.begin(), e.begin(), e.end());
  d = e;

  std::deque<greedy_ops::X>::iterator it;
  it == it;
  it != it;
  it < it;
  it <= it;
  it > it;
  it >= it;
  it - it;
  it + 1;

  return 0;
}
