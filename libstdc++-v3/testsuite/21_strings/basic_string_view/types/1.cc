//
// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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
//

// { dg-options "-std=gnu++17" }
// { dg-do compile }

#include <string_view>

struct T
{
  typedef std::string_view String_view;
  typedef String_view::iterator iterator;
  typedef String_view::const_iterator const_iterator;

  char t(iterator f)             { return *f; }
  char t(const_iterator f) const { return *f; }
};

void
f()
{
  std::string_view s;
  T t;
  T::const_iterator i = s.begin();
  
  t.t(i);
}
