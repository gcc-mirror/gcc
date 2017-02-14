// 2005-12-01  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

#include <string>

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
  std::basic_string<N::X> s(5, N::X());

  s.erase(s.begin());
  s.erase(s.begin(), s.end());
  s.insert(s.begin(), N::X());
  s.replace(s.begin(), s.end(), s.begin(), s.end());

  return 0;
}
