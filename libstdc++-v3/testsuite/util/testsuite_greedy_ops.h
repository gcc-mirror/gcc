// Copyright (C) 2010 Free Software Foundation, Inc.
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

namespace greedy_ops
{
  struct X
  { };

  template<typename T>
  X operator==(T, T)
  { return X(); }

  template<typename T>
  X operator!=(T, T)
  { return X(); }

  template<typename T>
  X operator<(T, T)
  { return X(); }

  template<typename T>
  X operator<=(T, T)
  { return X(); }

  template<typename T>
    X operator>(T, T)
  { return X(); }

  template<typename T>
  X operator>=(T, T)
  { return X(); }

  template<typename T>
  X operator-(T, T)
  { return X(); }
  /*
  template<typename T>
  T operator+(std::size_t, T)
  { return T(); }
  */
  template<typename T>
  T operator+(T, std::size_t)
  { return T(); }
}
