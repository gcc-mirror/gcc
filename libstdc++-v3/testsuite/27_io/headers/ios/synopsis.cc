// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

#include <ios>

namespace std {
#if 0
  typedef OFF_T streamoff;
  typedef SZ_T streamsize;
#endif

  template <class stateT> class fpos;
  class ios_base;
  template <class charT, class traits>
     class basic_ios;

  // 27.4.5, manipulators:
  ios_base& boolalpha (ios_base& str);
  ios_base& noboolalpha(ios_base& str);
  ios_base& showbase     (ios_base& str);
  ios_base& noshowbase (ios_base& str);
  ios_base& showpoint (ios_base& str);
  ios_base& noshowpoint(ios_base& str);
  ios_base& showpos      (ios_base& str);
  ios_base& noshowpos    (ios_base& str);
  ios_base& skipws       (ios_base& str);
  ios_base& noskipws     (ios_base& str);
  ios_base& uppercase (ios_base& str);
  ios_base& nouppercase(ios_base& str);
  ios_base& unitbuf      (ios_base& str);
  ios_base& nounitbuf    (ios_base& str);
  // 27.4.5.2 adjustfield:
  ios_base& internal     (ios_base& str);
  ios_base& left         (ios_base& str);
  ios_base& right        (ios_base& str);
  // 27.4.5.3 basefield:
  ios_base& dec          (ios_base& str);
  ios_base& hex          (ios_base& str);
  ios_base& oct          (ios_base& str);
  // 27.4.5.4 floatfield:
  ios_base& fixed        (ios_base& str);
  ios_base& scientific (ios_base& str);
}
