// { dg-do compile }
// { dg-require-c-std "" }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

#include <tr1/functional>

namespace std {
namespace tr1 {

  // [6.3.3] Hash function base template
  template <class T> struct hash;

  // Hash function specializations
  template <> struct hash<bool>;
  template <> struct hash<char>;
  template <> struct hash<signed char>;
  template <> struct hash<unsigned char>;
  template <> struct hash<wchar_t>;
  template <> struct hash<short>;
  template <> struct hash<unsigned short>;
  template <> struct hash<int>;
  template <> struct hash<unsigned int>;
  template <> struct hash<long>;
  template <> struct hash<unsigned long>;
  template <> struct hash<float>;
  template <> struct hash<double>;
  template <> struct hash<long double>;
  template<class T> struct hash<T*>;
  template <> struct hash<std::string>;
  template <> struct hash<std::wstring>;

} // namespace tr1
} // namespace std
