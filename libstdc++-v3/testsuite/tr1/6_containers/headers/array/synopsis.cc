// { dg-do compile }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2013 Free Software Foundation, Inc.
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
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <tr1/array>

namespace std {
namespace tr1 {

  // [6.2.2] Class template array
  template <class T, size_t N > struct array;

  // Array comparisons
  template <class T, size_t N>  bool  operator==(const array<T,N>& x, const array<T,N>& y);
  template <class T,  size_t N>  bool  operator!=(const array<T,N>& x, const array<T,N>& y);
  template <class T,  size_t N>  bool  operator<(const array<T,N>& x, const array<T,N>& y);
  template <class T,  size_t N>  bool  operator>(const array<T,N>& x, const array<T,N>& y);
  template <class T,  size_t N>  bool  operator<=(const array<T,N>& x, const array<T,N>& y);
  template <class T,  size_t N>  bool  operator>=(const array<T,N>& x, const array<T,N>& y);

  // [6.2.2.2] Specialized algorithms
  template <class T, size_t N > void swap(array<T,N>& x, array<T,N>& y);

  // [6.2.2.5] Tuple interface to class template array
  template <class T> class tuple_size; // forward declaration
  template <int I, class T> class tuple_element; // forward declaration
  template <class T, size_t N>  struct tuple_size<array<T, N> >;
  template <int I, class T, size_t N> struct tuple_element<I, array<T, N> >;
  template <int I, class T, size_t N> T& get(array<T, N>&);
  template <int I, class T, size_t N> const T& get(const array<T, N>&);

} // namespace tr1
} // namespace std

