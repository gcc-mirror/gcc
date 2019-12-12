// { dg-do compile }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

#include <tr1/tuple>

namespace std {
namespace tr1 {

#if 0
  // [6.1.3] Class template tuple
  template <class T1 = unspecified ,
	    class T2 = unspecified ,
	    ...,
	    class TM = unspecified > class tuple;

  // [6.1.3.2] Tuple creation functions
  const unspecified ignore;
  template<class T1, class T2, ..., class TN>
  tuple<V1, V2, ..., VN> make_tuple(const T1&, const T2& , ..., const TN&);

  template<class T1, class T2, ..., class TN>
  tuple<T1&, T2&, ..., TN&> tie(T1&, T2& , ..., TN&);
#endif

  // [6.1.3.3] Tuple helper classes
  template <class T> class tuple_size;
  template <int I, class T> class tuple_element;

#if 0
  // [6.1.3.4] Element access
  template <int I, class T1, class T2, ..., class TN>
  RJ get(tuple<T1, T2, ..., TN>&);
  template <int I, class T1, class T2, ..., class TN>
  PJ get(const tuple<T1, T2, ..., TN>&);

  // [6.1.3.5] relational operators
  template<class T1, class T2, ..., class TM, class U1, class U2, ..., class UM>
  bool operator==(const tuple<T1, T2, ..., TM>&, const tuple<U1, U2, ..., UM>&);
  template<class T1, class T2, ..., class TM, class U1, class U2, ..., class UM>
  bool operator<(const tuple<T1, T2, ..., TM>&, const tuple<U1, U2, ..., UM>&);
  template<class T1, class T2, ..., class TM, class U1, class U2, ..., class UM>
  bool operator!=(const tuple<T1, T2, ..., TM>&, const tuple<U1, U2, ..., UM>&);
  template<class T1, class T2, ..., class TM, class U1, class U2, ..., class UM>
  bool operator>(const tuple<T1, T2, ..., TM>&, const tuple<U1, U2, ..., UM>&);
  template<class T1, class T2, ..., class TM, class U1, class U2, ..., class UM>
  bool operator<=(const tuple<T1, T2, ..., TM>&, const tuple<U1, U2, ..., UM>&);
  template<class T1, class T2, ..., class TM, class U1, class U2, ..., class UM>
  bool operator>=(const tuple<T1, T2, ..., TM>&, const tuple<U1, U2, ..., UM>&);
#endif

} // namespace tr1
} // namespace std
