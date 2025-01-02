// { dg-do compile }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

#include <tr1/functional>

namespace std {
namespace tr1 {

  // [3.4] class template result_of
  template <class FunctionCallType> class result_of;

#if 0
  // [3.5] function template mem_fn
  template<class R, class T> unspecified mem_fn(R T::* pm);
#endif

  // [3.6] function object binders
  template<class T> struct is_bind_expression;
  template<class T> struct is_placeholder;

#if 0
  template<class F, class T1, ..., class TN>
  unspecified bind(F f, T1 t1, ..., TN tN);

  template<class R, class F, class T1, ..., class Tn >
  unspecified bind(F f, T1 t1, ..., TN tN);

  namespace placeholders {
     // M is the implementation-defined number of placeholders
     extern unspecified _1;
     extern unspecified _2;
                     .
                     .
                     .
     extern unspecified _M;
  }
#endif

  // [3.7] polymorphic function wrappers
  class bad_function_call;
  template<class Function> class function;
  template<class Function>
     void swap(function<Function>&, function<Function>&);
  template<class Function1, class Function2>
     void operator==(const function<Function1>&, const function<Function2>&);
  template<class Function1, class Function2>
     void operator!=(const function<Function1>&, const function<Function2>&);

#if 0
  template <class Function>
     bool operator==(const function<Function>&, unspecified-null-pointer-type);
  template <class Function>
     bool operator==(unspecified-null-pointer-type, const function<Function>&);
  template <class Function>
     bool operator!=(const function<Function>&, unspecified-null-pointer-type);
  template <class Function>
     bool operator!=(unspecified-null-pointer-type, const function<Function>&);
#endif

} // namespace tr1
} // namespace std
