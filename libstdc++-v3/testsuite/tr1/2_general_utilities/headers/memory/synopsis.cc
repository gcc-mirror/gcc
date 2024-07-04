// { dg-do compile }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

#include <tr1/memory>

namespace std {
namespace tr1 {

  // [2.2.2] Class bad_weak_ptr
  class bad_weak_ptr;

  // [2.2.3] Class template shared_ptr
  template<class T> class shared_ptr;

  // [2.2.3.6] shared_ptr comparisons
  template<class T, class U> bool operator==(shared_ptr<T> const& a, shared_ptr<U> const& b);
  template<class T, class U> bool operator!=(shared_ptr<T> const& a, shared_ptr<U> const& b);
  template<class T, class U> bool operator<(shared_ptr<T> const& a, shared_ptr<U> const& b);

  // [2.2.3.8] shared_ptr specialized algorithms
  template<class T> void swap(shared_ptr<T>& a, shared_ptr<T>& b);

  // [2.2.3.9] shared_ptr casts
  template<class T, class U> shared_ptr<T> static_pointer_cast(shared_ptr<U> const& r);
  template<class T, class U> shared_ptr<T> dynamic_pointer_cast(shared_ptr<U> const& r);
  template<class T, class U> shared_ptr<T> const_pointer_cast(shared_ptr<U> const& r);

  // [2.2.3.7] shared_ptr I/O
  template<class E, class T, class Y>
     basic_ostream<E, T>& operator<< (basic_ostream<E, T>& os, shared_ptr<Y> const& p);

  // [2.2.3.10] shared_ptr get_deleter
  template<class D, class T> D* get_deleter(shared_ptr<T> const& p);

  // [2.2.4] Class template weak_ptr
  template<class T> class weak_ptr;

  // [2.2.4.6] weak_ptr comparison
  template<class T, class U> bool operator<(weak_ptr<T> const& a, weak_ptr<U> const& b);

  // [2.2.4.7] weak_ptr specialized algorithms
  template<class T> void swap(weak_ptr<T>& a, weak_ptr<T>& b);

  // [2.2.5] Class enable_shared_from_this
  template<class T> class enable_shared_from_this;
} // namespace tr1
} // namespace std
