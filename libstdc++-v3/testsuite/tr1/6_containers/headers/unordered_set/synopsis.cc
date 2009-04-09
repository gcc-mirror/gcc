// { dg-do compile }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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

#include <tr1/unordered_set>

namespace std {
namespace tr1 {

  // [6.3.4.3] Class template unordered_set
  template <class Value,
	    class Hash,
	    class Pred,
	    class Alloc>
     class unordered_set;

  // [6.3.4.5] Class template unordered_multiset
  template <class Value,
	    class Hash,
	    class Pred,
	    class Alloc>
     class unordered_multiset;

  template <class Value, class Hash, class Pred, class Alloc>
     void swap(unordered_set<Value, Hash, Pred, Alloc>& x,
	       unordered_set<Value, Hash, Pred, Alloc>& y);

  template <class Value, class Hash, class Pred, class Alloc>
     void swap(unordered_multiset<Value, Hash, Pred, Alloc>& x,
	       unordered_multiset<Value, Hash, Pred, Alloc>& y);

} // namespace tr1
} // namespace std

