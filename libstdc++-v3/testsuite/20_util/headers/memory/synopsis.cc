// { dg-do compile }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

#include <memory>

namespace std {
  //  lib.default.allocator, the default allocator:
  template <class T> class allocator;
  template <> class allocator<void>;
  template <class T, class U>
  bool operator==(const allocator<T>&, const allocator<U>&) throw();
  template <class T, class U>
  bool operator!=(const allocator<T>&, const allocator<U>&) throw();

  //  lib.storage.iterator, raw storage iterator:
  template <class OutputIterator, class T> class raw_storage_iterator;

  //  lib.temporary.buffer, temporary buffers:
  template <class T>
  pair<T*,ptrdiff_t> get_temporary_buffer(ptrdiff_t n);
  template <class T>
  void return_temporary_buffer(T* p);

  //  lib.specialized.algorithms, specialized algorithms:
  template <class InputIterator, class ForwardIterator>
  ForwardIterator
  uninitialized_copy(InputIterator first, InputIterator last,
		     ForwardIterator result);
  template <class ForwardIterator, class T>
  void uninitialized_fill(ForwardIterator first, ForwardIterator last,
			  const T& x);
  template <class ForwardIterator, class Size, class T>
  void uninitialized_fill_n(ForwardIterator first, Size n, const T& x);

  //  lib.auto.ptr, pointers:
  template<class X> class auto_ptr;
}
