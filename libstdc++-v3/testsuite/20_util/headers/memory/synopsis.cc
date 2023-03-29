// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

#if __cplusplus >= 201103L
# define NOTHROW noexcept
#else
# define NOTHROW
#endif

namespace std
{
#if __cplusplus >= 201103L
  template<class Ptr> struct pointer_traits;
  template<class T> struct pointer_traits<T*>;

  void* align(size_t alignment, size_t size, void*& ptr, size_t& space) noexcept;

  struct allocator_arg_t;
  extern const allocator_arg_t allocator_arg;

  template<class T, class Alloc> struct uses_allocator;

  template<class Alloc> struct allocator_traits;
#endif // C++11

#if __STDC_HOSTED__
  //  lib.default.allocator, the default allocator:
  template <class T> class allocator;
#if __cplusplus >= 202002L
  template <class T, class U>
  constexpr bool operator==(const allocator<T>&, const allocator<U>&) throw();
#else
  template <> class allocator<void>;
  template <class T, class U>
  bool operator==(const allocator<T>&, const allocator<U>&) throw();
  template <class T, class U>
  bool operator!=(const allocator<T>&, const allocator<U>&) throw();
#endif

  //  lib.storage.iterator, raw storage iterator:
  template <class OutputIterator, class T> class raw_storage_iterator;

  //  lib.temporary.buffer, temporary buffers:
  template <class T>
  pair<T*,ptrdiff_t> get_temporary_buffer(ptrdiff_t n) NOTHROW;
  template <class T>
  void return_temporary_buffer(T* p);
#endif // HOSTED

  //  lib.specialized.algorithms, specialized algorithms:
#if __cplusplus >= 201703L
  template <class T> constexpr T* addressof(T&) noexcept;
#elif __cplusplus >= 201402L
  template <class T> T* addressof(T&) noexcept;
#endif
  template <class InputIterator, class ForwardIterator>
  ForwardIterator
  uninitialized_copy(InputIterator first, InputIterator last,
		     ForwardIterator result);
#if __cplusplus >= 201103L
  template <class InputIterator, class Size, class ForwardIterator>
  ForwardIterator
  uninitialized_copy_n(InputIterator first, Size n, ForwardIterator result);
#endif
  template <class ForwardIterator, class T>
  void uninitialized_fill(ForwardIterator first, ForwardIterator last,
			  const T& x);
  template <class ForwardIterator, class Size, class T>
  void uninitialized_fill_n(ForwardIterator first, Size n, const T& x);

#if __cplusplus >= 201103L
  template<class T> class default_delete;
  template<class T> class default_delete<T[]>;
  template<class T, class D> class unique_ptr;
  template<class T, class D> class unique_ptr<T[], D>;
  template<class T, class D>
    void swap(unique_ptr<T, D>&, unique_ptr<T, D>&) noexcept;
#if __cplusplus >= 201402L
  template<class T, class... Args> unique_ptr<T> make_unique(Args&&...);
#endif

  class bad_weak_ptr;
  template<class T> class shared_ptr;
  template<class T, class... Args> shared_ptr<T> make_shared(Args&&... args);
  template<class T, class A, class... Args>
  shared_ptr<T> allocate_shared(const A& a, Args&&... args);
  template<class T> void swap(shared_ptr<T>&, shared_ptr<T>&) noexcept;
  template<class T> class weak_ptr;
  template<class T> void swap(weak_ptr<T>&, weak_ptr<T>&) noexcept;
  template<class T> class owner_less;
  template<class T> class enable_shared_from_this;

  template<class T, class D> struct hash<unique_ptr<T, D>>;
  template<class T> struct hash<shared_ptr<T>>;
#endif

  //  lib.auto.ptr, pointers:
  template<class X> class auto_ptr;
}
