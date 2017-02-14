/* Copyright (C) 2011-2017 Free Software Foundation, Inc.
   Contributed by Torvald Riegel <triegel@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef LIBITM_CONTAINERS_H
#define LIBITM_CONTAINERS_H 1

#include "common.h"

namespace GTM HIDDEN {

// A simple vector-like container.
// If alloc_seperate_cl is true, allocations will happen on separate cache
// lines.
template <typename T, bool alloc_separate_cl = true>
class vector
{
 private:
  size_t m_capacity;
  size_t m_size;
  T* entries;

  // Initial capacity of the vector.
  static const size_t default_initial_capacity = 32;
  // Above that capacity, grow vector by that size for each call.
  static const size_t default_resize_max = 2048;
  // Resize vector to at least this capacity.
  static const size_t default_resize_min = 32;

  // Don't try to copy this vector.
  vector<T, alloc_separate_cl>(const vector<T, alloc_separate_cl>& x);

 public:
  typedef T datatype;
  typedef T* iterator;

  iterator begin() const { return entries; }
  iterator end() const { return entries + m_size; }
  T& operator[] (size_t pos) { return entries[pos]; }
  const T& operator[] (size_t pos) const  { return entries[pos]; }

  vector<T, alloc_separate_cl>(size_t initial_size = default_initial_capacity)
    : m_capacity(initial_size),
      m_size(0)
  {
    if (m_capacity > 0)
      entries = (T*) xmalloc(sizeof(T) * m_capacity, alloc_separate_cl);
    else
      entries = 0;
  }
  ~vector<T, alloc_separate_cl>() { if (m_capacity) free(entries); }

  void resize(size_t additional_capacity)
  {
    size_t target = m_capacity + additional_capacity;
    if (target > default_resize_max)
      m_capacity = ((target - 1 + default_resize_max) / default_resize_max)
        * default_resize_max;
    else
      while (m_capacity < target)
        m_capacity = m_capacity * 2;
    if (m_capacity < default_resize_min)
      m_capacity = default_resize_min;
    entries = (T*) xrealloc(entries, sizeof(T) * m_capacity, alloc_separate_cl);
  }
  void resize_noinline() __attribute__((noinline)) { resize(1); }
  void resize_noinline(size_t elements) __attribute__((noinline))
  {
    resize(elements);
  }

  size_t size() const { return m_size; }
  size_t capacity() const { return this->capacity; }

  void set_size (size_t size) { m_size = size; }
  void clear() { m_size = 0; }

  iterator push() {
    // We don't want inlining here since push() is often on the fast path.
    if (unlikely(m_size == m_capacity)) resize_noinline();
    return &entries[m_size++];
  }

  iterator push(size_t elements)
  {
    // We don't want inlining here since push() is often on the fast path.
    if (unlikely(m_size + elements > m_capacity)) resize_noinline(elements);
    iterator it = &entries[m_size];
    m_size += elements;
    return it;
  }

  iterator pop() {
    if (likely(m_size > 0))
      {
	m_size--;
	return entries + m_size;
      }
    else return 0;
  }
};

} // namespace GTM

#endif // LIBITM_CONTAINERS_H
