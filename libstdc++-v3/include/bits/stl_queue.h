// Queue implementation -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996,1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file stl_queue.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef __GLIBCPP_INTERNAL_QUEUE_H
#define __GLIBCPP_INTERNAL_QUEUE_H

#include <bits/concept_check.h>

namespace std
{

// Forward declarations of operators < and ==, needed for friend declaration.

template <class _Tp, 
          class _Sequence = deque<_Tp> >
class queue;

template <class _Tp, class _Seq>
inline bool operator==(const queue<_Tp, _Seq>&, const queue<_Tp, _Seq>&);

template <class _Tp, class _Seq>
inline bool operator<(const queue<_Tp, _Seq>&, const queue<_Tp, _Seq>&);


template <class _Tp, class _Sequence>
class queue
{
  // concept requirements
  __glibcpp_class_requires(_Tp, _SGIAssignableConcept)
  __glibcpp_class_requires(_Sequence, _FrontInsertionSequenceConcept)
  __glibcpp_class_requires(_Sequence, _BackInsertionSequenceConcept)
  typedef typename _Sequence::value_type _Sequence_value_type;
  __glibcpp_class_requires2(_Tp, _Sequence_value_type, _SameTypeConcept);

  template <class _Tp1, class _Seq1>
  friend bool operator== (const queue<_Tp1, _Seq1>&,
                          const queue<_Tp1, _Seq1>&);
  template <class _Tp1, class _Seq1>
  friend bool operator< (const queue<_Tp1, _Seq1>&,
                         const queue<_Tp1, _Seq1>&);
public:
  typedef typename _Sequence::value_type      value_type;
  typedef typename _Sequence::size_type       size_type;
  typedef          _Sequence                  container_type;

  typedef typename _Sequence::reference       reference;
  typedef typename _Sequence::const_reference const_reference;
protected:
  _Sequence c;
public:
  explicit queue(const _Sequence& __c = _Sequence()) : c(__c) {}

  bool empty() const { return c.empty(); }
  size_type size() const { return c.size(); }
  reference front() { return c.front(); }
  const_reference front() const { return c.front(); }
  reference back() { return c.back(); }
  const_reference back() const { return c.back(); }
  void push(const value_type& __x) { c.push_back(__x); }
  void pop() { c.pop_front(); }
};

template <class _Tp, class _Sequence>
bool 
operator==(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return __x.c == __y.c;
}

template <class _Tp, class _Sequence>
bool
operator<(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return __x.c < __y.c;
}

template <class _Tp, class _Sequence>
bool
operator!=(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return !(__x == __y);
}

template <class _Tp, class _Sequence>
bool 
operator>(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return __y < __x;
}

template <class _Tp, class _Sequence>
bool 
operator<=(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return !(__y < __x);
}

template <class _Tp, class _Sequence>
bool 
operator>=(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return !(__x < __y);
}

template <class _Tp, 
          class _Sequence = vector<_Tp>,
          class _Compare  = less<typename _Sequence::value_type> >
class priority_queue
{
  // concept requirements
  __glibcpp_class_requires(_Tp, _SGIAssignableConcept)
  __glibcpp_class_requires(_Sequence, _SequenceConcept)
  __glibcpp_class_requires(_Sequence, _RandomAccessContainerConcept)
  typedef typename _Sequence::value_type _Sequence_value_type;
  __glibcpp_class_requires2(_Tp, _Sequence_value_type, _SameTypeConcept);
  __glibcpp_class_requires4(_Compare, bool, _Tp, _Tp, _BinaryFunctionConcept);

public:
  typedef typename _Sequence::value_type      value_type;
  typedef typename _Sequence::size_type       size_type;
  typedef          _Sequence                  container_type;

  typedef typename _Sequence::reference       reference;
  typedef typename _Sequence::const_reference const_reference;
protected:
  _Sequence c;
  _Compare comp;
public:
  explicit priority_queue(const _Compare& __x = _Compare(),
			  const _Sequence& __s = _Sequence()) 
    : c(__s), comp(__x) 
    { make_heap(c.begin(), c.end(), comp); }

  template <class _InputIterator>
  priority_queue(_InputIterator __first, _InputIterator __last,
                 const _Compare& __x = _Compare(),
		 const _Sequence& __s = _Sequence())
  : c(__s), comp(__x)
  { 
    c.insert(c.end(), __first, __last);
    make_heap(c.begin(), c.end(), comp);
  }

  bool empty() const { return c.empty(); }
  size_type size() const { return c.size(); }
  const_reference top() const { return c.front(); }

  void 
  push(const value_type& __x) 
  {
    try 
      {
	c.push_back(__x); 
	push_heap(c.begin(), c.end(), comp);
      }
    catch(...)
      {
	c.clear();
	__throw_exception_again; 
      }
  }

  void 
  pop() 
  {
    try 
      {
	pop_heap(c.begin(), c.end(), comp);
	c.pop_back();
      }
    catch(...)
      {
	c.clear();
	__throw_exception_again; 
      }
  }
};

// no equality is provided

} // namespace std

#endif /* __GLIBCPP_INTERNAL_QUEUE_H */

// Local Variables:
// mode:C++
// End:
