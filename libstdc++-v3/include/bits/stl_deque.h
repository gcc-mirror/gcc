// deque implementation -*- C++ -*-

// Copyright (C) 2001, 2002 Free Software Foundation, Inc.
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
 * Copyright (c) 1997
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

/** @file stl_deque.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#include <bits/concept_check.h>
#include <bits/stl_iterator_base_types.h>
#include <bits/stl_iterator_base_funcs.h>

#ifndef __GLIBCPP_INTERNAL_DEQUE_H
#define __GLIBCPP_INTERNAL_DEQUE_H


// Since this entire file is within namespace std, there's no reason to
// waste two spaces along the left column.  Thus the leading indentation is
// slightly violated from here on.
namespace std
{ 

/**
 *  @if maint
 *  @brief This function controls the size of memory nodes.
 *  @param  size  The size of an element.
 *  @return   The number (not bytesize) of elements per node.
 *
 *  This function started off as a compiler kludge from SGI, but seems to
 *  be a useful wrapper around a repeated constant expression.
 *  @endif
*/
inline size_t 
__deque_buf_size(size_t __size) 
{ return __size < 512 ? size_t(512 / __size) : size_t(1); }


/// A deque::iterator.
/**
 *  Quite a bit of intelligence here.  Much of the functionality of deque is
 *  actually passed off to this class.  A deque holds two of these internally,
 *  marking its valid range.  Access to elements is done as offsets of either
 *  of those two, relying on operator overloading in this class.
 *
 *  @if maint
 *  All the functions are op overloads except for _M_set_node.
 *  @endif
*/
template <class _Tp, class _Ref, class _Ptr>
struct _Deque_iterator
{
  typedef _Deque_iterator<_Tp, _Tp&, _Tp*>             iterator;
  typedef _Deque_iterator<_Tp, const _Tp&, const _Tp*> const_iterator;
  static size_t _S_buffer_size() { return __deque_buf_size(sizeof(_Tp)); }

  typedef random_access_iterator_tag iterator_category;
  typedef _Tp                        value_type;
  typedef _Ptr                       pointer;
  typedef _Ref                       reference;
  typedef size_t                     size_type;
  typedef ptrdiff_t                  difference_type;
  typedef _Tp**                      _Map_pointer;
  typedef _Deque_iterator            _Self;

  _Tp* _M_cur;
  _Tp* _M_first;
  _Tp* _M_last;
  _Map_pointer _M_node;

  _Deque_iterator(_Tp* __x, _Map_pointer __y) 
    : _M_cur(__x), _M_first(*__y),
      _M_last(*__y + _S_buffer_size()), _M_node(__y) {}
  _Deque_iterator() : _M_cur(0), _M_first(0), _M_last(0), _M_node(0) {}
  _Deque_iterator(const iterator& __x)
    : _M_cur(__x._M_cur), _M_first(__x._M_first), 
      _M_last(__x._M_last), _M_node(__x._M_node) {}

  reference operator*() const { return *_M_cur; }
  pointer operator->() const { return _M_cur; }

  _Self& operator++() {
    ++_M_cur;
    if (_M_cur == _M_last) {
      _M_set_node(_M_node + 1);
      _M_cur = _M_first;
    }
    return *this; 
  }
  _Self operator++(int)  {
    _Self __tmp = *this;
    ++*this;
    return __tmp;
  }

  _Self& operator--() {
    if (_M_cur == _M_first) {
      _M_set_node(_M_node - 1);
      _M_cur = _M_last;
    }
    --_M_cur;
    return *this;
  }
  _Self operator--(int) {
    _Self __tmp = *this;
    --*this;
    return __tmp;
  }

  _Self& operator+=(difference_type __n)
  {
    difference_type __offset = __n + (_M_cur - _M_first);
    if (__offset >= 0 && __offset < difference_type(_S_buffer_size()))
      _M_cur += __n;
    else {
      difference_type __node_offset =
        __offset > 0 ? __offset / difference_type(_S_buffer_size())
                   : -difference_type((-__offset - 1) / _S_buffer_size()) - 1;
      _M_set_node(_M_node + __node_offset);
      _M_cur = _M_first + 
        (__offset - __node_offset * difference_type(_S_buffer_size()));
    }
    return *this;
  }

  _Self operator+(difference_type __n) const
  {
    _Self __tmp = *this;
    return __tmp += __n;
  }

  _Self& operator-=(difference_type __n) { return *this += -__n; }
 
  _Self operator-(difference_type __n) const {
    _Self __tmp = *this;
    return __tmp -= __n;
  }

  reference operator[](difference_type __n) const { return *(*this + __n); }

  /** @if maint
   *  Prepares to traverse new_node.  Sets everything except _M_cur, which
   *  should therefore be set by the caller immediately afterwards, based on
   *  _M_first and _M_last.
   *  @endif
  */
  void _M_set_node(_Map_pointer __new_node) {
    _M_node = __new_node;
    _M_first = *__new_node;
    _M_last = _M_first + difference_type(_S_buffer_size());
  }
};

// Note: we also provide overloads whose operands are of the same type in
// order to avoid ambiguos overload resolution when std::rel_ops operators
// are in scope (for additional details, see libstdc++/3628)
template <class _Tp, class _Ref, class _Ptr>
inline bool
operator==(const _Deque_iterator<_Tp, _Ref, _Ptr>& __x,
	   const _Deque_iterator<_Tp, _Ref, _Ptr>& __y)
{
  return __x._M_cur == __y._M_cur;
}

template <class _Tp, class _RefL, class _PtrL, class _RefR, class _PtrR>
inline bool
operator==(const _Deque_iterator<_Tp, _RefL, _PtrL>& __x,
	   const _Deque_iterator<_Tp, _RefR, _PtrR>& __y)
{
  return __x._M_cur == __y._M_cur;
}

template <class _Tp, class _Ref, class _Ptr>
inline bool
operator!=(const _Deque_iterator<_Tp, _Ref, _Ptr>& __x,
	   const _Deque_iterator<_Tp, _Ref, _Ptr>& __y)
{
  return !(__x == __y);
}

template <class _Tp, class _RefL, class _PtrL, class _RefR, class _PtrR>
inline bool
operator!=(const _Deque_iterator<_Tp, _RefL, _PtrL>& __x,
	   const _Deque_iterator<_Tp, _RefR, _PtrR>& __y)
{
  return !(__x == __y);
}

template <class _Tp, class _Ref, class _Ptr>
inline bool
operator<(const _Deque_iterator<_Tp, _Ref, _Ptr>& __x,
	   const _Deque_iterator<_Tp, _Ref, _Ptr>& __y)
{
  return (__x._M_node == __y._M_node) ? 
    (__x._M_cur < __y._M_cur) : (__x._M_node < __y._M_node);
}

template <class _Tp, class _RefL, class _PtrL, class _RefR, class _PtrR>
inline bool
operator<(const _Deque_iterator<_Tp, _RefL, _PtrL>& __x,
	   const _Deque_iterator<_Tp, _RefR, _PtrR>& __y)
{
  return (__x._M_node == __y._M_node) ? 
    (__x._M_cur < __y._M_cur) : (__x._M_node < __y._M_node);
}

template <class _Tp, class _Ref, class _Ptr>
inline bool
operator>(const _Deque_iterator<_Tp, _Ref, _Ptr>& __x,
	   const _Deque_iterator<_Tp, _Ref, _Ptr>& __y)
{
  return __y < __x;
}

template <class _Tp, class _RefL, class _PtrL, class _RefR, class _PtrR>
inline bool
operator>(const _Deque_iterator<_Tp, _RefL, _PtrL>& __x,
	   const _Deque_iterator<_Tp, _RefR, _PtrR>& __y)
{
  return __y < __x;
}

template <class _Tp, class _Ref, class _Ptr>
inline bool
operator<=(const _Deque_iterator<_Tp, _Ref, _Ptr>& __x,
	   const _Deque_iterator<_Tp, _Ref, _Ptr>& __y)
{
  return !(__y < __x);
}

template <class _Tp, class _RefL, class _PtrL, class _RefR, class _PtrR>
inline bool
operator<=(const _Deque_iterator<_Tp, _RefL, _PtrL>& __x,
	   const _Deque_iterator<_Tp, _RefR, _PtrR>& __y)
{
  return !(__y < __x);
}

template <class _Tp, class _Ref, class _Ptr>
inline bool
operator>=(const _Deque_iterator<_Tp, _Ref, _Ptr>& __x,
	   const _Deque_iterator<_Tp, _Ref, _Ptr>& __y)
{
  return !(__x < __y);
}

template <class _Tp, class _RefL, class _PtrL, class _RefR, class _PtrR>
inline bool
operator>=(const _Deque_iterator<_Tp, _RefL, _PtrL>& __x,
	   const _Deque_iterator<_Tp, _RefR, _PtrR>& __y)
{
  return !(__x < __y);
}

// _GLIBCPP_RESOLVE_LIB_DEFECTS
// According to the resolution of DR179 not only the various comparison
// operators but also operator- must accept mixed iterator/const_iterator
// parameters.
template <typename _Tp, typename _RefL, typename _PtrL,
                        typename _RefR, typename _PtrR>
inline typename _Deque_iterator<_Tp, _RefL, _PtrL>::difference_type
operator-(const _Deque_iterator<_Tp, _RefL, _PtrL>& __x,
	  const _Deque_iterator<_Tp, _RefR, _PtrR>& __y)
{
  return _Deque_iterator<_Tp, _RefL, _PtrL>::difference_type
    (_Deque_iterator<_Tp, _RefL, _PtrL>::_S_buffer_size()) *
    (__x._M_node - __y._M_node - 1) + (__x._M_cur - __x._M_first) +
    (__y._M_last - __y._M_cur);
}

template <class _Tp, class _Ref, class _Ptr>
inline _Deque_iterator<_Tp, _Ref, _Ptr>
operator+(ptrdiff_t __n, const _Deque_iterator<_Tp, _Ref, _Ptr>& __x)
{
  return __x + __n;
}


/// @if maint Primary default version.  @endif
/**
 *  @if maint
 *  Deque base class.  It has two purposes.  First, its constructor
 *  and destructor allocate (but don't initialize) storage.  This makes
 *  exception safety easier.  Second, the base class encapsulates all of
 *  the differences between SGI-style allocators and standard-conforming
 *  allocators.  There are two versions:  this ordinary one, and the
 *  space-saving specialization for instanceless allocators.
 *  @endif
*/
template <class _Tp, class _Alloc, bool __is_static>
class _Deque_alloc_base
{
public:
  typedef typename _Alloc_traits<_Tp,_Alloc>::allocator_type allocator_type;
  allocator_type get_allocator() const { return _M_node_allocator; }

  _Deque_alloc_base(const allocator_type& __a)
    : _M_node_allocator(__a), _M_map_allocator(__a),
      _M_map(0), _M_map_size(0)
  {}
  
protected:
  typedef typename _Alloc_traits<_Tp*, _Alloc>::allocator_type
          _Map_allocator_type;

  allocator_type      _M_node_allocator;
  _Map_allocator_type _M_map_allocator;

  _Tp* _M_allocate_node() {
    return _M_node_allocator.allocate(__deque_buf_size(sizeof(_Tp)));
  }
  void _M_deallocate_node(_Tp* __p) {
    _M_node_allocator.deallocate(__p, __deque_buf_size(sizeof(_Tp)));
  }
  _Tp** _M_allocate_map(size_t __n) 
    { return _M_map_allocator.allocate(__n); }
  void _M_deallocate_map(_Tp** __p, size_t __n) 
    { _M_map_allocator.deallocate(__p, __n); }

  _Tp** _M_map;
  size_t _M_map_size;
};

/// @if maint Specialization for instanceless allocators.  @endif
template <class _Tp, class _Alloc>
class _Deque_alloc_base<_Tp, _Alloc, true>
{
public:
  typedef typename _Alloc_traits<_Tp,_Alloc>::allocator_type allocator_type;
  allocator_type get_allocator() const { return allocator_type(); }

  _Deque_alloc_base(const allocator_type&) : _M_map(0), _M_map_size(0) {}
  
protected:
  typedef typename _Alloc_traits<_Tp, _Alloc>::_Alloc_type _Node_alloc_type;
  typedef typename _Alloc_traits<_Tp*, _Alloc>::_Alloc_type _Map_alloc_type;

  _Tp* _M_allocate_node() {
    return _Node_alloc_type::allocate(__deque_buf_size(sizeof(_Tp)));
  }
  void _M_deallocate_node(_Tp* __p) {
    _Node_alloc_type::deallocate(__p, __deque_buf_size(sizeof(_Tp)));
  }
  _Tp** _M_allocate_map(size_t __n) 
    { return _Map_alloc_type::allocate(__n); }
  void _M_deallocate_map(_Tp** __p, size_t __n) 
    { _Map_alloc_type::deallocate(__p, __n); }

  _Tp** _M_map;
  size_t _M_map_size;
};


/**
 *  @if maint
 *  Deque base class.  Using _Alloc_traits in the instantiation of the parent
 *  class provides the compile-time dispatching mentioned in the parent's docs.
 *  This class provides the unified face for deque's allocation.
 *
 *  Nothing in this class ever constructs or destroys an actual Tp element.
 *  (Deque handles that itself.)  Only/All memory management is performed here.
 *  @endif
*/
template <class _Tp, class _Alloc>
class _Deque_base
  : public _Deque_alloc_base<_Tp,_Alloc,
                              _Alloc_traits<_Tp, _Alloc>::_S_instanceless>
{
public:
  typedef _Deque_alloc_base<_Tp,_Alloc,
                             _Alloc_traits<_Tp, _Alloc>::_S_instanceless>
          _Base;
  typedef typename _Base::allocator_type             allocator_type;
  typedef _Deque_iterator<_Tp,_Tp&,_Tp*>             iterator;
  typedef _Deque_iterator<_Tp,const _Tp&,const _Tp*> const_iterator;

  _Deque_base(const allocator_type& __a, size_t __num_elements)
    : _Base(__a), _M_start(), _M_finish()
    { _M_initialize_map(__num_elements); }
  _Deque_base(const allocator_type& __a) 
    : _Base(__a), _M_start(), _M_finish() {}
  ~_Deque_base();    

protected:
  void _M_initialize_map(size_t);
  void _M_create_nodes(_Tp** __nstart, _Tp** __nfinish);
  void _M_destroy_nodes(_Tp** __nstart, _Tp** __nfinish);
  enum { _S_initial_map_size = 8 };

protected:
  iterator _M_start;
  iterator _M_finish;
};


template <class _Tp, class _Alloc>
_Deque_base<_Tp,_Alloc>::~_Deque_base()
{
  if (_M_map) {
    _M_destroy_nodes(_M_start._M_node, _M_finish._M_node + 1);
    _M_deallocate_map(_M_map, _M_map_size);
  }
}

/**
 *  @if maint
 *  @brief Layout storage.
 *  @param  num_elements  The count of T's for which to allocate space at first.
 *  @return   Nothing.
 *
 *  The initial underlying memory layout is a bit complicated...
 *  @endif
*/
template <class _Tp, class _Alloc>
void
_Deque_base<_Tp,_Alloc>::_M_initialize_map(size_t __num_elements)
{
  size_t __num_nodes = 
    __num_elements / __deque_buf_size(sizeof(_Tp)) + 1;

  _M_map_size = max((size_t) _S_initial_map_size, __num_nodes + 2);
  _M_map = _M_allocate_map(_M_map_size);

  _Tp** __nstart = _M_map + (_M_map_size - __num_nodes) / 2;
  _Tp** __nfinish = __nstart + __num_nodes;
    
  try 
    { _M_create_nodes(__nstart, __nfinish); }
  catch(...)
    {
      _M_deallocate_map(_M_map, _M_map_size);
      _M_map = 0;
      _M_map_size = 0;
      __throw_exception_again;
    }
  
  _M_start._M_set_node(__nstart);
  _M_finish._M_set_node(__nfinish - 1);
  _M_start._M_cur = _M_start._M_first;
  _M_finish._M_cur = _M_finish._M_first +
               __num_elements % __deque_buf_size(sizeof(_Tp));
}

template <class _Tp, class _Alloc>
void _Deque_base<_Tp,_Alloc>::_M_create_nodes(_Tp** __nstart, _Tp** __nfinish)
{
  _Tp** __cur;
  try {
    for (__cur = __nstart; __cur < __nfinish; ++__cur)
      *__cur = _M_allocate_node();
  }
  catch(...)
    { 
      _M_destroy_nodes(__nstart, __cur);
      __throw_exception_again; 
    }
}

template <class _Tp, class _Alloc>
void
_Deque_base<_Tp,_Alloc>::_M_destroy_nodes(_Tp** __nstart, _Tp** __nfinish)
{
  for (_Tp** __n = __nstart; __n < __nfinish; ++__n)
    _M_deallocate_node(*__n);
}


/**
 *  @ingroup Containers
 *  @ingroup Sequences
 *
 *  Meets the requirements of a <a href="tables.html#65">container</a>, a
 *  <a href="tables.html#66">reversible container</a>, and a
 *  <a href="tables.html#67">sequence</a>, including the
 *  <a href="tables.html#68">optional sequence requirements</a>.
 *
 *  Placeholder:  see http://www.sgi.com/tech/stl/Deque.html for now.
 *
 *  In previous HP/SGI versions of deque, there was an extra template parameter
 *  so users could control the node size.  This extension turned out to violate
 *  the C++ standard (it can be detected using template template parameters),
 *  and it was removed.
 *
 *  @if maint
 *  Here's how a deque<Tp> manages memory.  Each deque has 4 members:
 *  
 *  - Tp**        _M_map
 *  - size_t      _M_map_size
 *  - iterator    _M_start, _M_finish
 *  
 *  map_size is at least 8.  %map is an array of map_size pointers-to-"nodes".
 *  (The name has nothing to do with the std::map class.)
 *  
 *  A "node" has no specific type name as such, but it is referred to as
 *  "node" in this file.  It is a simple array-of-Tp.  If Tp is very large,
 *  there will be one Tp element per node (i.e., an "array" of one).
 *  For non-huge Tp's, node size is inversely related to Tp size:  the
 *  larger the Tp, the fewer Tp's will fit in a node.  The goal here is to
 *  keep the total size of a node relatively small and constant over different
 *  Tp's, to improve allocator efficiency.
 *  
 *  **** As I write this, the nodes are /not/ allocated using the high-speed
 *  memory pool.  There are 20 hours left in the year; perhaps I can fix
 *  this before 2002.
 *  
 *  Not every pointer in the %map array will point to a node.  If the initial
 *  number of elements in the deque is small, the /middle/ %map pointers will
 *  be valid, and the ones at the edges will be unused.  This same situation
 *  will arise as the %map grows:  available %map pointers, if any, will be on
 *  the ends.  As new nodes are created, only a subset of the %map's pointers
 *  need to be copied "outward".
 *
 *  Class invariants:
 * - For any nonsingular iterator i:
 *    - i.node points to a member of the %map array.  (Yes, you read that
 *      correctly:  i.node does not actually point to a node.)  The member of
 *      the %map array is what actually points to the node.
 *    - i.first == *(i.node)    (This points to the node (first Tp element).)
 *    - i.last  == i.first + node_size
 *    - i.cur is a pointer in the range [i.first, i.last).  NOTE:
 *      the implication of this is that i.cur is always a dereferenceable
 *      pointer, even if i is a past-the-end iterator.
 * - Start and Finish are always nonsingular iterators.  NOTE: this means that
 *   an empty deque must have one node, a deque with <N elements (where N is
 *   the node buffer size) must have one node, a deque with N through (2N-1)
 *   elements must have two nodes, etc.
 * - For every node other than start.node and finish.node, every element in the
 *   node is an initialized object.  If start.node == finish.node, then
 *   [start.cur, finish.cur) are initialized objects, and the elements outside
 *   that range are uninitialized storage.  Otherwise, [start.cur, start.last)
 *   and [finish.first, finish.cur) are initialized objects, and [start.first,
 *   start.cur) and [finish.cur, finish.last) are uninitialized storage.
 * - [%map, %map + map_size) is a valid, non-empty range.  
 * - [start.node, finish.node] is a valid range contained within 
 *   [%map, %map + map_size).  
 * - A pointer in the range [%map, %map + map_size) points to an allocated node
 *   if and only if the pointer is in the range [start.node, finish.node].
 *
 *  Here's the magic:  nothing in deque is "aware" of the discontiguous storage!
 *
 *  The memory setup and layout occurs in the parent, _Base, and the iterator
 *  class is entirely responsible for "leaping" from one node to the next.  All
 *  the implementation routines for deque itself work only through the start
 *  and finish iterators.  This keeps the routines simple and sane, and we can
 *  use other standard algorithms as well.
 *  @endif
*/
template <class _Tp, class _Alloc = allocator<_Tp> >
class deque : protected _Deque_base<_Tp, _Alloc>
{
  // concept requirements
  __glibcpp_class_requires(_Tp, _SGIAssignableConcept)

  typedef _Deque_base<_Tp, _Alloc> _Base;

public:
  typedef _Tp                                value_type;
  typedef value_type*                        pointer;
  typedef const value_type*                  const_pointer;
  typedef value_type&                        reference;
  typedef const value_type&                  const_reference;
  typedef size_t                             size_type;
  typedef ptrdiff_t                          difference_type;

  typedef typename _Base::allocator_type allocator_type;
  allocator_type get_allocator() const { return _Base::get_allocator(); }

  typedef typename _Base::iterator           iterator;
  typedef typename _Base::const_iterator     const_iterator;
  typedef reverse_iterator<const_iterator>   const_reverse_iterator;
  typedef reverse_iterator<iterator>         reverse_iterator;

protected:
  typedef pointer* _Map_pointer;
  static size_t _S_buffer_size() { return __deque_buf_size(sizeof(_Tp)); }

  // Functions controlling memory layout, and nothing else.
  using _Base::_M_initialize_map;
  using _Base::_M_create_nodes;
  using _Base::_M_destroy_nodes;
  using _Base::_M_allocate_node;
  using _Base::_M_deallocate_node;
  using _Base::_M_allocate_map;
  using _Base::_M_deallocate_map;

  /** @if maint
   *  A total of four data members accumulated down the heirarchy.  If the
   *  _Alloc type requires separate instances, then two of them will also be
   *  included in each deque.
   *  @endif
  */
  using _Base::_M_map;
  using _Base::_M_map_size;
  using _Base::_M_start;
  using _Base::_M_finish;

public:                         // Basic accessors
  iterator begin() { return _M_start; }
  iterator end() { return _M_finish; }
  const_iterator begin() const { return _M_start; }
  const_iterator end() const { return _M_finish; }

  reverse_iterator rbegin() { return reverse_iterator(_M_finish); }
  reverse_iterator rend() { return reverse_iterator(_M_start); }
  const_reverse_iterator rbegin() const 
    { return const_reverse_iterator(_M_finish); }
  const_reverse_iterator rend() const 
    { return const_reverse_iterator(_M_start); }

  reference operator[](size_type __n)
    { return _M_start[difference_type(__n)]; }
  const_reference operator[](size_type __n) const 
    { return _M_start[difference_type(__n)]; }

  void _M_range_check(size_type __n) const {
    if (__n >= this->size())
      __throw_range_error("deque");
  }

  reference at(size_type __n)
    { _M_range_check(__n); return (*this)[__n]; }
  const_reference at(size_type __n) const
    { _M_range_check(__n); return (*this)[__n]; }

  reference front() { return *_M_start; }
  reference back() {
    iterator __tmp = _M_finish;
    --__tmp;
    return *__tmp;
  }
  const_reference front() const { return *_M_start; }
  const_reference back() const {
    const_iterator __tmp = _M_finish;
    --__tmp;
    return *__tmp;
  }

  size_type size() const { return _M_finish - _M_start; }
  size_type max_size() const { return size_type(-1); }
  bool empty() const { return _M_finish == _M_start; }

public:                         // Constructor, destructor.
  explicit deque(const allocator_type& __a = allocator_type()) 
    : _Base(__a, 0) {}
  deque(const deque& __x) : _Base(__x.get_allocator(), __x.size()) 
    { uninitialized_copy(__x.begin(), __x.end(), _M_start); }
  deque(size_type __n, const value_type& __value,
        const allocator_type& __a = allocator_type()) : _Base(__a, __n)
    { _M_fill_initialize(__value); }

  explicit
  deque(size_type __n)
  : _Base(allocator_type(), __n)
  { _M_fill_initialize(value_type()); }

  // Check whether it's an integral type.  If so, it's not an iterator.
  template<class _InputIterator>
    deque(_InputIterator __first, _InputIterator __last,
          const allocator_type& __a = allocator_type())
    : _Base(__a)
    {
      typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
      _M_initialize_dispatch(__first, __last, _Integral());
    }

  template<class _Integer>
    void
    _M_initialize_dispatch(_Integer __n, _Integer __x, __true_type)
    {
      _M_initialize_map(__n);
      _M_fill_initialize(__x);
    }

  template<class _InputIter>
    void
    _M_initialize_dispatch(_InputIter __first, _InputIter __last, __false_type)
    {
      typedef typename iterator_traits<_InputIter>::iterator_category _IterCategory;
      _M_range_initialize(__first, __last, _IterCategory());
    }

  ~deque()
  { _Destroy(_M_start, _M_finish); }

  deque& operator= (const deque& __x) {
    const size_type __len = size();
    if (&__x != this) {
      if (__len >= __x.size())
        erase(copy(__x.begin(), __x.end(), _M_start), _M_finish);
      else {
        const_iterator __mid = __x.begin() + difference_type(__len);
        copy(__x.begin(), __mid, _M_start);
        insert(_M_finish, __mid, __x.end());
      }
    }
    return *this;
  }        

  void swap(deque& __x) {
    std::swap(_M_start, __x._M_start);
    std::swap(_M_finish, __x._M_finish);
    std::swap(_M_map, __x._M_map);
    std::swap(_M_map_size, __x._M_map_size);
  }

public: 
  // assign(), a generalized assignment member function.  Two
  // versions: one that takes a count, and one that takes a range.
  // The range version is a member template, so we dispatch on whether
  // or not the type is an integer.

  void _M_fill_assign(size_type __n, const _Tp& __val) {
    if (__n > size()) {
      fill(begin(), end(), __val);
      insert(end(), __n - size(), __val);
    }
    else {
      erase(begin() + __n, end());
      fill(begin(), end(), __val);
    }
  }

  void
  assign(size_type __n, const _Tp& __val)
  { _M_fill_assign(__n, __val); }

  template<class _InputIterator>
    void
    assign(_InputIterator __first, _InputIterator __last)
    {
      typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
      _M_assign_dispatch(__first, __last, _Integral());
    }

private:                        // helper functions for assign() 

  template<class _Integer>
    void
    _M_assign_dispatch(_Integer __n, _Integer __val, __true_type)
    { _M_fill_assign(static_cast<size_type>(__n), static_cast<_Tp>(__val)); }

  template<class _InputIterator>
    void
    _M_assign_dispatch(_InputIterator __first, _InputIterator __last, __false_type)
    {
      typedef typename iterator_traits<_InputIterator>::iterator_category _IterCategory;
      _M_assign_aux(__first, __last, _IterCategory());
    }

  template <class _InputIterator>
  void _M_assign_aux(_InputIterator __first, _InputIterator __last,
                     input_iterator_tag);

  template <class _ForwardIterator>
  void _M_assign_aux(_ForwardIterator __first, _ForwardIterator __last,
                     forward_iterator_tag) {
    size_type __len = distance(__first, __last);
    if (__len > size()) {
      _ForwardIterator __mid = __first;
      advance(__mid, size());
      copy(__first, __mid, begin());
      insert(end(), __mid, __last);
    }
    else
      erase(copy(__first, __last, begin()), end());
  }

public:                         // push_* and pop_*
  
  void
  push_back(const value_type& __t)
  {
    if (_M_finish._M_cur != _M_finish._M_last - 1) {
      _Construct(_M_finish._M_cur, __t);
      ++_M_finish._M_cur;
    }
    else
      _M_push_back_aux(__t);
  }

  void
  push_back()
  {
    if (_M_finish._M_cur != _M_finish._M_last - 1) {
      _Construct(_M_finish._M_cur);
      ++_M_finish._M_cur;
    }
    else
      _M_push_back_aux();
  }

  void
  push_front(const value_type& __t) 
  {
    if (_M_start._M_cur != _M_start._M_first) {
      _Construct(_M_start._M_cur - 1, __t);
      --_M_start._M_cur;
    }
    else
      _M_push_front_aux(__t);
  }

  void
  push_front()
  {
    if (_M_start._M_cur != _M_start._M_first) {
      _Construct(_M_start._M_cur - 1);
      --_M_start._M_cur;
    }
    else
      _M_push_front_aux();
  }


  void
  pop_back()
  {
    if (_M_finish._M_cur != _M_finish._M_first) {
      --_M_finish._M_cur;
      _Destroy(_M_finish._M_cur);
    }
    else
      _M_pop_back_aux();
  }

  void
  pop_front()
  {
    if (_M_start._M_cur != _M_start._M_last - 1) {
      _Destroy(_M_start._M_cur);
      ++_M_start._M_cur;
    }
    else 
      _M_pop_front_aux();
  }

public:                         // Insert

  iterator
  insert(iterator position, const value_type& __x)
  {
    if (position._M_cur == _M_start._M_cur) {
      push_front(__x);
      return _M_start;
    }
    else if (position._M_cur == _M_finish._M_cur) {
      push_back(__x);
      iterator __tmp = _M_finish;
      --__tmp;
      return __tmp;
    }
    else {
      return _M_insert_aux(position, __x);
    }
  }

  iterator
  insert(iterator __position)
  { return insert(__position, value_type()); }

  void
  insert(iterator __pos, size_type __n, const value_type& __x)
  { _M_fill_insert(__pos, __n, __x); }

  void
  _M_fill_insert(iterator __pos, size_type __n, const value_type& __x); 

  // Check whether it's an integral type.  If so, it's not an iterator.
  template<class _InputIterator>
    void
    insert(iterator __pos, _InputIterator __first, _InputIterator __last)
    {
      typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
      _M_insert_dispatch(__pos, __first, __last, _Integral());
    }

  template<class _Integer>
    void
    _M_insert_dispatch(iterator __pos, _Integer __n, _Integer __x, __true_type)
    { _M_fill_insert(__pos, static_cast<size_type>(__n), static_cast<value_type>(__x)); }

  template<class _InputIterator>
    void
    _M_insert_dispatch(iterator __pos,
                       _InputIterator __first, _InputIterator __last,
                       __false_type)
    {
      typedef typename iterator_traits<_InputIterator>::iterator_category _IterCategory;
      insert(__pos, __first, __last, _IterCategory());
    }

  void resize(size_type __new_size, const value_type& __x) {
    const size_type __len = size();
    if (__new_size < __len) 
      erase(_M_start + __new_size, _M_finish);
    else
      insert(_M_finish, __new_size - __len, __x);
  }

  void resize(size_type new_size) { resize(new_size, value_type()); }

public:                         // Erase
  iterator erase(iterator __pos) {
    iterator __next = __pos;
    ++__next;
    size_type __index = __pos - _M_start;
    if (__index < (size() >> 1)) {
      copy_backward(_M_start, __pos, __next);
      pop_front();
    }
    else {
      copy(__next, _M_finish, __pos);
      pop_back();
    }
    return _M_start + __index;
  }

  iterator erase(iterator __first, iterator __last);
  void clear(); 

protected:                        // Internal construction/destruction

  void _M_fill_initialize(const value_type& __value);

  template <class _InputIterator>
  void _M_range_initialize(_InputIterator __first, _InputIterator __last,
                        input_iterator_tag);

  template <class _ForwardIterator>
  void _M_range_initialize(_ForwardIterator __first, _ForwardIterator __last,
                        forward_iterator_tag);

protected:                        // Internal push_* and pop_*

  void _M_push_back_aux(const value_type&);
  void _M_push_back_aux();
  void _M_push_front_aux(const value_type&);
  void _M_push_front_aux();
  void _M_pop_back_aux();
  void _M_pop_front_aux();

protected:                        // Internal insert functions

  template <class _InputIterator>
  void insert(iterator __pos, _InputIterator __first, _InputIterator __last,
              input_iterator_tag);

  template <class _ForwardIterator>
  void insert(iterator __pos,
              _ForwardIterator __first, _ForwardIterator __last,
              forward_iterator_tag);

  iterator _M_insert_aux(iterator __pos, const value_type& __x);
  iterator _M_insert_aux(iterator __pos);
  void _M_insert_aux(iterator __pos, size_type __n, const value_type& __x);

  template <class _ForwardIterator>
  void _M_insert_aux(iterator __pos, 
                     _ForwardIterator __first, _ForwardIterator __last,
                     size_type __n);

  iterator _M_reserve_elements_at_front(size_type __n) {
    size_type __vacancies = _M_start._M_cur - _M_start._M_first;
    if (__n > __vacancies) 
      _M_new_elements_at_front(__n - __vacancies);
    return _M_start - difference_type(__n);
  }

  iterator _M_reserve_elements_at_back(size_type __n) {
    size_type __vacancies = (_M_finish._M_last - _M_finish._M_cur) - 1;
    if (__n > __vacancies)
      _M_new_elements_at_back(__n - __vacancies);
    return _M_finish + difference_type(__n);
  }

  void _M_new_elements_at_front(size_type __new_elements);
  void _M_new_elements_at_back(size_type __new_elements);

protected:                      // Allocation of _M_map and nodes

  // Makes sure the _M_map has space for new nodes.  Does not actually
  //  add the nodes.  Can invalidate _M_map pointers.  (And consequently, 
  //  deque iterators.)

  void _M_reserve_map_at_back (size_type __nodes_to_add = 1) {
    if (__nodes_to_add + 1 > _M_map_size - (_M_finish._M_node - _M_map))
      _M_reallocate_map(__nodes_to_add, false);
  }

  void _M_reserve_map_at_front (size_type __nodes_to_add = 1) {
    if (__nodes_to_add > size_type(_M_start._M_node - _M_map))
      _M_reallocate_map(__nodes_to_add, true);
  }

  void _M_reallocate_map(size_type __nodes_to_add, bool __add_at_front);
};

// Non-inline member functions

template <class _Tp, class _Alloc>
template <class _InputIter>
void deque<_Tp, _Alloc>
  ::_M_assign_aux(_InputIter __first, _InputIter __last, input_iterator_tag)
{
  iterator __cur = begin();
  for ( ; __first != __last && __cur != end(); ++__cur, ++__first)
    *__cur = *__first;
  if (__first == __last)
    erase(__cur, end());
  else
    insert(end(), __first, __last);
}

template <class _Tp, class _Alloc>
void deque<_Tp, _Alloc>::_M_fill_insert(iterator __pos,
                                        size_type __n, const value_type& __x)
{
  if (__pos._M_cur == _M_start._M_cur) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    try {
      uninitialized_fill(__new_start, _M_start, __x);
      _M_start = __new_start;
    }
    catch(...)
      {
	_M_destroy_nodes(__new_start._M_node, _M_start._M_node);
	__throw_exception_again;
      }
  }
  else if (__pos._M_cur == _M_finish._M_cur) {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    try {
      uninitialized_fill(_M_finish, __new_finish, __x);
      _M_finish = __new_finish;
    }
    catch(...)
      {
	_M_destroy_nodes(_M_finish._M_node + 1, __new_finish._M_node + 1);    
	__throw_exception_again;
      }
  }
  else 
    _M_insert_aux(__pos, __n, __x);
}

template <class _Tp, class _Alloc>
typename deque<_Tp,_Alloc>::iterator 
deque<_Tp,_Alloc>::erase(iterator __first, iterator __last)
{
  if (__first == _M_start && __last == _M_finish) {
    clear();
    return _M_finish;
  }
  else {
    difference_type __n = __last - __first;
    difference_type __elems_before = __first - _M_start;
    if (static_cast<size_type>(__elems_before) < (size() - __n) / 2) {
      copy_backward(_M_start, __first, __last);
      iterator __new_start = _M_start + __n;
      _Destroy(_M_start, __new_start);
      _M_destroy_nodes(_M_start._M_node, __new_start._M_node);
      _M_start = __new_start;
    }
    else {
      copy(__last, _M_finish, __first);
      iterator __new_finish = _M_finish - __n;
      _Destroy(__new_finish, _M_finish);
      _M_destroy_nodes(__new_finish._M_node + 1, _M_finish._M_node + 1);
      _M_finish = __new_finish;
    }
    return _M_start + __elems_before;
  }
}

template <class _Tp, class _Alloc> 
void deque<_Tp,_Alloc>::clear()
{
  for (_Map_pointer __node = _M_start._M_node + 1;
       __node < _M_finish._M_node;
       ++__node) {
    _Destroy(*__node, *__node + _S_buffer_size());
    _M_deallocate_node(*__node);
  }

  if (_M_start._M_node != _M_finish._M_node) {
    _Destroy(_M_start._M_cur, _M_start._M_last);
    _Destroy(_M_finish._M_first, _M_finish._M_cur);
    _M_deallocate_node(_M_finish._M_first);
  }
  else
    _Destroy(_M_start._M_cur, _M_finish._M_cur);

  _M_finish = _M_start;
}

/**
 *  @if maint
 *  @brief Fills the deque with copies of value.
 *  @param  value  Initial value.
 *  @return   Nothing.
 *  @pre _M_start and _M_finish have already been initialized, but none of the
 *       deque's elements have yet been constructed.
 *
 *  This function is called only when the user provides an explicit size (with
 *  or without an explicit exemplar value).
 *  @endif
*/
template <class _Tp, class _Alloc>
void deque<_Tp,_Alloc>::_M_fill_initialize(const value_type& __value)
{
  _Map_pointer __cur;
  try {
    for (__cur = _M_start._M_node; __cur < _M_finish._M_node; ++__cur)
      uninitialized_fill(*__cur, *__cur + _S_buffer_size(), __value);
    uninitialized_fill(_M_finish._M_first, _M_finish._M_cur, __value);
  }
  catch(...)
    {
      _Destroy(_M_start, iterator(*__cur, __cur));
      __throw_exception_again;
    }
}

/** @{
 *  @if maint
 *  @brief Fills the deque with whatever is in [first,last).
 *  @param  first  An input iterator.
 *  @param  last  An input iterator.
 *  @return   Nothing.
 *
 *  If the iterators are actually forward iterators (or better), then the
 *  memory layout can be done all at once.  Else we move forward using
 *  push_back on each value from the iterator.
 *  @endif
*/
template <class _Tp, class _Alloc> template <class _InputIterator>
void deque<_Tp,_Alloc>::_M_range_initialize(_InputIterator __first,
                                            _InputIterator __last,
                                            input_iterator_tag)
{
  _M_initialize_map(0);
  try {
    for ( ; __first != __last; ++__first)
      push_back(*__first);
  }
  catch(...)
    {
      clear();
      __throw_exception_again;
    }
}

template <class _Tp, class _Alloc> template <class _ForwardIterator>
void deque<_Tp,_Alloc>::_M_range_initialize(_ForwardIterator __first,
                                            _ForwardIterator __last,
                                            forward_iterator_tag)
{
  size_type __n = distance(__first, __last);
  _M_initialize_map(__n);

  _Map_pointer __cur_node;
  try {
    for (__cur_node = _M_start._M_node; 
         __cur_node < _M_finish._M_node; 
         ++__cur_node) {
      _ForwardIterator __mid = __first;
      advance(__mid, _S_buffer_size());
      uninitialized_copy(__first, __mid, *__cur_node);
      __first = __mid;
    }
    uninitialized_copy(__first, __last, _M_finish._M_first);
  }
  catch(...)
    {
      _Destroy(_M_start, iterator(*__cur_node, __cur_node));
      __throw_exception_again;
    }
}
/** @} */

// Called only if _M_finish._M_cur == _M_finish._M_last - 1.
template <class _Tp, class _Alloc>
void
deque<_Tp,_Alloc>::_M_push_back_aux(const value_type& __t)
{
  value_type __t_copy = __t;
  _M_reserve_map_at_back();
  *(_M_finish._M_node + 1) = _M_allocate_node();
  try {
    _Construct(_M_finish._M_cur, __t_copy);
    _M_finish._M_set_node(_M_finish._M_node + 1);
    _M_finish._M_cur = _M_finish._M_first;
  }
  catch(...)
    {
      _M_deallocate_node(*(_M_finish._M_node + 1));
      __throw_exception_again;
    }
}

// Called only if _M_finish._M_cur == _M_finish._M_last - 1.
template <class _Tp, class _Alloc>
void
deque<_Tp,_Alloc>::_M_push_back_aux()
{
  _M_reserve_map_at_back();
  *(_M_finish._M_node + 1) = _M_allocate_node();
  try {
    _Construct(_M_finish._M_cur);
    _M_finish._M_set_node(_M_finish._M_node + 1);
    _M_finish._M_cur = _M_finish._M_first;
  }
  catch(...)
    {
      _M_deallocate_node(*(_M_finish._M_node + 1));
      __throw_exception_again;
    }
}

// Called only if _M_start._M_cur == _M_start._M_first.
template <class _Tp, class _Alloc>
void
deque<_Tp,_Alloc>::_M_push_front_aux(const value_type& __t)
{
  value_type __t_copy = __t;
  _M_reserve_map_at_front();
  *(_M_start._M_node - 1) = _M_allocate_node();
  try {
    _M_start._M_set_node(_M_start._M_node - 1);
    _M_start._M_cur = _M_start._M_last - 1;
    _Construct(_M_start._M_cur, __t_copy);
  }
  catch(...)
    {
      ++_M_start;
      _M_deallocate_node(*(_M_start._M_node - 1));
      __throw_exception_again;
    }
} 

// Called only if _M_start._M_cur == _M_start._M_first.
template <class _Tp, class _Alloc>
void
deque<_Tp,_Alloc>::_M_push_front_aux()
{
  _M_reserve_map_at_front();
  *(_M_start._M_node - 1) = _M_allocate_node();
  try {
    _M_start._M_set_node(_M_start._M_node - 1);
    _M_start._M_cur = _M_start._M_last - 1;
    _Construct(_M_start._M_cur);
  }
  catch(...)
    {
      ++_M_start;
      _M_deallocate_node(*(_M_start._M_node - 1));
      __throw_exception_again;
    }
} 

// Called only if _M_finish._M_cur == _M_finish._M_first.
template <class _Tp, class _Alloc>
void deque<_Tp,_Alloc>::_M_pop_back_aux()
{
  _M_deallocate_node(_M_finish._M_first);
  _M_finish._M_set_node(_M_finish._M_node - 1);
  _M_finish._M_cur = _M_finish._M_last - 1;
  _Destroy(_M_finish._M_cur);
}

// Called only if _M_start._M_cur == _M_start._M_last - 1.  Note that 
// if the deque has at least one element (a precondition for this member 
// function), and if _M_start._M_cur == _M_start._M_last, then the deque 
// must have at least two nodes.
template <class _Tp, class _Alloc>
void deque<_Tp,_Alloc>::_M_pop_front_aux()
{
  _Destroy(_M_start._M_cur);
  _M_deallocate_node(_M_start._M_first);
  _M_start._M_set_node(_M_start._M_node + 1);
  _M_start._M_cur = _M_start._M_first;
}      

template <class _Tp, class _Alloc> template <class _InputIterator>
void deque<_Tp,_Alloc>::insert(iterator __pos,
                               _InputIterator __first, _InputIterator __last,
                               input_iterator_tag)
{
  copy(__first, __last, inserter(*this, __pos));
}

template <class _Tp, class _Alloc> template <class _ForwardIterator>
void
deque<_Tp,_Alloc>::insert(iterator __pos,
                          _ForwardIterator __first, _ForwardIterator __last,
                          forward_iterator_tag) {
  size_type __n = distance(__first, __last);
  if (__pos._M_cur == _M_start._M_cur) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    try {
      uninitialized_copy(__first, __last, __new_start);
      _M_start = __new_start;
    }
    catch(...)
      {
	_M_destroy_nodes(__new_start._M_node, _M_start._M_node);
	__throw_exception_again;
      }
  }
  else if (__pos._M_cur == _M_finish._M_cur) {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    try {
      uninitialized_copy(__first, __last, _M_finish);
      _M_finish = __new_finish;
    }
    catch(...)
      {
	_M_destroy_nodes(_M_finish._M_node + 1, __new_finish._M_node + 1);
	__throw_exception_again;
      }
  }
  else
    _M_insert_aux(__pos, __first, __last, __n);
}

template <class _Tp, class _Alloc>
typename deque<_Tp, _Alloc>::iterator
deque<_Tp,_Alloc>::_M_insert_aux(iterator __pos, const value_type& __x)
{
  difference_type __index = __pos - _M_start;
  value_type __x_copy = __x;
  if (static_cast<size_type>(__index) < size() / 2) {
    push_front(front());
    iterator __front1 = _M_start;
    ++__front1;
    iterator __front2 = __front1;
    ++__front2;
    __pos = _M_start + __index;
    iterator __pos1 = __pos;
    ++__pos1;
    copy(__front2, __pos1, __front1);
  }
  else {
    push_back(back());
    iterator __back1 = _M_finish;
    --__back1;
    iterator __back2 = __back1;
    --__back2;
    __pos = _M_start + __index;
    copy_backward(__pos, __back2, __back1);
  }
  *__pos = __x_copy;
  return __pos;
}

template <class _Tp, class _Alloc>
typename deque<_Tp,_Alloc>::iterator 
deque<_Tp,_Alloc>::_M_insert_aux(iterator __pos)
{
  difference_type __index = __pos - _M_start;
  if (static_cast<size_type>(__index) < size() / 2) {
    push_front(front());
    iterator __front1 = _M_start;
    ++__front1;
    iterator __front2 = __front1;
    ++__front2;
    __pos = _M_start + __index;
    iterator __pos1 = __pos;
    ++__pos1;
    copy(__front2, __pos1, __front1);
  }
  else {
    push_back(back());
    iterator __back1 = _M_finish;
    --__back1;
    iterator __back2 = __back1;
    --__back2;
    __pos = _M_start + __index;
    copy_backward(__pos, __back2, __back1);
  }
  *__pos = value_type();
  return __pos;
}

template <class _Tp, class _Alloc>
void deque<_Tp,_Alloc>::_M_insert_aux(iterator __pos,
                                      size_type __n,
                                      const value_type& __x)
{
  const difference_type __elems_before = __pos - _M_start;
  size_type __length = this->size();
  value_type __x_copy = __x;
  if (__elems_before < difference_type(__length / 2)) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    iterator __old_start = _M_start;
    __pos = _M_start + __elems_before;
    try {
      if (__elems_before >= difference_type(__n)) {
        iterator __start_n = _M_start + difference_type(__n);
        uninitialized_copy(_M_start, __start_n, __new_start);
        _M_start = __new_start;
        copy(__start_n, __pos, __old_start);
        fill(__pos - difference_type(__n), __pos, __x_copy);
      }
      else {
        __uninitialized_copy_fill(_M_start, __pos, __new_start, 
                                  _M_start, __x_copy);
        _M_start = __new_start;
        fill(__old_start, __pos, __x_copy);
      }
    }
    catch(...)
      { 
	_M_destroy_nodes(__new_start._M_node, _M_start._M_node);
	__throw_exception_again;
      }
  }
  else {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    iterator __old_finish = _M_finish;
    const difference_type __elems_after = 
      difference_type(__length) - __elems_before;
    __pos = _M_finish - __elems_after;
    try {
      if (__elems_after > difference_type(__n)) {
        iterator __finish_n = _M_finish - difference_type(__n);
        uninitialized_copy(__finish_n, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy_backward(__pos, __finish_n, __old_finish);
        fill(__pos, __pos + difference_type(__n), __x_copy);
      }
      else {
        __uninitialized_fill_copy(_M_finish, __pos + difference_type(__n),
                                  __x_copy, __pos, _M_finish);
        _M_finish = __new_finish;
        fill(__pos, __old_finish, __x_copy);
      }
    }
    catch(...)
      { 
	_M_destroy_nodes(_M_finish._M_node + 1, __new_finish._M_node + 1);
	__throw_exception_again;
      }
  }
}

template <class _Tp, class _Alloc> template <class _ForwardIterator>
void deque<_Tp,_Alloc>::_M_insert_aux(iterator __pos,
                                      _ForwardIterator __first,
                                      _ForwardIterator __last,
                                      size_type __n)
{
  const difference_type __elemsbefore = __pos - _M_start;
  size_type __length = size();
  if (static_cast<size_type>(__elemsbefore) < __length / 2) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    iterator __old_start = _M_start;
    __pos = _M_start + __elemsbefore;
    try {
      if (__elemsbefore >= difference_type(__n)) {
        iterator __start_n = _M_start + difference_type(__n); 
        uninitialized_copy(_M_start, __start_n, __new_start);
        _M_start = __new_start;
        copy(__start_n, __pos, __old_start);
        copy(__first, __last, __pos - difference_type(__n));
      }
      else {
        _ForwardIterator __mid = __first;
        advance(__mid, difference_type(__n) - __elemsbefore);
        __uninitialized_copy_copy(_M_start, __pos, __first, __mid,
                                  __new_start);
        _M_start = __new_start;
        copy(__mid, __last, __old_start);
      }
    }
    catch(...)
      {
	_M_destroy_nodes(__new_start._M_node, _M_start._M_node);
	__throw_exception_again;
      }
  }
  else {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    iterator __old_finish = _M_finish;
    const difference_type __elemsafter = 
      difference_type(__length) - __elemsbefore;
    __pos = _M_finish - __elemsafter;
    try {
      if (__elemsafter > difference_type(__n)) {
        iterator __finish_n = _M_finish - difference_type(__n);
        uninitialized_copy(__finish_n, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy_backward(__pos, __finish_n, __old_finish);
        copy(__first, __last, __pos);
      }
      else {
        _ForwardIterator __mid = __first;
        advance(__mid, __elemsafter);
        __uninitialized_copy_copy(__mid, __last, __pos, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy(__first, __mid, __pos);
      }
    }
    catch(...)
      {
	_M_destroy_nodes(_M_finish._M_node + 1, __new_finish._M_node + 1);
	__throw_exception_again;
      }
  }
}

template <class _Tp, class _Alloc>
void deque<_Tp,_Alloc>::_M_new_elements_at_front(size_type __new_elems)
{
  size_type __new_nodes
      = (__new_elems + _S_buffer_size() - 1) / _S_buffer_size();
  _M_reserve_map_at_front(__new_nodes);
  size_type __i;
  try {
    for (__i = 1; __i <= __new_nodes; ++__i)
      *(_M_start._M_node - __i) = _M_allocate_node();
  }
  catch(...) {
    for (size_type __j = 1; __j < __i; ++__j)
      _M_deallocate_node(*(_M_start._M_node - __j));      
    __throw_exception_again;
  }
}

template <class _Tp, class _Alloc>
void deque<_Tp,_Alloc>::_M_new_elements_at_back(size_type __new_elems)
{
  size_type __new_nodes
      = (__new_elems + _S_buffer_size() - 1) / _S_buffer_size();
  _M_reserve_map_at_back(__new_nodes);
  size_type __i;
  try {
    for (__i = 1; __i <= __new_nodes; ++__i)
      *(_M_finish._M_node + __i) = _M_allocate_node();
  }
  catch(...) {
    for (size_type __j = 1; __j < __i; ++__j)
      _M_deallocate_node(*(_M_finish._M_node + __j));      
    __throw_exception_again;
  }
}

template <class _Tp, class _Alloc>
void deque<_Tp,_Alloc>::_M_reallocate_map(size_type __nodes_to_add,
                                          bool __add_at_front)
{
  size_type __old_num_nodes = _M_finish._M_node - _M_start._M_node + 1;
  size_type __new_num_nodes = __old_num_nodes + __nodes_to_add;

  _Map_pointer __new_nstart;
  if (_M_map_size > 2 * __new_num_nodes) {
    __new_nstart = _M_map + (_M_map_size - __new_num_nodes) / 2 
                     + (__add_at_front ? __nodes_to_add : 0);
    if (__new_nstart < _M_start._M_node)
      copy(_M_start._M_node, _M_finish._M_node + 1, __new_nstart);
    else
      copy_backward(_M_start._M_node, _M_finish._M_node + 1, 
                    __new_nstart + __old_num_nodes);
  }
  else {
    size_type __new_map_size = 
      _M_map_size + max(_M_map_size, __nodes_to_add) + 2;

    _Map_pointer __new_map = _M_allocate_map(__new_map_size);
    __new_nstart = __new_map + (__new_map_size - __new_num_nodes) / 2
                         + (__add_at_front ? __nodes_to_add : 0);
    copy(_M_start._M_node, _M_finish._M_node + 1, __new_nstart);
    _M_deallocate_map(_M_map, _M_map_size);

    _M_map = __new_map;
    _M_map_size = __new_map_size;
  }

  _M_start._M_set_node(__new_nstart);
  _M_finish._M_set_node(__new_nstart + __old_num_nodes - 1);
}


// Nonmember functions.

template <class _Tp, class _Alloc>
inline bool operator==(const deque<_Tp, _Alloc>& __x,
                       const deque<_Tp, _Alloc>& __y) {
  return __x.size() == __y.size() &&
         equal(__x.begin(), __x.end(), __y.begin());
}

template <class _Tp, class _Alloc>
inline bool operator<(const deque<_Tp, _Alloc>& __x,
                      const deque<_Tp, _Alloc>& __y) {
  return lexicographical_compare(__x.begin(), __x.end(), 
                                 __y.begin(), __y.end());
}

template <class _Tp, class _Alloc>
inline bool operator!=(const deque<_Tp, _Alloc>& __x,
                       const deque<_Tp, _Alloc>& __y) {
  return !(__x == __y);
}

template <class _Tp, class _Alloc>
inline bool operator>(const deque<_Tp, _Alloc>& __x,
                      const deque<_Tp, _Alloc>& __y) {
  return __y < __x;
}

template <class _Tp, class _Alloc>
inline bool operator<=(const deque<_Tp, _Alloc>& __x,
                       const deque<_Tp, _Alloc>& __y) {
  return !(__y < __x);
}
template <class _Tp, class _Alloc>
inline bool operator>=(const deque<_Tp, _Alloc>& __x,
                       const deque<_Tp, _Alloc>& __y) {
  return !(__x < __y);
}

template <class _Tp, class _Alloc>
inline void swap(deque<_Tp,_Alloc>& __x, deque<_Tp,_Alloc>& __y) {
  __x.swap(__y);
}

} // namespace std 
  
#endif /* __GLIBCPP_INTERNAL_DEQUE_H */

