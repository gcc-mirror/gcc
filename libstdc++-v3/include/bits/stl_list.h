// List implementation -*- C++ -*-

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

/** @file stl_list.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef __GLIBCPP_INTERNAL_LIST_H
#define __GLIBCPP_INTERNAL_LIST_H

#include <bits/concept_check.h>

namespace std
{

  struct _List_node_base
  {
    _List_node_base* _M_next;
    _List_node_base* _M_prev;
  };

  template<typename _Tp>
    struct _List_node : public _List_node_base
    {
      _Tp _M_data;
    };

  struct _List_iterator_base
  {
    typedef size_t                     size_type;
    typedef ptrdiff_t                  difference_type;
    typedef bidirectional_iterator_tag iterator_category;

    _List_node_base* _M_node;

    _List_iterator_base(_List_node_base* __x)
    : _M_node(__x)
    { }

    _List_iterator_base()
    { }

    void
    _M_incr()
    { _M_node = _M_node->_M_next; }

    void
    _M_decr()
    { _M_node = _M_node->_M_prev; }

    bool
    operator==(const _List_iterator_base& __x) const
    { return _M_node == __x._M_node; }

    bool
    operator!=(const _List_iterator_base& __x) const
    { return _M_node != __x._M_node; }
  };  

  template<typename _Tp, typename _Ref, typename _Ptr>
    struct _List_iterator : public _List_iterator_base
    {
      typedef _List_iterator<_Tp,_Tp&,_Tp*>             iterator;
      typedef _List_iterator<_Tp,const _Tp&,const _Tp*> const_iterator;
      typedef _List_iterator<_Tp,_Ref,_Ptr>             _Self;

      typedef _Tp value_type;
      typedef _Ptr pointer;
      typedef _Ref reference;
      typedef _List_node<_Tp> _Node;

      _List_iterator(_Node* __x)
      : _List_iterator_base(__x)
      { }

      _List_iterator()
      { }

      _List_iterator(const iterator& __x)
      : _List_iterator_base(__x._M_node)
      { }

      reference
      operator*() const
      { return ((_Node*) _M_node)->_M_data; }

      pointer
      operator->() const
      { return &(operator*()); }

      _Self&
      operator++()
      { 
        this->_M_incr();
        return *this;
      }

      _Self
      operator++(int)
      { 
        _Self __tmp = *this;
        this->_M_incr();
        return __tmp;
      }

      _Self&
      operator--()
      { 
        this->_M_decr();
        return *this;
      }

      _Self
      operator--(int)
      { 
        _Self __tmp = *this;
        this->_M_decr();
        return __tmp;
      }
    };


  // Base class that encapsulates details of allocators.  Three cases:
  // an ordinary standard-conforming allocator, a standard-conforming
  // allocator with no non-static data, and an SGI-style allocator.
  // This complexity is necessary only because we're worrying about backward
  // compatibility and because we want to avoid wasting storage on an 
  // allocator instance if it isn't necessary.


  // Base for general standard-conforming allocators.
  template<typename _Tp, typename _Allocator, bool _IsStatic>
    class _List_alloc_base
    {
    public:
      typedef typename _Alloc_traits<_Tp, _Allocator>::allocator_type
              allocator_type;

      allocator_type
      get_allocator() const
      { return _Node_allocator; }

      _List_alloc_base(const allocator_type& __a)
      : _Node_allocator(__a)
      { }

    protected:
      _List_node<_Tp>*
      _M_get_node()
      { return _Node_allocator.allocate(1); }

      void
      _M_put_node(_List_node<_Tp>* __p)
      { _Node_allocator.deallocate(__p, 1); }

    protected:
      typename _Alloc_traits<_List_node<_Tp>, _Allocator>::allocator_type
               _Node_allocator;

      _List_node<_Tp>* _M_node;
    };

  // Specialization for instanceless allocators.

  template<typename _Tp, typename _Allocator>
    class _List_alloc_base<_Tp, _Allocator, true>
    {
    public:
      typedef typename _Alloc_traits<_Tp, _Allocator>::allocator_type
              allocator_type;

      allocator_type
      get_allocator() const
      { return allocator_type(); }

      _List_alloc_base(const allocator_type&)
      { }

    protected:
      typedef typename _Alloc_traits<_List_node<_Tp>, _Allocator>::_Alloc_type
              _Alloc_type;

      _List_node<_Tp>*
      _M_get_node()
      { return _Alloc_type::allocate(1); }

      void
      _M_put_node(_List_node<_Tp>* __p)
      { _Alloc_type::deallocate(__p, 1); }

    protected:
      _List_node<_Tp>* _M_node;
    };

  template<typename _Tp, typename _Alloc>
    class _List_base 
      : public _List_alloc_base<_Tp, _Alloc,
                                _Alloc_traits<_Tp, _Alloc>::_S_instanceless>
    {
    public:
      typedef _List_alloc_base<_Tp, _Alloc,
                               _Alloc_traits<_Tp, _Alloc>::_S_instanceless>
              _Base; 
      typedef typename _Base::allocator_type allocator_type;

      _List_base(const allocator_type& __a)
      : _Base(__a)
      {
        _M_node = _M_get_node();
        _M_node->_M_next = _M_node;
        _M_node->_M_prev = _M_node;
      }

      ~_List_base()
      {
        clear();
        _M_put_node(_M_node);
      }

      void clear();
    };

  /**
   *  @ingroup Containers
   *  @ingroup Sequences
   *
   *  Meets the requirements of a <a href="tables.html#65">container</a>, a
   *  <a href="tables.html#66">reversible container</a>, and a
   *  <a href="tables.html#67">sequence</a>, including the
   *  <a href="tables.html#68">optional sequence requirements</a> with the
   *  %exception of @c at and @c operator[].
   *
   *  @doctodo
   *
  */
  template<typename _Tp, typename _Alloc = allocator<_Tp> >
    class list : protected _List_base<_Tp, _Alloc>
    {
      // concept requirements
      __glibcpp_class_requires(_Tp, _SGIAssignableConcept)

      typedef _List_base<_Tp, _Alloc> _Base;
    protected:
      typedef void* _Void_pointer;

    public:      
      typedef _Tp value_type;
      typedef value_type* pointer;
      typedef const value_type* const_pointer;
      typedef value_type& reference;
      typedef const value_type& const_reference;
      typedef _List_node<_Tp> _Node;
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;

      typedef typename _Base::allocator_type allocator_type;

      typedef _List_iterator<_Tp,_Tp&,_Tp*>             iterator;
      typedef _List_iterator<_Tp,const _Tp&,const _Tp*> const_iterator;

      typedef reverse_iterator<const_iterator> const_reverse_iterator;
      typedef reverse_iterator<iterator>       reverse_iterator;

    protected:
      using _Base::_M_node;
      using _Base::_M_put_node;
      using _Base::_M_get_node;

    protected:
      _Node*
      _M_create_node(const _Tp& __x)
      {
        _Node* __p = _M_get_node();
        try {
          _Construct(&__p->_M_data, __x);
        }
        catch(...)
        { 
          _M_put_node(__p);
          __throw_exception_again; 
        }
        return __p;
      }

      _Node*
      _M_create_node()
      {
        _Node* __p = _M_get_node();
        try {
          _Construct(&__p->_M_data);
        }
        catch(...)
        { 
          _M_put_node(__p);
          __throw_exception_again; 
        }
        return __p;
      }

    public:
      allocator_type
      get_allocator() const
      { return _Base::get_allocator(); }

      explicit
      list(const allocator_type& __a = allocator_type())
      : _Base(__a)
      { }

      iterator
      begin()
      { return static_cast<_Node*>(_M_node->_M_next); }

      const_iterator
      begin() const
      { return static_cast<_Node*>(_M_node->_M_next); }

      iterator
      end()
      { return _M_node; }

      const_iterator
      end() const
      { return _M_node; }

      reverse_iterator
      rbegin() 
      { return reverse_iterator(end()); }

      const_reverse_iterator
      rbegin() const 
      { return const_reverse_iterator(end()); }

      reverse_iterator
      rend()
      { return reverse_iterator(begin()); }

      const_reverse_iterator
      rend() const
      { return const_reverse_iterator(begin()); }

      bool
      empty() const
      { return _M_node->_M_next == _M_node; }

      size_type
      size() const
      { return distance(begin(), end()); }

      size_type
      max_size() const
      { return size_type(-1); }

      reference
      front()
      { return *begin(); }

      const_reference
      front() const
      { return *begin(); }

      reference
      back()
      { return *(--end()); }

      const_reference
      back() const
      { return *(--end()); }

      void
      swap(list<_Tp, _Alloc>& __x)
      { std::swap(_M_node, __x._M_node); }

      iterator
      insert(iterator __position, const _Tp& __x)
      {
        _Node* __tmp = _M_create_node(__x);
        __tmp->_M_next = __position._M_node;
        __tmp->_M_prev = __position._M_node->_M_prev;
        __position._M_node->_M_prev->_M_next = __tmp;
        __position._M_node->_M_prev = __tmp;
        return __tmp;
      }

      iterator
      insert(iterator __position)
      { return insert(__position, _Tp()); }

      // Check whether it's an integral type.  If so, it's not an iterator.
      template<typename _Integer>
        void
        _M_insert_dispatch(iterator __pos, _Integer __n, _Integer __x, __true_type)
        { _M_fill_insert(__pos, (size_type) __n, (_Tp) __x); }

      template<typename _InputIterator>
        void
        _M_insert_dispatch(iterator __pos,
                           _InputIterator __first, _InputIterator __last,
                           __false_type);

      template<typename _InputIterator>
        void
        insert(iterator __pos, _InputIterator __first, _InputIterator __last)
        {
          typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
          _M_insert_dispatch(__pos, __first, __last, _Integral());
        }

      void
      insert(iterator __pos, size_type __n, const _Tp& __x)
      { _M_fill_insert(__pos, __n, __x); }

      void
      _M_fill_insert(iterator __pos, size_type __n, const _Tp& __x); 

      void
      push_front(const _Tp& __x)
      { insert(begin(), __x); }

      void
      push_front()
      { insert(begin()); }

      void
      push_back(const _Tp& __x)
      { insert(end(), __x); }

      void
      push_back()
      { insert(end()); }

      iterator
      erase(iterator __position)
      {
        _List_node_base* __next_node = __position._M_node->_M_next;
        _List_node_base* __prev_node = __position._M_node->_M_prev;
        _Node* __n = static_cast<_Node*>(__position._M_node);
        __prev_node->_M_next = __next_node;
        __next_node->_M_prev = __prev_node;
        _Destroy(&__n->_M_data);
        _M_put_node(__n);
        return iterator(static_cast<_Node*>(__next_node));
      }

      iterator
      erase(iterator __first, iterator __last);

      void
      clear()
      { _Base::clear(); }

      void
      resize(size_type __new_size, const _Tp& __x);
      
      void
      resize(size_type __new_size)
      { this->resize(__new_size, _Tp()); }

      void
      pop_front()
      { erase(begin()); }

      void
      pop_back()
      { 
        iterator __tmp = end();
        erase(--__tmp);
      }

      list(size_type __n, const _Tp& __value,
           const allocator_type& __a = allocator_type())
      : _Base(__a)
      { insert(begin(), __n, __value); }

      explicit
      list(size_type __n)
      : _Base(allocator_type())
      { insert(begin(), __n, _Tp()); }

      // We don't need any dispatching tricks here, because insert does all of
      // that anyway.  
      template<typename _InputIterator>
      list(_InputIterator __first, _InputIterator __last,
           const allocator_type& __a = allocator_type())
      : _Base(__a)
      { insert(begin(), __first, __last); }

      list(const list<_Tp, _Alloc>& __x)
      : _Base(__x.get_allocator())
      { insert(begin(), __x.begin(), __x.end()); }

      ~list()
      { }

      list<_Tp, _Alloc>&
      operator=(const list<_Tp, _Alloc>& __x);

    public:
      // assign(), a generalized assignment member function.  Two
      // versions: one that takes a count, and one that takes a range.
      // The range version is a member template, so we dispatch on whether
      // or not the type is an integer.

      void
      assign(size_type __n, const _Tp& __val)
      { _M_fill_assign(__n, __val); }

      void
      _M_fill_assign(size_type __n, const _Tp& __val);

      template<typename _InputIterator>
        void
        assign(_InputIterator __first, _InputIterator __last)
        {
          typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
          _M_assign_dispatch(__first, __last, _Integral());
        }

      template<typename _Integer>
        void
        _M_assign_dispatch(_Integer __n, _Integer __val, __true_type)
        { _M_fill_assign((size_type) __n, (_Tp) __val); }

      template<typename _InputIterator>
        void
        _M_assign_dispatch(_InputIterator __first, _InputIterator __last,
                           __false_type);

    protected:
      void
      _M_transfer(iterator __position, iterator __first, iterator __last)
      {
        if (__position != __last) {
          // Remove [first, last) from its old position.
          __last._M_node->_M_prev->_M_next     = __position._M_node;
          __first._M_node->_M_prev->_M_next    = __last._M_node;
          __position._M_node->_M_prev->_M_next = __first._M_node; 

          // Splice [first, last) into its new position.
          _List_node_base* __tmp      = __position._M_node->_M_prev;
          __position._M_node->_M_prev = __last._M_node->_M_prev;
          __last._M_node->_M_prev     = __first._M_node->_M_prev; 
          __first._M_node->_M_prev    = __tmp;
        }
      }

    public:
      void
      splice(iterator __position, list& __x)
      {
        if (!__x.empty()) 
          this->_M_transfer(__position, __x.begin(), __x.end());
      }

      void
      splice(iterator __position, list&, iterator __i)
      {
        iterator __j = __i;
        ++__j;
        if (__position == __i || __position == __j) return;
        this->_M_transfer(__position, __i, __j);
      }

      void
      splice(iterator __position, list&, iterator __first, iterator __last)
      {
        if (__first != __last) 
          this->_M_transfer(__position, __first, __last);
      }

      void
      remove(const _Tp& __value);

      void
      unique();

      void
      merge(list& __x);

      void
      reverse();

      void
      sort();

      template<typename _Predicate>
        void
        remove_if(_Predicate);

      template<typename _BinaryPredicate>
        void
        unique(_BinaryPredicate);

      template<typename _StrictWeakOrdering>
        void
        merge(list&, _StrictWeakOrdering);

      template<typename _StrictWeakOrdering>
        void
        sort(_StrictWeakOrdering);
    };

  template<typename _Tp, typename _Alloc>
    inline bool 
    operator==(const list<_Tp,_Alloc>& __x, const list<_Tp,_Alloc>& __y)
    {
      typedef typename list<_Tp,_Alloc>::const_iterator const_iterator;
      const_iterator __end1 = __x.end();
      const_iterator __end2 = __y.end();

      const_iterator __i1 = __x.begin();
      const_iterator __i2 = __y.begin();
      while (__i1 != __end1 && __i2 != __end2 && *__i1 == *__i2) {
        ++__i1;
        ++__i2;
      }
      return __i1 == __end1 && __i2 == __end2;
    }

  template<typename _Tp, typename _Alloc>
    inline bool
    operator<(const list<_Tp,_Alloc>& __x, const list<_Tp,_Alloc>& __y)
    {
      return lexicographical_compare(__x.begin(), __x.end(),
                                     __y.begin(), __y.end());
    }

  template<typename _Tp, typename _Alloc>
    inline bool
    operator!=(const list<_Tp,_Alloc>& __x, const list<_Tp,_Alloc>& __y)
    { return !(__x == __y); }

  template<typename _Tp, typename _Alloc>
    inline bool
    operator>(const list<_Tp,_Alloc>& __x, const list<_Tp,_Alloc>& __y)
    { return __y < __x; }

  template<typename _Tp, typename _Alloc>
    inline bool
    operator<=(const list<_Tp,_Alloc>& __x, const list<_Tp,_Alloc>& __y)
    { return !(__y < __x); }

  template<typename _Tp, typename _Alloc>
    inline bool
    operator>=(const list<_Tp,_Alloc>& __x, const list<_Tp,_Alloc>& __y)
    { return !(__x < __y); }

  template<typename _Tp, typename _Alloc>
    inline void 
    swap(list<_Tp, _Alloc>& __x, list<_Tp, _Alloc>& __y)
    { __x.swap(__y); }

  // move these to stl_list.tcc

  template<typename _Tp, typename _Alloc>
    void _List_base<_Tp,_Alloc>::
    clear() 
    {
      _List_node<_Tp>* __cur = static_cast<_List_node<_Tp>*>(_M_node->_M_next);
      while (__cur != _M_node) {
        _List_node<_Tp>* __tmp = __cur;
        __cur = static_cast<_List_node<_Tp>*>(__cur->_M_next);
        _Destroy(&__tmp->_M_data);
        _M_put_node(__tmp);
      }
      _M_node->_M_next = _M_node;
      _M_node->_M_prev = _M_node;
    }

  template<typename _Tp, typename _Alloc>
    template <typename _InputIter>
      void list<_Tp, _Alloc>::
      _M_insert_dispatch(iterator __position, _InputIter __first, _InputIter __last,
                                            __false_type)
      {
        for ( ; __first != __last; ++__first)
          insert(__position, *__first);
      
      }

  template<typename _Tp, typename _Alloc>
    void list<_Tp, _Alloc>::
    _M_fill_insert(iterator __position, size_type __n, const _Tp& __x)
    {
      for ( ; __n > 0; --__n)
        insert(__position, __x);
    }

  template<typename _Tp, typename _Alloc>
    typename list<_Tp,_Alloc>::iterator list<_Tp, _Alloc>::
    erase(iterator __first, iterator __last)
    {
      while (__first != __last)
        erase(__first++);
      return __last;
    }

  template<typename _Tp, typename _Alloc>
    void list<_Tp, _Alloc>::
    resize(size_type __new_size, const _Tp& __x)
    {
      iterator __i = begin();
      size_type __len = 0;
      for ( ; __i != end() && __len < __new_size; ++__i, ++__len)
        ;
      if (__len == __new_size)
        erase(__i, end());
      else                          // __i == end()
        insert(end(), __new_size - __len, __x);
    }

  template<typename _Tp, typename _Alloc>
    list<_Tp, _Alloc>& list<_Tp, _Alloc>::
    operator=(const list<_Tp, _Alloc>& __x)
    {
      if (this != &__x) {
        iterator __first1 = begin();
        iterator __last1 = end();
        const_iterator __first2 = __x.begin();
        const_iterator __last2 = __x.end();
        while (__first1 != __last1 && __first2 != __last2) 
          *__first1++ = *__first2++;
        if (__first2 == __last2)
          erase(__first1, __last1);
        else
          insert(__last1, __first2, __last2);
      }
      return *this;
    }

  template<typename _Tp, typename _Alloc>
    void list<_Tp, _Alloc>::
    _M_fill_assign(size_type __n, const _Tp& __val) {
      iterator __i = begin();
      for ( ; __i != end() && __n > 0; ++__i, --__n)
        *__i = __val;
      if (__n > 0)
        insert(end(), __n, __val);
      else
        erase(__i, end());
    }

  template<typename _Tp, typename _Alloc>
    template <typename _InputIter>
      void list<_Tp, _Alloc>::
      _M_assign_dispatch(_InputIter __first2, _InputIter __last2, __false_type)
      {
        iterator __first1 = begin();
        iterator __last1 = end();
        for ( ; __first1 != __last1 && __first2 != __last2; ++__first1, ++__first2)
          *__first1 = *__first2;
        if (__first2 == __last2)
          erase(__first1, __last1);
        else
          insert(__last1, __first2, __last2);
      }

  template<typename _Tp, typename _Alloc>
    void list<_Tp, _Alloc>::
    remove(const _Tp& __value)
    {
      iterator __first = begin();
      iterator __last = end();
      while (__first != __last) {
        iterator __next = __first;
        ++__next;
        if (*__first == __value) erase(__first);
        __first = __next;
      }
    }

  template<typename _Tp, typename _Alloc>
    void list<_Tp, _Alloc>::
    unique()
    {
      iterator __first = begin();
      iterator __last = end();
      if (__first == __last) return;
      iterator __next = __first;
      while (++__next != __last) {
        if (*__first == *__next)
          erase(__next);
        else
          __first = __next;
        __next = __first;
      }
    }

  template<typename _Tp, typename _Alloc>
    void list<_Tp, _Alloc>::
    merge(list<_Tp, _Alloc>& __x)
    {
      iterator __first1 = begin();
      iterator __last1 = end();
      iterator __first2 = __x.begin();
      iterator __last2 = __x.end();
      while (__first1 != __last1 && __first2 != __last2)
        if (*__first2 < *__first1) {
          iterator __next = __first2;
          _M_transfer(__first1, __first2, ++__next);
          __first2 = __next;
        }
        else
          ++__first1;
      if (__first2 != __last2) _M_transfer(__last1, __first2, __last2);
    }

  inline void
  __List_base_reverse(_List_node_base* __p)
  {
    _List_node_base* __tmp = __p;
    do {
      std::swap(__tmp->_M_next, __tmp->_M_prev);
      __tmp = __tmp->_M_prev;     // Old next node is now prev.
    } while (__tmp != __p);
  }

  template<typename _Tp, typename _Alloc>
  inline void list<_Tp, _Alloc>::
  reverse() 
  { __List_base_reverse(this->_M_node); }    

  template<typename _Tp, typename _Alloc>
    void list<_Tp, _Alloc>::
    sort()
    {
      // Do nothing if the list has length 0 or 1.
      if (_M_node->_M_next != _M_node && _M_node->_M_next->_M_next != _M_node) {
        list<_Tp, _Alloc> __carry;
        list<_Tp, _Alloc> __counter[64];
        int __fill = 0;
        while (!empty()) {
          __carry.splice(__carry.begin(), *this, begin());
          int __i = 0;
          while(__i < __fill && !__counter[__i].empty()) {
            __counter[__i].merge(__carry);
            __carry.swap(__counter[__i++]);
          }
          __carry.swap(__counter[__i]);         
          if (__i == __fill) ++__fill;
        } 

        for (int __i = 1; __i < __fill; ++__i)
          __counter[__i].merge(__counter[__i-1]);
        swap(__counter[__fill-1]);
      }
    }

  template<typename _Tp, typename _Alloc>
    template <typename _Predicate>
      void list<_Tp, _Alloc>::
      remove_if(_Predicate __pred)
      {
        iterator __first = begin();
        iterator __last = end();
        while (__first != __last) {
          iterator __next = __first;
          ++__next;
          if (__pred(*__first)) erase(__first);
          __first = __next;
        }
      }

  template<typename _Tp, typename _Alloc>
    template <typename _BinaryPredicate>
      void list<_Tp, _Alloc>::
      unique(_BinaryPredicate __binary_pred)
      {
        iterator __first = begin();
        iterator __last = end();
        if (__first == __last) return;
        iterator __next = __first;
        while (++__next != __last) {
          if (__binary_pred(*__first, *__next))
            erase(__next);
          else
            __first = __next;
          __next = __first;
        }
      }

  template<typename _Tp, typename _Alloc>
    template <typename _StrictWeakOrdering>
      void list<_Tp, _Alloc>::
      merge(list<_Tp, _Alloc>& __x, _StrictWeakOrdering __comp)
      {
        iterator __first1 = begin();
        iterator __last1 = end();
        iterator __first2 = __x.begin();
        iterator __last2 = __x.end();
        while (__first1 != __last1 && __first2 != __last2)
          if (__comp(*__first2, *__first1)) {
            iterator __next = __first2;
            _M_transfer(__first1, __first2, ++__next);
            __first2 = __next;
          }
          else
            ++__first1;
        if (__first2 != __last2) _M_transfer(__last1, __first2, __last2);
      }

  template<typename _Tp, typename _Alloc>
    template <typename _StrictWeakOrdering>
    void list<_Tp, _Alloc>::
    sort(_StrictWeakOrdering __comp)
    {
      // Do nothing if the list has length 0 or 1.
      if (_M_node->_M_next != _M_node && _M_node->_M_next->_M_next != _M_node) {
        list<_Tp, _Alloc> __carry;
        list<_Tp, _Alloc> __counter[64];
        int __fill = 0;
        while (!empty()) {
          __carry.splice(__carry.begin(), *this, begin());
          int __i = 0;
          while(__i < __fill && !__counter[__i].empty()) {
            __counter[__i].merge(__carry, __comp);
            __carry.swap(__counter[__i++]);
          }
          __carry.swap(__counter[__i]);         
          if (__i == __fill) ++__fill;
        } 

        for (int __i = 1; __i < __fill; ++__i) 
          __counter[__i].merge(__counter[__i-1], __comp);
        swap(__counter[__fill-1]);
      }
    }

} // namespace std 

#endif /* __GLIBCPP_INTERNAL_LIST_H */

// vi:set ts=2 sw=2:
// Local Variables:
// mode:C++
// End:
