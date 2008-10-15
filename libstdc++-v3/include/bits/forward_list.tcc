// <forward_list.tcc> -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file forward_list.tcc
 *  This is a Standard C++ Library header.
 */

#ifndef _FORWARD_LIST_TCC
#define _FORWARD_LIST_TCC 1

_GLIBCXX_BEGIN_NAMESPACE(std)

 /**
  *  @brief  Sort the singly linked list starting after this node.
  *          This node is assumed to be an empty head node (of type
  *          _Fwd_list_node_base).
  */
  template<typename _Tp>
    template<typename _Comp>
      void
      _Fwd_list_node<_Tp>::
      _M_sort_after(_Comp __comp)
      {
	// If `next' is 0, return immediately.
	_Fwd_list_node* __list = static_cast<_Fwd_list_node*>(this->_M_next);
	if (!__list)
	  return;

	unsigned long __insize = 1;

	while (1)
	  {
	    _Fwd_list_node* __p = __list;
	    __list = 0;
	    _Fwd_list_node* __tail = 0;

	    // Count number of merges we do in this pass.
	    unsigned long __nmerges = 0;

	    while (__p)
	      {
		++__nmerges;
		// There exists a merge to be done.
		// Step `insize' places along from p.
		_Fwd_list_node* __q = __p;
		unsigned long __psize = 0;
		for (unsigned long __i = 0; __i < __insize; ++__i)
		  {
		    ++__psize;
		    __q = static_cast<_Fwd_list_node*>(__q->_M_next);
		    if (!__q)
		      break;
		  }

		// If q hasn't fallen off end, we have two lists to merge.
		unsigned long __qsize = __insize;

		// Now we have two lists; merge them.
		while (__psize > 0 || (__qsize > 0 && __q))
		  {
		    // Decide whether next node of merge comes from p or q.
		    _Fwd_list_node<_Tp>* __e;
		    if (__psize == 0)
		      {
			// p is empty; e must come from q.
			__e = __q;
			__q = static_cast<_Fwd_list_node*>(__q->_M_next);
			--__qsize;
		      }
		    else if (__qsize == 0 || !__q)
		      {
			// q is empty; e must come from p.
			__e = __p;
			__p = static_cast<_Fwd_list_node*>(__p->_M_next);
			--__psize;
		      }
		    else if (__comp(__p->_M_value, __q->_M_value))
		      {
			// First node of p is lower; e must come from p.
			__e = __p;
			__p = static_cast<_Fwd_list_node*>(__p->_M_next);
			--__psize;
		      }
		    else
		      {
			// First node of q is lower; e must come from q.
			__e = __q;
			__q = static_cast<_Fwd_list_node*>(__q->_M_next);
			--__qsize;
		      }

		    // Add the next node to the merged list.
		    if (__tail)
		      __tail->_M_next = __e;
		    else
		      __list = __e;
		    __tail = __e;
		  }

		// Now p has stepped `insize' places along, and q has too.
		__p = __q;
	      }
	    __tail->_M_next = 0;

	    // If we have done only one merge, we're finished.
	    // Allow for nmerges == 0, the empty list case.
	    if (__nmerges <= 1)
	      {
		this->_M_next = __list;
		return;
	      }

	    // Otherwise repeat, merging lists twice the size.
	    __insize *= 2;
	  }
      }
 
  template<typename _Tp, typename _Alloc>
    _Fwd_list_base<_Tp, _Alloc>::
    _Fwd_list_base(const _Fwd_list_base& __lst, const _Alloc& __a)
    : _M_impl(__a)
    {
      this->_M_impl._M_head._M_next = 0;
      _Fwd_list_node_base* __to = &this->_M_impl._M_head;
      _Node* __curr = static_cast<_Node*>(__lst._M_impl._M_head._M_next);
      while (__curr)
	{
	  __to->_M_next = _M_create_node(__curr->_M_value);
	  __to = __to->_M_next;
	  __curr = static_cast<_Node*>(__curr->_M_next);
	}
    }

  template<typename _Tp, typename _Alloc>
    _Fwd_list_node_base*
    _Fwd_list_base<_Tp, _Alloc>::
    _M_erase_after(_Fwd_list_node_base* __pos)
    {
      _Node* __curr = static_cast<_Node*>(__pos->_M_next);
      if (__curr)
	{
	  _Fwd_list_node_base* __next = __curr->_M_next;
	  __pos->_M_next = __next;
	  _M_get_Node_allocator().destroy(__curr);
	  _M_put_node(__curr);
	}
      return __pos;
    }

  template<typename _Tp, typename _Alloc>
    _Fwd_list_node_base*
    _Fwd_list_base<_Tp, _Alloc>::
    _M_erase_after(_Fwd_list_node_base* __pos, _Fwd_list_node_base* __last)
    {
      _Node* __curr = static_cast<_Node*>(__pos->_M_next);
      while (__curr)
	{
	  _Node* __temp = __curr;
	  __curr = static_cast<_Node*>(__curr->_M_next);
	  _M_get_Node_allocator().destroy(__temp);
	  _M_put_node(__temp);
	  __pos->_M_next = __curr;
	  if (__temp == __last)
	    break;
	}
      return __pos;
    }
  
  template<typename _Tp, typename _Alloc>
    forward_list<_Tp, _Alloc>::
    forward_list(size_type __n)
    : _Base()
    {
      _Fwd_list_node_base* __to = &this->_M_impl._M_head;
      for (size_type __i = 0; __i < __n; ++__i)
	{
	  __to->_M_next = _M_create_node(_Tp());
	  __to = __to->_M_next;
	}
    }

  template<typename _Tp, typename _Alloc>
    forward_list<_Tp, _Alloc>::
    forward_list(size_type __n, const _Tp& __value, const _Alloc& __al)
    : _Base(__al)
    {
      _Fwd_list_node_base* __to = &this->_M_impl._M_head;
      for (size_type __i = 0; __i < __n; ++__i)
	{
	  __to->_M_next = _M_create_node(__value);
	  __to = __to->_M_next;
	}
    }

  template<typename _Tp, typename _Alloc>
    template<typename _InputIterator>
      forward_list<_Tp, _Alloc>::
      forward_list(_InputIterator __first, _InputIterator __last,
                   const _Alloc& __al)
      : _Base(__al)
      {
        _Fwd_list_node_base* __to = &this->_M_impl._M_head;
        _InputIterator __curr = __first;
        while (__curr != __last)
          {
            __to->_M_next = _M_create_node(*__curr);
            __to = __to->_M_next;
            ++__curr;
          }
      }

  template<typename _Tp, typename _Alloc>
    forward_list<_Tp, _Alloc>::
    forward_list(const forward_list& __list)
    : _Base(__list._M_get_Node_allocator())
    {
      const _Fwd_list_node_base* __from = &__list._M_impl._M_head;
      _Fwd_list_node_base* __to = &this->_M_impl._M_head;
      while (__from->_M_next != 0)
	{
	  const _Node* __temp = static_cast<_Node*>(__from->_M_next);
	  __to->_M_next = _M_create_node(__temp->_M_value);
	  __from = __from->_M_next;
	  __to = __to->_M_next;
	}
    }

  template<typename _Tp, typename _Alloc>
    forward_list<_Tp, _Alloc>::
    forward_list(std::initializer_list<_Tp> __il, const _Alloc& __al)
    : _Base(__al)
    {
      _Fwd_list_node_base* __to = &this->_M_impl._M_head;
      for (const _Tp* __item = __il.begin();
	   __item != __il.end(); ++__item)
	{
	  __to->_M_next = _M_create_node(*__item);
	  __to = __to->_M_next;
	}
    }

  template<typename _Tp, typename _Alloc>
    forward_list<_Tp, _Alloc>&
    forward_list<_Tp, _Alloc>::
    operator=(const forward_list& __list)
    {
      if (&__list != this)
	{
	  iterator __prev1 = before_begin();
	  iterator __curr1 = begin();
	  iterator __last1 = end();
	  const_iterator __first2 = __list.cbegin();
	  const_iterator __last2 = __list.cend();
	  while (__curr1 != __last1 && __first2 != __last2)
	    {
	      *__curr1 = *__first2;
	      ++__prev1;
	      ++__curr1;
	      ++__first2;
	    }
	  if (__first2 == __last2)
	    erase_after(__prev1, __last1);
	  else
	    insert_after(__prev1, __first2, __last2);
	}
      return *this;
    }

  template<typename _Tp, typename _Alloc>
    void
    forward_list<_Tp, _Alloc>::
    insert_after(const_iterator __pos,
		 size_type __n, const _Tp& __val)
    {
      _Fwd_list_node_base* __to
	= const_cast<_Fwd_list_node_base*>(__pos._M_node);
      _Fwd_list_node_base* __keep = __to->_M_next;
      for (size_type __i = 0; __i < __n; ++__i)
	{
	  __to->_M_next = _M_create_node(__val);
	  __to = __to->_M_next;
	}
      __to->_M_next = __keep;
    }

  template<typename _Tp, typename _Alloc>
    template<typename _InputIterator>
      void
      forward_list<_Tp, _Alloc>::
      insert_after(const_iterator __pos,
		   _InputIterator __first, _InputIterator __last)
      {
	_Fwd_list_node_base* __to
	  = const_cast<_Fwd_list_node_base*>(__pos._M_node);
	_Fwd_list_node_base* __keep = __to->_M_next;
	_InputIterator __curr = __first;
	while (__curr != __last)
	  {
	    __to->_M_next = _M_create_node(*__curr);
	    __to = __to->_M_next;
	    ++__curr;
	  }
	__to->_M_next = __keep;
      }

  template<typename _Tp, typename _Alloc>
    void
    forward_list<_Tp, _Alloc>::
    insert_after(const_iterator __pos, std::initializer_list<_Tp> __il)
    {
      _Fwd_list_node_base* __to
	= const_cast<_Fwd_list_node_base*>(__pos._M_node);
      _Fwd_list_node_base* __keep = __to->_M_next;
      const _Tp* __item = __il.begin();
      while (__item != __il.end())
	{
	  __to->_M_next = _M_create_node(*__item);
	  __to = __to->_M_next;
	  ++__item;
	}
      __to->_M_next = __keep;
    }

  template<typename _Tp, typename _Alloc>
    void
    forward_list<_Tp, _Alloc>::
    resize(size_type __sz, value_type __val)
    {
      iterator __k = before_begin();

      size_type __len = 0;
      while (__k._M_next() != end() && __len < __sz)
	{
	  ++__k;
	  ++__len;
	}
      if (__len == __sz)
	erase_after(__k, end());
      else
	insert_after(__k, __sz - __len, __val);
    }

  template<typename _Tp, typename _Alloc>
    void
    forward_list<_Tp, _Alloc>::
    remove(const _Tp& __val)
    {
      _Node* __curr = static_cast<_Node*>(&this->_M_impl._M_head);
      while (_Node* __temp = static_cast<_Node*>(__curr->_M_next))
	{
	  if (__temp->_M_value == __val)
	    _M_erase_after(__curr);
	  else
	    __curr = static_cast<_Node*>(__curr->_M_next);
	}
    }

  template<typename _Tp, typename _Alloc>
    template<typename _Pred>
      void
      forward_list<_Tp, _Alloc>::
      remove_if(_Pred __pred)
      {
	_Node* __curr = static_cast<_Node*>(&this->_M_impl._M_head);
	while (_Node* __temp = static_cast<_Node*>(__curr->_M_next))
	  {
	    if (__pred(__temp->_M_value))
	      _M_erase_after(__curr);
	    else
	      __curr = static_cast<_Node*>(__curr->_M_next);
	  }
      }

  template<typename _Tp, typename _Alloc>
    void
    forward_list<_Tp, _Alloc>::
    unique()
    {
      iterator __first = begin();
      iterator __last = end();
      if (__first == __last)
	return;
      iterator __next = __first;
      while (++__next != __last)
	{
	  if (*__first == *__next)
	    erase_after(__first);
	  else
	    __first = __next;
	  __next = __first;
	}
    }

  template<typename _Tp, typename _Alloc>
    template<typename _BinPred>
      void
      forward_list<_Tp, _Alloc>::
      unique(_BinPred __binary_pred)
      {
        iterator __first = begin();
        iterator __last = end();
        if (__first == __last)
	return;
        iterator __next = __first;
        while (++__next != __last)
	{
	  if (__binary_pred(*__first, *__next))
	    erase_after(__first);
	  else
	    __first = __next;
	  __next = __first;
	}
      }

  template<typename _Tp, typename _Alloc>
    template<typename _Comp>
      void
      forward_list<_Tp, _Alloc>::
      merge(forward_list&& __list, _Comp __comp)
      {
        _Fwd_list_node_base* __node = &this->_M_impl._M_head;
        while (__node->_M_next && __list._M_impl._M_head._M_next)
          {
            if (__comp(static_cast<_Node*>
		       (__list._M_impl._M_head._M_next)->_M_value,
                       static_cast<_Node*>
		       (__node->_M_next)->_M_value))
              __node->_M_transfer_after(&__list._M_impl._M_head,
                                        __list._M_impl._M_head._M_next);
            __node = __node->_M_next;
          }
        if (__list._M_impl._M_head._M_next)
          {
            __node->_M_next = __list._M_impl._M_head._M_next;
            __list._M_impl._M_head._M_next = 0;
          }
      }

_GLIBCXX_END_NAMESPACE // namespace std

#endif /* _FORWARD_LIST_TCC */

