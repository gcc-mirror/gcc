// RB tree implementation -*- C++ -*-

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*
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
 *
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
 */

/** @file bits/stl_tree.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{map,set}
 */

#ifndef _STL_TREE_H
#define _STL_TREE_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/stl_algobase.h>
#include <bits/allocator.h>
#include <bits/stl_function.h>
#include <bits/cpp_type_traits.h>
#include <bits/ptr_traits.h>
#include <ext/alloc_traits.h>
#if __cplusplus >= 201103L
# include <ext/aligned_buffer.h>
#endif
#ifdef __glibcxx_node_extract // >= C++17
# include <bits/node_handle.h>
#endif

#if __cplusplus < 201103L
# undef _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
# define _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE 0
#elif ! defined _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
# define _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE 1
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Red-black tree class, designed for use in implementing STL
  // associative containers (set, multiset, map, and multimap). The
  // insertion and deletion algorithms are based on those in Cormen,
  // Leiserson, and Rivest, Introduction to Algorithms (MIT Press,
  // 1990), except that
  //
  // (1) the header cell is maintained with links not only to the root
  // but also to the leftmost node of the tree, to enable constant
  // time begin(), and to the rightmost node of the tree, to enable
  // linear time performance when used with the generic set algorithms
  // (set_union, etc.)
  //
  // (2) when a node being deleted has two children its successor node
  // is relinked into its place, rather than copied, so that the only
  // iterators invalidated are those referring to the deleted node.

  enum _Rb_tree_color { _S_red = false, _S_black = true };

  struct _Rb_tree_node_base
  {
    typedef _Rb_tree_node_base* _Base_ptr;

    _Rb_tree_color	_M_color;
    _Base_ptr		_M_parent;
    _Base_ptr		_M_left;
    _Base_ptr		_M_right;

    static _Base_ptr
    _S_minimum(_Base_ptr __x) _GLIBCXX_NOEXCEPT
    {
      while (__x->_M_left != 0) __x = __x->_M_left;
      return __x;
    }

    static _Base_ptr
    _S_maximum(_Base_ptr __x) _GLIBCXX_NOEXCEPT
    {
      while (__x->_M_right != 0) __x = __x->_M_right;
      return __x;
    }

    // This is not const-correct, but it's only used in a const access path
    // by std::_Rb_tree::_M_end() where the pointer is used to initialize a
    // const_iterator and so constness is restored.
    _Base_ptr
    _M_base_ptr() const _GLIBCXX_NOEXCEPT
    { return const_cast<_Rb_tree_node_base*>(this); }
  };

  // Helper type offering value initialization guarantee on the compare functor.
  template<typename _Key_compare>
    struct _Rb_tree_key_compare
    {
      _Key_compare		_M_key_compare;

      _Rb_tree_key_compare()
      _GLIBCXX_NOEXCEPT_IF(
	is_nothrow_default_constructible<_Key_compare>::value)
      : _M_key_compare()
      { }

      _Rb_tree_key_compare(const _Key_compare& __comp)
      : _M_key_compare(__comp)
      { }

#if __cplusplus >= 201103L
      // Copy constructor added for consistency with C++98 mode.
      _Rb_tree_key_compare(const _Rb_tree_key_compare&) = default;

      _Rb_tree_key_compare(_Rb_tree_key_compare&& __x)
	noexcept(is_nothrow_copy_constructible<_Key_compare>::value)
      : _M_key_compare(__x._M_key_compare)
      { }
#endif
    };

  // Helper type to manage default initialization of node count and header.
  struct _Rb_tree_header
  {
    _Rb_tree_node_base	_M_header;
    size_t		_M_node_count; // Keeps track of size of tree.

    _Rb_tree_header() _GLIBCXX_NOEXCEPT
    {
      _M_header._M_color = _S_red;
      _M_reset();
    }

#if __cplusplus >= 201103L
    _Rb_tree_header(_Rb_tree_header&& __x) noexcept
    {
      if (__x._M_header._M_parent != nullptr)
	_M_move_data(__x);
      else
	{
	  _M_header._M_color = _S_red;
	  _M_reset();
	}
    }
#endif

    void
    _M_move_data(_Rb_tree_header& __from)
    {
      _M_header._M_color = __from._M_header._M_color;
      _M_header._M_parent = __from._M_header._M_parent;
      _M_header._M_left = __from._M_header._M_left;
      _M_header._M_right = __from._M_header._M_right;
      _M_header._M_parent->_M_parent = &_M_header;
      _M_node_count = __from._M_node_count;

      __from._M_reset();
    }

    void
    _M_reset()
    {
      _M_header._M_parent = 0;
      _M_header._M_left = &_M_header;
      _M_header._M_right = &_M_header;
      _M_node_count = 0;
    }
  };

  template<typename _Val>
    struct _Rb_tree_node : public _Rb_tree_node_base
    {
#if __cplusplus < 201103L
      _Val _M_value_field;

      _Val*
      _M_valptr()
      { return std::__addressof(_M_value_field); }

      const _Val*
      _M_valptr() const
      { return std::__addressof(_M_value_field); }
#else
      __gnu_cxx::__aligned_membuf<_Val> _M_storage;

      _Val*
      _M_valptr()
      { return _M_storage._M_ptr(); }

      const _Val*
      _M_valptr() const
      { return _M_storage._M_ptr(); }
#endif

      _Rb_tree_node*
      _M_node_ptr() _GLIBCXX_NOEXCEPT
      { return this; }
    };

#if _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
namespace __rb_tree
{
  template<typename _VoidPtr>
    struct _Node_base
    {
      using _Base_ptr = __ptr_rebind<_VoidPtr, _Node_base>;

      _Rb_tree_color	_M_color;
      _Base_ptr		_M_parent;
      _Base_ptr		_M_left;
      _Base_ptr		_M_right;

      static _Base_ptr
      _S_minimum(_Base_ptr __x) _GLIBCXX_NOEXCEPT
      {
	while (__x->_M_left) __x = __x->_M_left;
	return __x;
      }

      static _Base_ptr
      _S_maximum(_Base_ptr __x) _GLIBCXX_NOEXCEPT
      {
	while (__x->_M_right) __x = __x->_M_right;
	return __x;
      }

      // This is not const-correct, but it's only used in a const access path
      // by std::_Rb_tree::_M_end() where the pointer is used to initialize a
      // const_iterator and so constness is restored.
      _Base_ptr
      _M_base_ptr() const noexcept
      {
	return pointer_traits<_Base_ptr>::pointer_to
	  (*const_cast<_Node_base*>(this));
      }
    };

  // Helper type to manage default initialization of node count and header.
  template<typename _NodeBase>
    struct _Header
    {
    private:
      using _Base_ptr =  typename _NodeBase::_Base_ptr;

    public:
      _NodeBase		_M_header;
      size_t		_M_node_count; // Keeps track of size of tree.

      _Header() noexcept
      {
	_M_header._M_color = _S_red;
	_M_reset();
      }

      _Header(_Header&& __x) noexcept
      {
	if (__x._M_header._M_parent)
	  _M_move_data(__x);
	else
	  {
	    _M_header._M_color = _S_red;
	    _M_reset();
	  }
      }

      void
      _M_move_data(_Header& __from)
      {
	_M_header._M_color = __from._M_header._M_color;
	_M_header._M_parent = __from._M_header._M_parent;
	_M_header._M_left = __from._M_header._M_left;
	_M_header._M_right = __from._M_header._M_right;
	_M_header._M_parent->_M_parent = _M_header._M_base_ptr();
	_M_node_count = __from._M_node_count;

	__from._M_reset();
      }

      void
      _M_reset()
      {
	_M_header._M_parent = nullptr;
	_M_header._M_left = _M_header._M_right = _M_header._M_base_ptr();
	_M_node_count = 0;
      }
    };

  template<typename _ValPtr>
    struct _Node : public __rb_tree::_Node_base<__ptr_rebind<_ValPtr, void>>
    {
      using value_type = typename pointer_traits<_ValPtr>::element_type;
      using _Node_ptr = __ptr_rebind<_ValPtr, _Node>;

      _Node() noexcept { }
      ~_Node() { }
      _Node(_Node&&) = delete;

      union _Uninit_storage
      {
	_Uninit_storage() noexcept { }
	~_Uninit_storage() { }

	value_type _M_data;
      };
      _Uninit_storage _M_u;

      value_type*
      _M_valptr()
      { return std::addressof(_M_u._M_data); }

      value_type const*
      _M_valptr() const
      { return std::addressof(_M_u._M_data); }

      _Node_ptr
      _M_node_ptr() noexcept
      { return pointer_traits<_Node_ptr>::pointer_to(*this); }
    };
} // namespace __rb_tree
#endif // _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE

  _GLIBCXX_PURE _Rb_tree_node_base*
  _Rb_tree_increment(_Rb_tree_node_base* __x) throw ();

  _GLIBCXX_PURE _Rb_tree_node_base*
  _Rb_tree_decrement(_Rb_tree_node_base* __x) throw ();

  template<typename _Tp>
    struct _Rb_tree_iterator
    {
      typedef _Tp  value_type;
      typedef _Tp& reference;
      typedef _Tp* pointer;

      typedef bidirectional_iterator_tag iterator_category;
      typedef ptrdiff_t			 difference_type;

      typedef _Rb_tree_node_base::_Base_ptr	_Base_ptr;
      typedef _Rb_tree_node<_Tp>*		_Node_ptr;

      _Rb_tree_iterator() _GLIBCXX_NOEXCEPT
      : _M_node() { }

      explicit
      _Rb_tree_iterator(_Base_ptr __x) _GLIBCXX_NOEXCEPT
      : _M_node(__x) { }

      reference
      operator*() const _GLIBCXX_NOEXCEPT
      { return *static_cast<_Node_ptr>(_M_node)->_M_valptr(); }

      pointer
      operator->() const _GLIBCXX_NOEXCEPT
      { return static_cast<_Node_ptr>(_M_node)->_M_valptr(); }

      _Rb_tree_iterator&
      operator++() _GLIBCXX_NOEXCEPT
      {
	_M_node = _Rb_tree_increment(_M_node);
	return *this;
      }

      _Rb_tree_iterator
      operator++(int) _GLIBCXX_NOEXCEPT
      {
	_Rb_tree_iterator __tmp = *this;
	_M_node = _Rb_tree_increment(_M_node);
	return __tmp;
      }

      _Rb_tree_iterator&
      operator--() _GLIBCXX_NOEXCEPT
      {
	_M_node = _Rb_tree_decrement(_M_node);
	return *this;
      }

      _Rb_tree_iterator
      operator--(int) _GLIBCXX_NOEXCEPT
      {
	_Rb_tree_iterator __tmp = *this;
	_M_node = _Rb_tree_decrement(_M_node);
	return __tmp;
      }

      friend bool
      operator==(const _Rb_tree_iterator& __x,
		 const _Rb_tree_iterator& __y) _GLIBCXX_NOEXCEPT
      { return __x._M_node == __y._M_node; }

#if ! __cpp_lib_three_way_comparison
      friend bool
      operator!=(const _Rb_tree_iterator& __x,
		 const _Rb_tree_iterator& __y) _GLIBCXX_NOEXCEPT
      { return __x._M_node != __y._M_node; }
#endif

      _Base_ptr _M_node;
    };

  template<typename _Tp>
    struct _Rb_tree_const_iterator
    {
      typedef _Tp	 value_type;
      typedef const _Tp& reference;
      typedef const _Tp* pointer;

      typedef _Rb_tree_iterator<_Tp> iterator;

      typedef bidirectional_iterator_tag iterator_category;
      typedef ptrdiff_t			 difference_type;

      typedef _Rb_tree_node_base::_Base_ptr	_Base_ptr;
      typedef const _Rb_tree_node<_Tp>*		_Node_ptr;

      _Rb_tree_const_iterator() _GLIBCXX_NOEXCEPT
      : _M_node() { }

      explicit
      _Rb_tree_const_iterator(_Base_ptr __x) _GLIBCXX_NOEXCEPT
      : _M_node(__x) { }

      _Rb_tree_const_iterator(const iterator& __it) _GLIBCXX_NOEXCEPT
      : _M_node(__it._M_node) { }

      reference
      operator*() const _GLIBCXX_NOEXCEPT
      { return *static_cast<_Node_ptr>(_M_node)->_M_valptr(); }

      pointer
      operator->() const _GLIBCXX_NOEXCEPT
      { return static_cast<_Node_ptr>(_M_node)->_M_valptr(); }

      _Rb_tree_const_iterator&
      operator++() _GLIBCXX_NOEXCEPT
      {
	_M_node = _Rb_tree_increment(_M_node);
	return *this;
      }

      _Rb_tree_const_iterator
      operator++(int) _GLIBCXX_NOEXCEPT
      {
	_Rb_tree_const_iterator __tmp = *this;
	_M_node = _Rb_tree_increment(_M_node);
	return __tmp;
      }

      _Rb_tree_const_iterator&
      operator--() _GLIBCXX_NOEXCEPT
      {
	_M_node = _Rb_tree_decrement(_M_node);
	return *this;
      }

      _Rb_tree_const_iterator
      operator--(int) _GLIBCXX_NOEXCEPT
      {
	_Rb_tree_const_iterator __tmp = *this;
	_M_node = _Rb_tree_decrement(_M_node);
	return __tmp;
      }

      friend bool
      operator==(const _Rb_tree_const_iterator& __x,
		 const _Rb_tree_const_iterator& __y) _GLIBCXX_NOEXCEPT
      { return __x._M_node == __y._M_node; }

#if ! __cpp_lib_three_way_comparison
      friend bool
      operator!=(const _Rb_tree_const_iterator& __x,
		 const _Rb_tree_const_iterator& __y) _GLIBCXX_NOEXCEPT
      { return __x._M_node != __y._M_node; }
#endif

      _Base_ptr _M_node;
    };

  __attribute__((__nonnull__))
  void
  _Rb_tree_insert_and_rebalance(const bool __insert_left,
				_Rb_tree_node_base* __x,
				_Rb_tree_node_base* __p,
				_Rb_tree_node_base& __header) throw ();

  __attribute__((__nonnull__,__returns_nonnull__))
  _Rb_tree_node_base*
  _Rb_tree_rebalance_for_erase(_Rb_tree_node_base* const __z,
			       _Rb_tree_node_base& __header) throw ();

namespace __rb_tree
{
#if _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
  template<bool _Const, typename _ValPtr>
    struct _Iterator
    {
      template<typename _Tp>
	using __maybe_const = __conditional_t<_Const, const _Tp, _Tp>;

      using __ptr_traits =	pointer_traits<_ValPtr>;
      using value_type =	typename __ptr_traits::element_type;
      using reference =		__maybe_const<value_type>&;
      using pointer =		__maybe_const<value_type>*;

      using iterator_category =	bidirectional_iterator_tag;
      using difference_type =	ptrdiff_t;

      using _Node = __rb_tree::_Node<_ValPtr>;
      using _Node_base = __rb_tree::_Node_base<__ptr_rebind<_ValPtr, void>>;
      using _Base_ptr =	 typename _Node_base::_Base_ptr;

      _Iterator() noexcept
      : _M_node() { }

      constexpr explicit
      _Iterator(_Base_ptr __x) noexcept
      : _M_node(__x) { }

      _Iterator(const _Iterator&) = default;
      _Iterator& operator=(const _Iterator&) = default;

#ifdef __glibcxx_concepts
      constexpr
      _Iterator(const _Iterator<false, _ValPtr>& __it) requires _Const
#else
      template<bool _OtherConst,
	       typename = __enable_if_t<_Const && !_OtherConst>>
	constexpr
	_Iterator(const _Iterator<_OtherConst, _ValPtr>& __it)
#endif
	: _M_node(__it._M_node) { }

      [[__nodiscard__]]
      reference
      operator*() const noexcept
      { return *static_cast<_Node&>(*_M_node)._M_valptr(); }

      [[__nodiscard__]]
      pointer
      operator->() const noexcept
      { return static_cast<_Node&>(*_M_node)._M_valptr(); }

      _GLIBCXX14_CONSTEXPR _Iterator&
      operator++() noexcept
      {
	if (_M_node->_M_right)
	  {
	    _M_node = _M_node->_M_right;
	    while (_M_node->_M_left)
	      _M_node = _M_node->_M_left;
	  }
	else
	  {
	    _Base_ptr __y = _M_node->_M_parent;
	    while (_M_node == __y->_M_right)
	      {
		_M_node = __y;
		__y = __y->_M_parent;
	      }
	    if (_M_node->_M_right != __y)
	      _M_node = __y;
	  }

	return *this;
      }

      _GLIBCXX14_CONSTEXPR _Iterator
      operator++(int) noexcept
      {
	_Iterator __tmp(this->_M_node);
	++*this;
	return __tmp;
      }

      _GLIBCXX14_CONSTEXPR _Iterator&
      operator--() noexcept
      {
	if (_M_node->_M_color == _S_red
	    && _M_node->_M_parent->_M_parent == _M_node)
	  _M_node = _M_node->_M_right;
	else if (_M_node->_M_left)
	  {
	    _Base_ptr __y = _M_node->_M_left;
	    while (__y->_M_right)
	      __y = __y->_M_right;
	    _M_node = __y;
	  }
	else
	  {
	    _Base_ptr __y = _M_node->_M_parent;
	    while (_M_node == __y->_M_left)
	      {
		_M_node = __y;
		__y = __y->_M_parent;
	      }
	    _M_node = __y;
	  }
	return *this;
      }

      _GLIBCXX14_CONSTEXPR _Iterator
      operator--(int) noexcept
      {
	_Iterator __tmp(this->_M_node);
	--*this;
	return __tmp;
      }

      [[__nodiscard__]]
      friend bool
      operator==(const _Iterator& __x, const _Iterator& __y) _GLIBCXX_NOEXCEPT
      { return __x._M_node == __y._M_node; }

#if ! __cpp_lib_three_way_comparison
      [[__nodiscard__]]
      friend bool
      operator!=(const _Iterator& __x, const _Iterator& __y) _GLIBCXX_NOEXCEPT
      { return __x._M_node != __y._M_node; }
#endif

      _Base_ptr _M_node;
    };
#endif // USE_ALLOC_PTR_FOR_RB_TREE

  // Determine the node and iterator types used by std::_Rb_tree.
  template<typename _Val, typename _Ptr>
    struct _Node_traits;

#if _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE <= 9000
  // Specialization for the simple case where the allocator's pointer type
  // is the same type as value_type*.
  // For ABI compatibility we can't change the types used for this case.
  template<typename _Val>
    struct _Node_traits<_Val, _Val*>
    {
      typedef _Rb_tree_node<_Val>		_Node;
      typedef _Node*				_Node_ptr;
      typedef _Rb_tree_node_base		_Node_base;
      typedef _Node_base*			_Base_ptr;
      typedef _Rb_tree_header			_Header_t;
      typedef _Rb_tree_iterator<_Val>		_Iterator;
      typedef _Rb_tree_const_iterator<_Val>	_Const_iterator;

      __attribute__((__nonnull__))
      static void
      _S_insert_and_rebalance(const bool __insert_left,
			      _Node_base* __x, _Node_base* __p,
			      _Node_base& __header) _GLIBCXX_USE_NOEXCEPT
      {
	return _Rb_tree_insert_and_rebalance(__insert_left, __x, __p, __header);
      }

      __attribute__((__nonnull__,__returns_nonnull__))
      static _Node_base*
      _S_rebalance_for_erase(_Node_base* const __z,
			     _Node_base& __header) _GLIBCXX_USE_NOEXCEPT
      { return _Rb_tree_rebalance_for_erase(__z, __header); }
    };
#endif

#if ! _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
  // Always use the T* specialization.
  template<typename _Val, typename _Ptr>
    struct _Node_traits
    : _Node_traits<_Val, _Val*>
    { };
#else
  // Primary template used when the allocator uses fancy pointers.
  template<typename _Val, typename _ValPtr>
    struct _Node_traits
    {
      using _Node = __rb_tree::_Node<_ValPtr>;
      using _Node_ptr = __ptr_rebind<_ValPtr, _Node>;
      using _Node_base = __rb_tree::_Node_base<__ptr_rebind<_ValPtr, void>>;
      using _Base_ptr = __ptr_rebind<_ValPtr, _Node_base>;
      using _Header_t = __rb_tree::_Header<_Node_base>;
      using _Iterator = __rb_tree::_Iterator<false, _ValPtr>;
      using _Const_iterator = __rb_tree::_Iterator<true, _ValPtr>;

      static void
      _Rotate_left(_Base_ptr __x, _Base_ptr& __root)
      {
	const _Base_ptr __y = __x->_M_right;

	__x->_M_right = __y->_M_left;
	if (__y->_M_left)
	  __y->_M_left->_M_parent = __x;
	__y->_M_parent = __x->_M_parent;

	if (__x == __root)
	  __root = __y;
	else if (__x == __x->_M_parent->_M_left)
	  __x->_M_parent->_M_left = __y;
	else
	  __x->_M_parent->_M_right = __y;
	__y->_M_left = __x;
	__x->_M_parent = __y;
      }

      static void
      _Rotate_right(_Base_ptr __x, _Base_ptr& __root)
      {
	const _Base_ptr __y = __x->_M_left;

	__x->_M_left = __y->_M_right;
	if (__y->_M_right)
	  __y->_M_right->_M_parent = __x;
	__y->_M_parent = __x->_M_parent;

	if (__x == __root)
	  __root = __y;
	else if (__x == __x->_M_parent->_M_right)
	  __x->_M_parent->_M_right = __y;
	else
	  __x->_M_parent->_M_left = __y;
	__y->_M_right = __x;
	__x->_M_parent = __y;
      }

      static void
      _S_insert_and_rebalance(const bool __insert_left,
			      _Base_ptr __x, _Base_ptr __p,
			      _Node_base& __header)
      {
	_Base_ptr& __root = __header._M_parent;

	// Initialize fields in new node to insert.
	__x->_M_parent = __p;
	__x->_M_left = __x->_M_right = nullptr;
	__x->_M_color = _S_red;

	// Insert.
	// Make new node child of parent and maintain root, leftmost and
	// rightmost nodes.
	// N.B. First node is always inserted left.
	if (__insert_left)
	  {
	    __p->_M_left = __x; // also makes leftmost = __x when __p == &__header

	    if (std::__to_address(__p) == std::addressof(__header))
	      {
		__header._M_parent = __x;
		__header._M_right = __x;
	      }
	    else if (__p == __header._M_left)
	      __header._M_left = __x; // maintain leftmost pointing to min node
	  }
	else
	  {
	    __p->_M_right = __x;

	    if (__p == __header._M_right)
	      __header._M_right = __x; // maintain rightmost pointing to max node
	  }
	// Rebalance.
	while (__x != __root
	       && __x->_M_parent->_M_color == _S_red)
	  {
	    const _Base_ptr __xpp = __x->_M_parent->_M_parent;

	    if (__x->_M_parent == __xpp->_M_left)
	      {
		const _Base_ptr __y = __xpp->_M_right;
		if (__y && __y->_M_color == _S_red)
		  {
		    __x->_M_parent->_M_color = _S_black;
		    __y->_M_color = _S_black;
		    __xpp->_M_color = _S_red;
		    __x = __xpp;
		  }
		else
		  {
		    if (__x == __x->_M_parent->_M_right)
		      {
			__x = __x->_M_parent;
			_Rotate_left(__x, __root);
		      }
		    __x->_M_parent->_M_color = _S_black;
		    __xpp->_M_color = _S_red;
		    _Rotate_right(__xpp, __root);
		  }
	      }
	    else
	      {
		const _Base_ptr __y = __xpp->_M_left;
		if (__y && __y->_M_color == _S_red)
		  {
		    __x->_M_parent->_M_color = _S_black;
		    __y->_M_color = _S_black;
		    __xpp->_M_color = _S_red;
		    __x = __xpp;
		  }
		else
		  {
		    if (__x == __x->_M_parent->_M_left)
		      {
			__x = __x->_M_parent;
			_Rotate_right(__x, __root);
		      }
		    __x->_M_parent->_M_color = _S_black;
		    __xpp->_M_color = _S_red;
		    _Rotate_left(__xpp, __root);
		  }
	      }
	  }
	__root->_M_color = _S_black;
      }

      static _Base_ptr
      _S_rebalance_for_erase(_Base_ptr __z, _Node_base& __header)
      {
	_Base_ptr& __root = __header._M_parent;
	_Base_ptr& __leftmost = __header._M_left;
	_Base_ptr& __rightmost = __header._M_right;
	_Base_ptr __y = __z;
	_Base_ptr __x{};
	_Base_ptr __x_parent{};

	if (!__y->_M_left)     // __z has at most one non-null child. y == z.
	  __x = __y->_M_right;     // __x might be null.
	else
	  if (!__y->_M_right)  // __z has exactly one non-null child. y == z.
	    __x = __y->_M_left;    // __x is not null.
	  else
	    {
	      // __z has two non-null children.  Set __y to
	      __y = __y->_M_right;   //   __z's successor.  __x might be null.
	      while (__y->_M_left)
		__y = __y->_M_left;
	      __x = __y->_M_right;
	    }
	if (__y != __z)
	  {
	    // relink y in place of z.  y is z's successor
	    __z->_M_left->_M_parent = __y;
	    __y->_M_left = __z->_M_left;
	    if (__y != __z->_M_right)
	      {
		__x_parent = __y->_M_parent;
		if (__x)
		  __x->_M_parent = __y->_M_parent;
		__y->_M_parent->_M_left = __x;   // __y must be a child of _M_left
		__y->_M_right = __z->_M_right;
		__z->_M_right->_M_parent = __y;
	      }
	    else
	      __x_parent = __y;
	    if (__root == __z)
	      __root = __y;
	    else if (__z->_M_parent->_M_left == __z)
	      __z->_M_parent->_M_left = __y;
	    else
	      __z->_M_parent->_M_right = __y;
	    __y->_M_parent = __z->_M_parent;
	    std::swap(__y->_M_color, __z->_M_color);
	    __y = __z;
	    // __y now points to node to be actually deleted
	  }
	else
	  {                        // __y == __z
	    __x_parent = __y->_M_parent;
	    if (__x)
	      __x->_M_parent = __y->_M_parent;
	    if (__root == __z)
	      __root = __x;
	    else
	      if (__z->_M_parent->_M_left == __z)
		__z->_M_parent->_M_left = __x;
	      else
		__z->_M_parent->_M_right = __x;
	    if (__leftmost == __z)
	      {
		if (!__z->_M_right)        // __z->_M_left must be null also
		  __leftmost = __z->_M_parent;
		// makes __leftmost == _M_header if __z == __root
		else
		  __leftmost = _Node_base::_S_minimum(__x);
	      }
	    if (__rightmost == __z)
	      {
		if (__z->_M_left == 0)         // __z->_M_right must be null also
		  __rightmost = __z->_M_parent;
		// makes __rightmost == _M_header if __z == __root
		else                      // __x == __z->_M_left
		  __rightmost = _Node_base::_S_maximum(__x);
	      }
	  }
	if (__y->_M_color != _S_red)
	  {
	    while (__x != __root && (__x == 0 || __x->_M_color == _S_black))
	      if (__x == __x_parent->_M_left)
		{
		  _Base_ptr __w = __x_parent->_M_right;
		  if (__w->_M_color == _S_red)
		    {
		      __w->_M_color = _S_black;
		      __x_parent->_M_color = _S_red;
		      _Rotate_left(__x_parent, __root);
		      __w = __x_parent->_M_right;
		    }
		  if ((!__w->_M_left || __w->_M_left->_M_color == _S_black) &&
		      (!__w->_M_right || __w->_M_right->_M_color == _S_black))
		    {
		      __w->_M_color = _S_red;
		      __x = __x_parent;
		      __x_parent = __x_parent->_M_parent;
		    }
		  else
		    {
		      if (!__w->_M_right || __w->_M_right->_M_color == _S_black)
			{
			  __w->_M_left->_M_color = _S_black;
			  __w->_M_color = _S_red;
			  _Rotate_right(__w, __root);
			  __w = __x_parent->_M_right;
			}
		      __w->_M_color = __x_parent->_M_color;
		      __x_parent->_M_color = _S_black;
		      if (__w->_M_right)
			__w->_M_right->_M_color = _S_black;
		      _Rotate_left(__x_parent, __root);
		      break;
		    }
		}
	      else
		{
		  // same as above, with _M_right <-> _M_left.
		  _Base_ptr __w = __x_parent->_M_left;
		  if (__w->_M_color == _S_red)
		    {
		      __w->_M_color = _S_black;
		      __x_parent->_M_color = _S_red;
		      _Rotate_right(__x_parent, __root);
		      __w = __x_parent->_M_left;
		    }
		  if ((!__w->_M_right || __w->_M_right->_M_color == _S_black) &&
		      (!__w->_M_left || __w->_M_left->_M_color == _S_black))
		    {
		      __w->_M_color = _S_red;
		      __x = __x_parent;
		      __x_parent = __x_parent->_M_parent;
		    }
		  else
		    {
		      if (!__w->_M_left || __w->_M_left->_M_color == _S_black)
			{
			  __w->_M_right->_M_color = _S_black;
			  __w->_M_color = _S_red;
			  _Rotate_left(__w, __root);
			  __w = __x_parent->_M_left;
			}
		      __w->_M_color = __x_parent->_M_color;
		      __x_parent->_M_color = _S_black;
		      if (__w->_M_left)
			__w->_M_left->_M_color = _S_black;
		      _Rotate_right(__x_parent, __root);
		      break;
		    }
		}
	    if (__x)
	      __x->_M_color = _S_black;
	  }

	return __y;
      }
    };
#endif
} // namespace __rb_tree

#ifdef __glibcxx_node_extract // >= C++17
  template<typename _Tree1, typename _Cmp2>
    struct _Rb_tree_merge_helper { };
#endif

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc = allocator<_Val> >
    class _Rb_tree
    {
      typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template
	rebind<_Val>::other _Val_alloc_type;

      typedef __gnu_cxx::__alloc_traits<_Val_alloc_type> _Val_alloc_traits;
      typedef typename _Val_alloc_traits::pointer _ValPtr;
      typedef __rb_tree::_Node_traits<_Val, _ValPtr> _Node_traits;

      typedef typename _Node_traits::_Node_base		_Node_base;
      typedef typename _Node_traits::_Node		_Node;

      typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template
	rebind<_Node>::other _Node_allocator;

      typedef __gnu_cxx::__alloc_traits<_Node_allocator> _Node_alloc_traits;

    protected:
      typedef typename _Node_traits::_Base_ptr	_Base_ptr;
      typedef typename _Node_traits::_Node_ptr	_Node_ptr;

    private:
      // Functor recycling a pool of nodes and using allocation once the pool
      // is empty.
      struct _Reuse_or_alloc_node
      {
	_Reuse_or_alloc_node(_Rb_tree& __t)
	: _M_root(__t._M_root()), _M_nodes(__t._M_rightmost()), _M_t(__t)
	{
	  if (_M_root)
	    {
	      _M_root->_M_parent = _Base_ptr();

	      if (_M_nodes->_M_left)
		_M_nodes = _M_nodes->_M_left;
	    }
	  else
	    _M_nodes = _Base_ptr();
	}

#if __cplusplus >= 201103L
	_Reuse_or_alloc_node(const _Reuse_or_alloc_node&) = delete;
#endif

	~_Reuse_or_alloc_node()
	{
	  if (_M_root)
	    _M_t._M_erase(static_cast<_Node&>(*_M_root)._M_node_ptr());
	}

	template<typename _Arg>
	  _Node_ptr
	  operator()(_GLIBCXX_FWDREF(_Arg) __arg)
	  {
	    _Base_ptr __base = _M_extract();
	    if (__base)
	      {
		_Node_ptr __node = static_cast<_Node&>(*__base)._M_node_ptr();
		_M_t._M_destroy_node(__node);
		_M_t._M_construct_node(__node, _GLIBCXX_FORWARD(_Arg, __arg));
		return __node;
	      }

	    return _M_t._M_create_node(_GLIBCXX_FORWARD(_Arg, __arg));
	  }

      private:
	_Base_ptr
	_M_extract()
	{
	  if (!_M_nodes)
	    return _M_nodes;

	  _Base_ptr __node = _M_nodes;
	  _M_nodes = _M_nodes->_M_parent;
	  if (_M_nodes)
	    {
	      if (_M_nodes->_M_right == __node)
		{
		  _M_nodes->_M_right = _Base_ptr();

		  if (_M_nodes->_M_left)
		    {
		      _M_nodes = _M_nodes->_M_left;

		      while (_M_nodes->_M_right)
			_M_nodes = _M_nodes->_M_right;

		      if (_M_nodes->_M_left)
			_M_nodes = _M_nodes->_M_left;
		    }
		}
	      else // __node is on the left.
		_M_nodes->_M_left = _Base_ptr();
	    }
	  else
	    _M_root = _Base_ptr();

	  return __node;
	}

	_Base_ptr _M_root;
	_Base_ptr _M_nodes;
	_Rb_tree& _M_t;
      };

      // Functor similar to the previous one but without any pool of nodes to
      // recycle.
      struct _Alloc_node
      {
	_Alloc_node(_Rb_tree& __t)
	: _M_t(__t) { }

	template<typename _Arg>
	  _Node_ptr
	  operator()(_GLIBCXX_FWDREF(_Arg) __arg) const
	  { return _M_t._M_create_node(_GLIBCXX_FORWARD(_Arg, __arg)); }

      private:
	_Rb_tree& _M_t;
      };

    public:
      typedef _Key 				key_type;
      typedef _Val 				value_type;
      typedef value_type* 			pointer;
      typedef const value_type* 		const_pointer;
      typedef value_type& 			reference;
      typedef const value_type& 		const_reference;
      typedef size_t 				size_type;
      typedef ptrdiff_t 			difference_type;
      typedef _Alloc 				allocator_type;

      _Node_allocator&
      _M_get_Node_allocator() _GLIBCXX_NOEXCEPT
      { return this->_M_impl; }

      const _Node_allocator&
      _M_get_Node_allocator() const _GLIBCXX_NOEXCEPT
      { return this->_M_impl; }

      allocator_type
      get_allocator() const _GLIBCXX_NOEXCEPT
      { return allocator_type(_M_get_Node_allocator()); }

    protected:
      _Node_ptr
      _M_get_node()
      {
#if __cplusplus < 201102L || _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
	return _Node_alloc_traits::allocate(_M_get_Node_allocator(), 1);
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
	using __alloc_pointer = typename _Node_alloc_traits::pointer;
	if constexpr (is_same<_Node_ptr, __alloc_pointer>::value)
	  return _Node_alloc_traits::allocate(_M_get_Node_allocator(), 1);
	else
	  {
	    auto __ptr =
	      _Node_alloc_traits::allocate(_M_get_Node_allocator(), 1);
	    return std::__to_address(__ptr);
	  }
#pragma GCC diagnostic pop
#endif
      }

      void
      _M_put_node(_Node_ptr __p) _GLIBCXX_NOEXCEPT
      {
#if __cplusplus < 201102L || _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
	_Node_alloc_traits::deallocate(_M_get_Node_allocator(), __p, 1);
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
	using __alloc_pointer = typename _Node_alloc_traits::pointer;
	if constexpr (is_same<_Node_ptr, __alloc_pointer>::value)
	  _Node_alloc_traits::deallocate(_M_get_Node_allocator(), __p, 1);
	else
	  {
	    // When not using the allocator's pointer type internally we must
	    // convert __p to __alloc_pointer so it can be deallocated.
	    auto __ap = pointer_traits<__alloc_pointer>::pointer_to(*__p);
	    _Node_alloc_traits::deallocate(_M_get_Node_allocator(), __ap, 1);
	  }
#pragma GCC diagnostic pop
#endif
      }

#if __cplusplus < 201103L
      void
      _M_construct_node(_Node_ptr __node, const value_type& __x)
      {
	__try
	  { get_allocator().construct(__node->_M_valptr(), __x); }
	__catch(...)
	  {
	    _M_put_node(__node);
	    __throw_exception_again;
	  }
      }

      _Node_ptr
      _M_create_node(const value_type& __x)
      {
	_Node_ptr __tmp = _M_get_node();
	_M_construct_node(__tmp, __x);
	return __tmp;
      }
#else
      template<typename... _Args>
	void
	_M_construct_node(_Node_ptr __node, _Args&&... __args)
	{
	  __try
	    {
	      ::new(std::addressof(*__node)) _Node;
	      _Node_alloc_traits::construct(_M_get_Node_allocator(),
					    __node->_M_valptr(),
					    std::forward<_Args>(__args)...);
	    }
	  __catch(...)
	    {
	      __node->~_Node();
	      _M_put_node(__node);
	      __throw_exception_again;
	    }
	}

      template<typename... _Args>
	_Node_ptr
	_M_create_node(_Args&&... __args)
	{
	  _Node_ptr __tmp = _M_get_node();
	  _M_construct_node(__tmp, std::forward<_Args>(__args)...);
	  return __tmp;
	}
#endif

      void
      _M_destroy_node(_Node_ptr __p) _GLIBCXX_NOEXCEPT
      {
#if __cplusplus < 201103L
	get_allocator().destroy(__p->_M_valptr());
#else
	_Node_alloc_traits::destroy(_M_get_Node_allocator(), __p->_M_valptr());
	__p->~_Node();
#endif
      }

      void
      _M_drop_node(_Node_ptr __p) _GLIBCXX_NOEXCEPT
      {
	_M_destroy_node(__p);
	_M_put_node(__p);
      }

      template<bool _MoveValue, typename _NodeGen>
	_Node_ptr
	_M_clone_node(_Node_ptr __x, _NodeGen& __node_gen)
	{
#if __cplusplus >= 201103L
	  using _Vp = __conditional_t<_MoveValue,
				      value_type&&,
				      const value_type&>;
#endif
	  _Node_ptr __tmp
	    = __node_gen(_GLIBCXX_FORWARD(_Vp, *__x->_M_valptr()));
	  __tmp->_M_color = __x->_M_color;
	  __tmp->_M_left = __tmp->_M_right = _Base_ptr();
	  return __tmp;
	}

    protected:
      typedef typename _Node_traits::_Header_t		_Header_t;

#if _GLIBCXX_INLINE_VERSION
      template<typename _Key_compare>
#else
      // Unused _Is_pod_comparator is kept as it is part of mangled name.
      template<typename _Key_compare,
	       bool /* _Is_pod_comparator */ = __is_pod(_Key_compare)>
#endif
	struct _Rb_tree_impl
	: public _Node_allocator
	, public _Rb_tree_key_compare<_Key_compare>
	, public _Header_t
	{
	  typedef _Rb_tree_key_compare<_Key_compare> _Base_key_compare;

	  _Rb_tree_impl()
	    _GLIBCXX_NOEXCEPT_IF(
		is_nothrow_default_constructible<_Node_allocator>::value
		&& is_nothrow_default_constructible<_Base_key_compare>::value )
	  : _Node_allocator()
	  { }

	  _Rb_tree_impl(const _Rb_tree_impl& __x)
	  : _Node_allocator(_Node_alloc_traits::_S_select_on_copy(__x))
	  , _Base_key_compare(__x._M_key_compare)
	  , _Header_t()
	  { }

#if __cplusplus < 201103L
	  _Rb_tree_impl(const _Key_compare& __comp, const _Node_allocator& __a)
	  : _Node_allocator(__a), _Base_key_compare(__comp)
	  { }
#else
	  _Rb_tree_impl(_Rb_tree_impl&&)
	    noexcept( is_nothrow_move_constructible<_Base_key_compare>::value )
	  = default;

	  explicit
	  _Rb_tree_impl(_Node_allocator&& __a)
	  : _Node_allocator(std::move(__a))
	  { }

	  _Rb_tree_impl(_Rb_tree_impl&& __x, _Node_allocator&& __a)
	  : _Node_allocator(std::move(__a)),
	    _Base_key_compare(std::move(__x)),
	    _Header_t(std::move(__x))
	  { }

	  _Rb_tree_impl(const _Key_compare& __comp, _Node_allocator&& __a)
	  : _Node_allocator(std::move(__a)), _Base_key_compare(__comp)
	  { }
#endif
	};

      _Rb_tree_impl<_Compare> _M_impl;

    protected:
      _Base_ptr&
      _M_root() _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_parent; }

      _Base_ptr
      _M_root() const _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_parent; }

      _Base_ptr&
      _M_leftmost() _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_left; }

      _Base_ptr
      _M_leftmost() const _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_left; }

      _Base_ptr&
      _M_rightmost() _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_right; }

      _Base_ptr
      _M_rightmost() const _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_right; }

      _Base_ptr
      _M_begin() const _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_parent; }

      _Node_ptr
      _M_begin_node() const _GLIBCXX_NOEXCEPT
      {
	_Base_ptr __begin = this->_M_impl._M_header._M_parent;
	return __begin
	  ? static_cast<_Node&>(*__begin)._M_node_ptr()
	  : _Node_ptr();
      }

      _Base_ptr
      _M_end() const _GLIBCXX_NOEXCEPT
      { return this->_M_impl._M_header._M_base_ptr(); }

      static const _Key&
      _S_key(const _Node& __node)
      {
#if __cplusplus >= 201103L
	// If we're asking for the key we're presumably using the comparison
	// object, and so this is a good place to sanity check it.
	static_assert(__is_invocable<_Compare&, const _Key&, const _Key&>{},
		      "comparison object must be invocable "
		      "with two arguments of key type");
# if __cplusplus >= 201703L
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 2542. Missing const requirements for associative containers
	if constexpr (__is_invocable<_Compare&, const _Key&, const _Key&>{})
	  static_assert(
	      is_invocable_v<const _Compare&, const _Key&, const _Key&>,
	      "comparison object must be invocable as const");
# endif // C++17
#endif // C++11

	return _KeyOfValue()(*__node._M_valptr());
      }

      static const _Key&
      _S_key(_Base_ptr __x)
      { return _S_key(static_cast<const _Node&>(*__x)); }

      static const _Key&
      _S_key(_Node_ptr __x)
      { return _S_key(*__x); }

      static _Base_ptr
      _S_left(_Base_ptr __x) _GLIBCXX_NOEXCEPT
      { return __x->_M_left; }

      static _Node_ptr
      _S_left(_Node_ptr __x)
      {
	return __x->_M_left
	  ? static_cast<_Node&>(*__x->_M_left)._M_node_ptr()
	  : _Node_ptr();
      }

      static _Base_ptr
      _S_right(_Base_ptr __x) _GLIBCXX_NOEXCEPT
      { return __x->_M_right; }

      static _Node_ptr
      _S_right(_Node_ptr __x) _GLIBCXX_NOEXCEPT
      {
	return __x->_M_right
	  ? static_cast<_Node&>(*__x->_M_right)._M_node_ptr()
	  : _Node_ptr();
      }

    public:
      typedef typename _Node_traits::_Iterator		iterator;
      typedef typename _Node_traits::_Const_iterator	const_iterator;

      typedef std::reverse_iterator<iterator>       reverse_iterator;
      typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

#ifdef __glibcxx_node_extract // >= C++17
      using node_type = _Node_handle<_Key, _Val, _Node_allocator>;
      using insert_return_type = _Node_insert_return<
	__conditional_t<is_same_v<_Key, _Val>, const_iterator, iterator>,
	node_type>;
#endif

      pair<_Base_ptr, _Base_ptr>
      _M_get_insert_unique_pos(const key_type& __k);

      pair<_Base_ptr, _Base_ptr>
      _M_get_insert_equal_pos(const key_type& __k);

      pair<_Base_ptr, _Base_ptr>
      _M_get_insert_hint_unique_pos(const_iterator __pos,
				    const key_type& __k);

      pair<_Base_ptr, _Base_ptr>
      _M_get_insert_hint_equal_pos(const_iterator __pos,
				   const key_type& __k);

    private:
#if __cplusplus >= 201103L
      template<typename _Arg, typename _NodeGen>
	iterator
	_M_insert_(_Base_ptr __x, _Base_ptr __y, _Arg&& __v, _NodeGen&);

      iterator
      _M_insert_node(_Base_ptr __x, _Base_ptr __y, _Node_ptr __z);

      template<typename _Arg>
	iterator
	_M_insert_lower(_Base_ptr __y, _Arg&& __v);

      template<typename _Arg>
	iterator
	_M_insert_equal_lower(_Arg&& __x);

      iterator
      _M_insert_lower_node(_Base_ptr __p, _Node_ptr __z);

      iterator
      _M_insert_equal_lower_node(_Node_ptr __z);
#else
      template<typename _NodeGen>
	iterator
	_M_insert_(_Base_ptr __x, _Base_ptr __y,
		   const value_type& __v, _NodeGen&);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 233. Insertion hints in associative containers.
      iterator
      _M_insert_lower(_Base_ptr __y, const value_type& __v);

      iterator
      _M_insert_equal_lower(const value_type& __x);
#endif

      enum { __as_lvalue, __as_rvalue };

      template<bool _MoveValues, typename _NodeGen>
	_Base_ptr
	_M_copy(_Node_ptr, _Base_ptr, _NodeGen&);

      template<bool _MoveValues, typename _NodeGen>
	_Base_ptr
	_M_copy(const _Rb_tree& __x, _NodeGen& __gen)
	{
	  _Base_ptr __root =
	    _M_copy<_MoveValues>(__x._M_begin_node(), _M_end(), __gen);
	  _M_leftmost() = _Node_base::_S_minimum(__root);
	  _M_rightmost() = _Node_base::_S_maximum(__root);
	  _M_impl._M_node_count = __x._M_impl._M_node_count;
	  return __root;
	}

      _Base_ptr
      _M_copy(const _Rb_tree& __x)
      {
	_Alloc_node __an(*this);
	return _M_copy<__as_lvalue>(__x, __an);
      }

      void
      _M_erase(_Node_ptr __x);

      _Base_ptr
      _M_lower_bound(_Base_ptr __x, _Base_ptr __y,
		     const _Key& __k) const;

      _Base_ptr
      _M_upper_bound(_Base_ptr __x, _Base_ptr __y,
		     const _Key& __k) const;

    public:
      // allocation/deallocation
#if __cplusplus < 201103L
      _Rb_tree() { }
#else
      _Rb_tree() = default;
#endif

      _Rb_tree(const _Compare& __comp,
	       const allocator_type& __a = allocator_type())
      : _M_impl(__comp, _Node_allocator(__a)) { }

      _Rb_tree(const _Rb_tree& __x)
      : _M_impl(__x._M_impl)
      {
	if (__x._M_root())
	  _M_root() = _M_copy(__x);
      }

#if __cplusplus >= 201103L
      _Rb_tree(const allocator_type& __a)
      : _M_impl(_Node_allocator(__a))
      { }

      _Rb_tree(const _Rb_tree& __x, const allocator_type& __a)
      : _M_impl(__x._M_impl._M_key_compare, _Node_allocator(__a))
      {
	if (__x._M_root())
	  _M_root() = _M_copy(__x);
      }

      _Rb_tree(_Rb_tree&&) = default;

      _Rb_tree(_Rb_tree&& __x, const allocator_type& __a)
      : _Rb_tree(std::move(__x), _Node_allocator(__a))
      { }

    private:
      _Rb_tree(_Rb_tree&& __x, _Node_allocator&& __a, true_type)
      noexcept(is_nothrow_default_constructible<_Compare>::value)
      : _M_impl(std::move(__x._M_impl), std::move(__a))
      { }

      _Rb_tree(_Rb_tree&& __x, _Node_allocator&& __a, false_type)
      : _M_impl(__x._M_impl._M_key_compare, std::move(__a))
      {
	if (__x._M_root())
	  _M_move_data(__x, false_type{});
      }

    public:
      _Rb_tree(_Rb_tree&& __x, _Node_allocator&& __a)
      noexcept( noexcept(
	_Rb_tree(std::declval<_Rb_tree&&>(), std::declval<_Node_allocator&&>(),
		 std::declval<typename _Node_alloc_traits::is_always_equal>())) )
      : _Rb_tree(std::move(__x), std::move(__a),
		 typename _Node_alloc_traits::is_always_equal{})
      { }
#endif

      ~_Rb_tree() _GLIBCXX_NOEXCEPT
      { _M_erase(_M_begin_node()); }

      _Rb_tree&
      operator=(const _Rb_tree& __x);

      // Accessors.
      _Compare
      key_comp() const
      { return _M_impl._M_key_compare; }

      iterator
      begin() _GLIBCXX_NOEXCEPT
      { return iterator(this->_M_impl._M_header._M_left); }

      const_iterator
      begin() const _GLIBCXX_NOEXCEPT
      { return const_iterator(this->_M_impl._M_header._M_left); }

      iterator
      end() _GLIBCXX_NOEXCEPT
      { return iterator(_M_end()); }

      const_iterator
      end() const _GLIBCXX_NOEXCEPT
      { return const_iterator(_M_end()); }

      reverse_iterator
      rbegin() _GLIBCXX_NOEXCEPT
      { return reverse_iterator(end()); }

      const_reverse_iterator
      rbegin() const _GLIBCXX_NOEXCEPT
      { return const_reverse_iterator(end()); }

      reverse_iterator
      rend() _GLIBCXX_NOEXCEPT
      { return reverse_iterator(begin()); }

      const_reverse_iterator
      rend() const _GLIBCXX_NOEXCEPT
      { return const_reverse_iterator(begin()); }

      _GLIBCXX_NODISCARD bool
      empty() const _GLIBCXX_NOEXCEPT
      { return _M_impl._M_node_count == 0; }

      size_type
      size() const _GLIBCXX_NOEXCEPT
      { return _M_impl._M_node_count; }

      size_type
      max_size() const _GLIBCXX_NOEXCEPT
      { return _Node_alloc_traits::max_size(_M_get_Node_allocator()); }

      void
      swap(_Rb_tree& __t)
      _GLIBCXX_NOEXCEPT_IF(__is_nothrow_swappable<_Compare>::value);

      // Insert/erase.
#if __cplusplus >= 201103L
      template<typename _Arg>
	pair<iterator, bool>
	_M_insert_unique(_Arg&& __x);

      template<typename _Arg>
	iterator
	_M_insert_equal(_Arg&& __x);

      template<typename _Arg, typename _NodeGen>
	iterator
	_M_insert_unique_(const_iterator __pos, _Arg&& __x, _NodeGen&);

      template<typename _Arg>
	iterator
	_M_insert_unique_(const_iterator __pos, _Arg&& __x)
	{
	  _Alloc_node __an(*this);
	  return _M_insert_unique_(__pos, std::forward<_Arg>(__x), __an);
	}

      template<typename _Arg, typename _NodeGen>
	iterator
	_M_insert_equal_(const_iterator __pos, _Arg&& __x, _NodeGen&);

      template<typename _Arg>
	iterator
	_M_insert_equal_(const_iterator __pos, _Arg&& __x)
	{
	  _Alloc_node __an(*this);
	  return _M_insert_equal_(__pos, std::forward<_Arg>(__x), __an);
	}

      template<typename... _Args>
	pair<iterator, bool>
	_M_emplace_unique(_Args&&... __args);

      template<typename... _Args>
	iterator
	_M_emplace_equal(_Args&&... __args);

      template<typename... _Args>
	iterator
	_M_emplace_hint_unique(const_iterator __pos, _Args&&... __args);

      template<typename... _Args>
	iterator
	_M_emplace_hint_equal(const_iterator __pos, _Args&&... __args);

      template<typename _Iter>
	using __same_value_type
	  = is_same<value_type, typename iterator_traits<_Iter>::value_type>;

      template<typename _InputIterator>
	__enable_if_t<__same_value_type<_InputIterator>::value>
	_M_insert_range_unique(_InputIterator __first, _InputIterator __last)
	{
	  _Alloc_node __an(*this);
	  for (; __first != __last; ++__first)
	    _M_insert_unique_(end(), *__first, __an);
	}

      template<typename _InputIterator>
	__enable_if_t<!__same_value_type<_InputIterator>::value>
	_M_insert_range_unique(_InputIterator __first, _InputIterator __last)
	{
	  for (; __first != __last; ++__first)
	    _M_emplace_unique(*__first);
	}

      template<typename _InputIterator>
	__enable_if_t<__same_value_type<_InputIterator>::value>
	_M_insert_range_equal(_InputIterator __first, _InputIterator __last)
	{
	  _Alloc_node __an(*this);
	  for (; __first != __last; ++__first)
	    _M_insert_equal_(end(), *__first, __an);
	}

      template<typename _InputIterator>
	__enable_if_t<!__same_value_type<_InputIterator>::value>
	_M_insert_range_equal(_InputIterator __first, _InputIterator __last)
	{
	  for (; __first != __last; ++__first)
	    _M_emplace_equal(*__first);
	}
#else
      pair<iterator, bool>
      _M_insert_unique(const value_type& __x);

      iterator
      _M_insert_equal(const value_type& __x);

      template<typename _NodeGen>
	iterator
	_M_insert_unique_(const_iterator __pos, const value_type& __x,
			  _NodeGen&);

      iterator
      _M_insert_unique_(const_iterator __pos, const value_type& __x)
      {
	_Alloc_node __an(*this);
	return _M_insert_unique_(__pos, __x, __an);
      }

      template<typename _NodeGen>
	iterator
	_M_insert_equal_(const_iterator __pos, const value_type& __x,
			 _NodeGen&);
      iterator
      _M_insert_equal_(const_iterator __pos, const value_type& __x)
      {
	_Alloc_node __an(*this);
	return _M_insert_equal_(__pos, __x, __an);
      }

      template<typename _InputIterator>
	void
	_M_insert_range_unique(_InputIterator __first, _InputIterator __last)
	{
	  _Alloc_node __an(*this);
	  for (; __first != __last; ++__first)
	    _M_insert_unique_(end(), *__first, __an);
	}

      template<typename _InputIterator>
	void
	_M_insert_range_equal(_InputIterator __first, _InputIterator __last)
	{
	  _Alloc_node __an(*this);
	  for (; __first != __last; ++__first)
	    _M_insert_equal_(end(), *__first, __an);
	}
#endif

    private:
      void
      _M_erase_aux(const_iterator __position);

      void
      _M_erase_aux(const_iterator __first, const_iterator __last);

    public:
#if __cplusplus >= 201103L
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // DR 130. Associative erase should return an iterator.
      _GLIBCXX_ABI_TAG_CXX11
      iterator
      erase(const_iterator __position)
      {
	__glibcxx_assert(__position != end());
	const_iterator __result = __position;
	++__result;
	_M_erase_aux(__position);
	return iterator(__result._M_node);
      }

      // LWG 2059.
      _GLIBCXX_ABI_TAG_CXX11
      iterator
      erase(iterator __position)
      {
	__glibcxx_assert(__position != end());
	iterator __result = __position;
	++__result;
	_M_erase_aux(__position);
	return __result;
      }
#else
      void
      erase(iterator __position)
      {
	__glibcxx_assert(__position != end());
	_M_erase_aux(__position);
      }

      void
      erase(const_iterator __position)
      {
	__glibcxx_assert(__position != end());
	_M_erase_aux(__position);
      }
#endif

      size_type
      erase(const key_type& __x);

#if __cplusplus >= 201103L
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // DR 130. Associative erase should return an iterator.
      _GLIBCXX_ABI_TAG_CXX11
      iterator
      erase(const_iterator __first, const_iterator __last)
      {
	_M_erase_aux(__first, __last);
	return iterator(__last._M_node);
      }
#else
      void
      erase(iterator __first, iterator __last)
      { _M_erase_aux(__first, __last); }

      void
      erase(const_iterator __first, const_iterator __last)
      { _M_erase_aux(__first, __last); }
#endif

      void
      clear() _GLIBCXX_NOEXCEPT
      {
	_M_erase(_M_begin_node());
	_M_impl._M_reset();
      }

      // Set operations.
      iterator
      find(const key_type& __k);

      const_iterator
      find(const key_type& __k) const;

      size_type
      count(const key_type& __k) const;

      iterator
      lower_bound(const key_type& __k)
      { return iterator(_M_lower_bound(_M_begin(), _M_end(), __k)); }

      const_iterator
      lower_bound(const key_type& __k) const
      {
	return const_iterator
	  (_M_lower_bound(_M_begin(), _M_end(), __k));
      }

      iterator
      upper_bound(const key_type& __k)
      { return iterator(_M_upper_bound(_M_begin(), _M_end(), __k)); }

      const_iterator
      upper_bound(const key_type& __k) const
      {
	return const_iterator
	  (_M_upper_bound(_M_begin(), _M_end(), __k));
      }

      pair<iterator, iterator>
      equal_range(const key_type& __k);

      pair<const_iterator, const_iterator>
      equal_range(const key_type& __k) const;

#if __cplusplus >= 201402L
      template<typename _Kt,
	       typename _Req = __has_is_transparent_t<_Compare, _Kt>>
	iterator
	_M_find_tr(const _Kt& __k)
	{
	  const _Rb_tree* __const_this = this;
	  return iterator(__const_this->_M_find_tr(__k)._M_node);
	}

      template<typename _Kt,
	       typename _Req = __has_is_transparent_t<_Compare, _Kt>>
	const_iterator
	_M_find_tr(const _Kt& __k) const
	{
	  const_iterator __j(_M_lower_bound_tr(__k));
	  if (__j != end() && _M_impl._M_key_compare(__k, _S_key(__j._M_node)))
	    __j = end();
	  return __j;
	}

      template<typename _Kt,
	       typename _Req = __has_is_transparent_t<_Compare, _Kt>>
	size_type
	_M_count_tr(const _Kt& __k) const
	{
	  auto __p = _M_equal_range_tr(__k);
	  return std::distance(__p.first, __p.second);
	}

      template<typename _Kt,
	       typename _Req = __has_is_transparent_t<_Compare, _Kt>>
	_Base_ptr
	_M_lower_bound_tr(const _Kt& __k) const
	{
	  auto __x = _M_begin();
	  auto __y = _M_end();
	  while (__x)
	    if (!_M_impl._M_key_compare(_S_key(__x), __k))
	      {
		__y = __x;
		__x = _S_left(__x);
	      }
	    else
	      __x = _S_right(__x);
	  return __y;
	}

      template<typename _Kt,
	       typename _Req = __has_is_transparent_t<_Compare, _Kt>>
	_Base_ptr
	_M_upper_bound_tr(const _Kt& __k) const
	{
	  auto __x = _M_begin();
	  auto __y = _M_end();
	  while (__x)
	    if (_M_impl._M_key_compare(__k, _S_key(__x)))
	      {
		__y = __x;
		__x = _S_left(__x);
	      }
	    else
	      __x = _S_right(__x);
	  return __y;
	}

      template<typename _Kt,
	       typename _Req = __has_is_transparent_t<_Compare, _Kt>>
	pair<iterator, iterator>
	_M_equal_range_tr(const _Kt& __k)
	{
	  const _Rb_tree* __const_this = this;
	  auto __ret = __const_this->_M_equal_range_tr(__k);
	  return
	    { iterator(__ret.first._M_node), iterator(__ret.second._M_node) };
	}

      template<typename _Kt,
	       typename _Req = __has_is_transparent_t<_Compare, _Kt>>
	pair<const_iterator, const_iterator>
	_M_equal_range_tr(const _Kt& __k) const
	{
	  const_iterator __low(_M_lower_bound_tr(__k));
	  auto __high = __low;
	  auto& __cmp = _M_impl._M_key_compare;
	  while (__high != end() && !__cmp(__k, _S_key(__high._M_node)))
	    ++__high;
	  return { __low, __high };
	}
#endif

      // Debugging.
      bool
      __rb_verify() const;

#if __cplusplus >= 201103L
      _Rb_tree&
      operator=(_Rb_tree&&)
      noexcept(_Node_alloc_traits::_S_nothrow_move()
	       && is_nothrow_move_assignable<_Compare>::value);

      template<typename _Iterator>
	void
	_M_assign_unique(_Iterator, _Iterator);

      template<typename _Iterator>
	void
	_M_assign_equal(_Iterator, _Iterator);

    private:
      // Move elements from container with equal allocator.
      void
      _M_move_data(_Rb_tree& __x, true_type)
      { _M_impl._M_move_data(__x._M_impl); }

      // Move elements from container with possibly non-equal allocator,
      // which might result in a copy not a move.
      void
      _M_move_data(_Rb_tree&, false_type);

      // Move assignment from container with equal allocator.
      void
      _M_move_assign(_Rb_tree&, true_type);

      // Move assignment from container with possibly non-equal allocator,
      // which might result in a copy not a move.
      void
      _M_move_assign(_Rb_tree&, false_type);
#endif

#if __glibcxx_node_extract // >= C++17
      static _Node_ptr
      _S_adapt(typename _Node_alloc_traits::pointer __ptr)
      {
#if _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
	return __ptr;
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
	using __alloc_ptr = typename _Node_alloc_traits::pointer;
	if constexpr (is_same<_Node_ptr, __alloc_ptr>::value)
	  return __ptr;
	else
	  return std::__to_address(__ptr);
#pragma GCC diagnostic pop
#endif
      }

    public:
      /// Re-insert an extracted node.
      insert_return_type
      _M_reinsert_node_unique(node_type&& __nh)
      {
	insert_return_type __ret;
	if (__nh.empty())
	  __ret.position = end();
	else
	  {
	    __glibcxx_assert(_M_get_Node_allocator() == *__nh._M_alloc);

	    auto __res = _M_get_insert_unique_pos(__nh._M_key());
	    if (__res.second)
	      {
		__ret.position
		  = _M_insert_node(__res.first, __res.second,
				   _S_adapt(__nh._M_ptr));
		__nh.release();
		__ret.inserted = true;
	      }
	    else
	      {
		__ret.node = std::move(__nh);
		__ret.position = iterator(__res.first);
		__ret.inserted = false;
	      }
	  }
	return __ret;
      }

      /// Re-insert an extracted node.
      iterator
      _M_reinsert_node_equal(node_type&& __nh)
      {
	iterator __ret;
	if (__nh.empty())
	  __ret = end();
	else
	  {
	    __glibcxx_assert(_M_get_Node_allocator() == *__nh._M_alloc);
	    auto __res = _M_get_insert_equal_pos(__nh._M_key());
	    if (__res.second)
	      __ret = _M_insert_node(__res.first, __res.second,
				     _S_adapt(__nh._M_ptr));
	    else
	      __ret = _M_insert_equal_lower_node(_S_adapt(__nh._M_ptr));
	    __nh.release();
	  }
	return __ret;
      }

      /// Re-insert an extracted node.
      iterator
      _M_reinsert_node_hint_unique(const_iterator __hint, node_type&& __nh)
      {
	iterator __ret;
	if (__nh.empty())
	  __ret = end();
	else
	  {
	    __glibcxx_assert(_M_get_Node_allocator() == *__nh._M_alloc);
	    auto __res = _M_get_insert_hint_unique_pos(__hint, __nh._M_key());
	    if (__res.second)
	      {
		__ret = _M_insert_node(__res.first, __res.second,
				       _S_adapt(__nh._M_ptr));
		__nh.release();
	      }
	    else
	      __ret = iterator(__res.first);
	  }
	return __ret;
      }

      /// Re-insert an extracted node.
      iterator
      _M_reinsert_node_hint_equal(const_iterator __hint, node_type&& __nh)
      {
	iterator __ret;
	if (__nh.empty())
	  __ret = end();
	else
	  {
	    __glibcxx_assert(_M_get_Node_allocator() == *__nh._M_alloc);
	    auto __res = _M_get_insert_hint_equal_pos(__hint, __nh._M_key());
	    if (__res.second)
	      __ret = _M_insert_node(__res.first, __res.second,
				     _S_adapt(__nh._M_ptr));
	    else
	      __ret = _M_insert_equal_lower_node(_S_adapt(__nh._M_ptr));
	    __nh.release();
	  }
	return __ret;
      }

      /// Extract a node.
      node_type
      extract(const_iterator __pos)
      {
	auto __ptr = _Node_traits::_S_rebalance_for_erase
	  (__pos._M_node, _M_impl._M_header);
	--_M_impl._M_node_count;
	auto __node_ptr = static_cast<_Node&>(*__ptr)._M_node_ptr();
#if _GLIBCXX_USE_ALLOC_PTR_FOR_RB_TREE
	return { __node_ptr, _M_get_Node_allocator() };
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
	using __alloc_ptr = typename _Node_alloc_traits::pointer;
	if constexpr (is_same<_Node_ptr, __alloc_ptr>::value)
	  return { __node_ptr, _M_get_Node_allocator() };
	else
	  {
	    auto __ap = pointer_traits<__alloc_ptr>::pointer_to(*__node_ptr);
	    return { __ap, _M_get_Node_allocator() };
	  }
#pragma GCC diagnostic pop
#endif
      }

      /// Extract a node.
      node_type
      extract(const key_type& __k)
      {
	node_type __nh;
	auto __pos = find(__k);
	if (__pos != end())
	  __nh = extract(const_iterator(__pos));
	return __nh;
      }

      template<typename _Compare2>
	using _Compatible_tree
	  = _Rb_tree<_Key, _Val, _KeyOfValue, _Compare2, _Alloc>;

      template<typename, typename>
	friend struct _Rb_tree_merge_helper;

      /// Merge from a compatible container into one with unique keys.
      template<typename _Compare2>
	void
	_M_merge_unique(_Compatible_tree<_Compare2>& __src) noexcept
	{
	  using _Merge_helper = _Rb_tree_merge_helper<_Rb_tree, _Compare2>;
	  for (auto __i = __src.begin(), __end = __src.end(); __i != __end;)
	    {
	      auto __pos = __i++;
	      auto __res = _M_get_insert_unique_pos(_KeyOfValue()(*__pos));
	      if (__res.second)
		{
		  auto& __src_impl = _Merge_helper::_S_get_impl(__src);
		  auto __ptr = _Node_traits::_S_rebalance_for_erase
		    (__pos._M_node, __src_impl._M_header);
		  --__src_impl._M_node_count;
		  auto __node_ptr = static_cast<_Node&>(*__ptr)._M_node_ptr();
		  _M_insert_node(__res.first, __res.second, __node_ptr);
		}
	    }
	}

      /// Merge from a compatible container into one with equivalent keys.
      template<typename _Compare2>
	void
	_M_merge_equal(_Compatible_tree<_Compare2>& __src) noexcept
	{
	  using _Merge_helper = _Rb_tree_merge_helper<_Rb_tree, _Compare2>;
	  for (auto __i = __src.begin(), __end = __src.end(); __i != __end;)
	    {
	      auto __pos = __i++;
	      auto __res = _M_get_insert_equal_pos(_KeyOfValue()(*__pos));
	      if (__res.second)
		{
		  auto& __src_impl = _Merge_helper::_S_get_impl(__src);
		  auto __ptr = _Node_traits::_S_rebalance_for_erase
		    (__pos._M_node, __src_impl._M_header);
		  --__src_impl._M_node_count;
		  auto __node_ptr = static_cast<_Node&>(*__ptr)._M_node_ptr();
		  _M_insert_node(__res.first, __res.second, __node_ptr);
		}
	    }
	}
#endif // C++17 node_extract

      friend bool
      operator==(const _Rb_tree& __x, const _Rb_tree& __y)
      {
	return __x.size() == __y.size()
	  && std::equal(__x.begin(), __x.end(), __y.begin());
      }

#if __cpp_lib_three_way_comparison
      friend auto
      operator<=>(const _Rb_tree& __x, const _Rb_tree& __y)
      {
	if constexpr (requires { typename __detail::__synth3way_t<_Val>; })
	  return std::lexicographical_compare_three_way(__x.begin(), __x.end(),
							__y.begin(), __y.end(),
							__detail::__synth3way);
      }
#else
      friend bool
      operator<(const _Rb_tree& __x, const _Rb_tree& __y)
      {
	return std::lexicographical_compare(__x.begin(), __x.end(),
					    __y.begin(), __y.end());
      }
#endif

    private:
#if __cplusplus >= 201103L
      // An RAII _Node handle
      struct _Auto_node
      {
	template<typename... _Args>
	  _Auto_node(_Rb_tree& __t, _Args&&... __args)
	  : _M_t(__t),
	    _M_node(__t._M_create_node(std::forward<_Args>(__args)...))
	  { }

	~_Auto_node()
	{
	  if (_M_node)
	    _M_t._M_drop_node(_M_node);
	}

	_Auto_node(_Auto_node&& __n)
	: _M_t(__n._M_t), _M_node(__n._M_node)
	{ __n._M_node = nullptr; }

	const _Key&
	_M_key() const
	{ return _S_key(_M_node); }

	iterator
	_M_insert(pair<_Base_ptr, _Base_ptr> __p)
	{
	  auto __it = _M_t._M_insert_node(__p.first, __p.second, _M_node);
	  _M_node = nullptr;
	  return __it;
	}

	iterator
	_M_insert_equal_lower()
	{
	  auto __it = _M_t._M_insert_equal_lower_node(_M_node);
	  _M_node = nullptr;
	  return __it;
	}

	_Rb_tree& _M_t;
	_Node_ptr _M_node;
      };
#endif // C++11
    };

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    inline void
    swap(_Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>& __x,
	 _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>& __y)
    { __x.swap(__y); }

#if __cplusplus >= 201103L
  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_move_data(_Rb_tree& __x, false_type)
    {
      if (_M_get_Node_allocator() == __x._M_get_Node_allocator())
	_M_move_data(__x, true_type());
      else
	{
	  constexpr bool __move = !__move_if_noexcept_cond<value_type>::value;
	  _Alloc_node __an(*this);
	  _M_root() = _M_copy<__move>(__x, __an);
	  if _GLIBCXX17_CONSTEXPR (__move)
	    __x.clear();
	}
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    inline void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_move_assign(_Rb_tree& __x, true_type)
    {
      clear();
      if (__x._M_root())
	_M_move_data(__x, true_type());
      std::__alloc_on_move(_M_get_Node_allocator(),
			   __x._M_get_Node_allocator());
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_move_assign(_Rb_tree& __x, false_type)
    {
      if (_M_get_Node_allocator() == __x._M_get_Node_allocator())
	return _M_move_assign(__x, true_type{});

      // Try to move each node reusing existing nodes and copying __x nodes
      // structure.
      _Reuse_or_alloc_node __roan(*this);
      _M_impl._M_reset();
      if (__x._M_root())
	{
	  _M_root() = _M_copy<__as_rvalue>(__x, __roan);
	  __x.clear();
	}
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    inline _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>&
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    operator=(_Rb_tree&& __x)
    noexcept(_Node_alloc_traits::_S_nothrow_move()
	     && is_nothrow_move_assignable<_Compare>::value)
    {
      _M_impl._M_key_compare = std::move(__x._M_impl._M_key_compare);
      _M_move_assign(__x,
		     __bool_constant<_Node_alloc_traits::_S_nothrow_move()>());
      return *this;
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    template<typename _Iterator>
      void
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_assign_unique(_Iterator __first, _Iterator __last)
      {
	_Reuse_or_alloc_node __roan(*this);
	_M_impl._M_reset();
	for (; __first != __last; ++__first)
	  _M_insert_unique_(end(), *__first, __roan);
      }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    template<typename _Iterator>
      void
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_assign_equal(_Iterator __first, _Iterator __last)
      {
	_Reuse_or_alloc_node __roan(*this);
	_M_impl._M_reset();
	for (; __first != __last; ++__first)
	  _M_insert_equal_(end(), *__first, __roan);
      }
#endif

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>&
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    operator=(const _Rb_tree& __x)
    {
      if (this != std::__addressof(__x))
	{
	  // Note that _Key may be a constant type.
#if __cplusplus >= 201103L
	  if (_Node_alloc_traits::_S_propagate_on_copy_assign())
	    {
	      auto& __this_alloc = this->_M_get_Node_allocator();
	      auto& __that_alloc = __x._M_get_Node_allocator();
	      if (!_Node_alloc_traits::_S_always_equal()
		  && __this_alloc != __that_alloc)
		{
		  // Replacement allocator cannot free existing storage, we need
		  // to erase nodes first.
		  clear();
		  std::__alloc_on_copy(__this_alloc, __that_alloc);
		}
	    }
#endif

	  _Reuse_or_alloc_node __roan(*this);
	  _M_impl._M_reset();
	  _M_impl._M_key_compare = __x._M_impl._M_key_compare;
	  if (__x._M_root())
	    _M_root() = _M_copy<__as_lvalue>(__x, __roan);
	}

      return *this;
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
#if __cplusplus >= 201103L
    template<typename _Arg, typename _NodeGen>
#else
    template<typename _NodeGen>
#endif
      typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_insert_(_Base_ptr __x, _Base_ptr __p,
#if __cplusplus >= 201103L
		 _Arg&& __v,
#else
		 const _Val& __v,
#endif
		 _NodeGen& __node_gen)
      {
	bool __insert_left = (__x || __p == _M_end()
			      || _M_impl._M_key_compare(_KeyOfValue()(__v),
							_S_key(__p)));

	_Base_ptr __z =
	  __node_gen(_GLIBCXX_FORWARD(_Arg, __v))->_M_base_ptr();

	_Node_traits::_S_insert_and_rebalance
	  (__insert_left, __z, __p, this->_M_impl._M_header);
	++_M_impl._M_node_count;
	return iterator(__z);
      }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
#if __cplusplus >= 201103L
    template<typename _Arg>
#endif
    typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
#if __cplusplus >= 201103L
    _M_insert_lower(_Base_ptr __p, _Arg&& __v)
#else
    _M_insert_lower(_Base_ptr __p, const _Val& __v)
#endif
    {
      bool __insert_left = (__p == _M_end()
			    || !_M_impl._M_key_compare(_S_key(__p),
						       _KeyOfValue()(__v)));

      _Base_ptr __z =
	_M_create_node(_GLIBCXX_FORWARD(_Arg, __v))->_M_base_ptr();
      _Node_traits::_S_insert_and_rebalance
	(__insert_left, __z, __p, this->_M_impl._M_header);
      ++_M_impl._M_node_count;
      return iterator(__z);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
#if __cplusplus >= 201103L
    template<typename _Arg>
#endif
    typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
#if __cplusplus >= 201103L
    _M_insert_equal_lower(_Arg&& __v)
#else
    _M_insert_equal_lower(const _Val& __v)
#endif
    {
      _Base_ptr __x = _M_begin();
      _Base_ptr __y = _M_end();
      while (__x)
	{
	  __y = __x;
	  __x = !_M_impl._M_key_compare(_S_key(__x), _KeyOfValue()(__v)) ?
		_S_left(__x) : _S_right(__x);
	}
      return _M_insert_lower(__y, _GLIBCXX_FORWARD(_Arg, __v));
    }

  template<typename _Key, typename _Val, typename _KoV,
	   typename _Compare, typename _Alloc>
    template<bool _MoveValues, typename _NodeGen>
      typename _Rb_tree<_Key, _Val, _KoV, _Compare, _Alloc>::_Base_ptr
      _Rb_tree<_Key, _Val, _KoV, _Compare, _Alloc>::
      _M_copy(_Node_ptr __x, _Base_ptr __p, _NodeGen& __node_gen)
      {
	// Structural copy. __x and __p must be non-null.
	_Node_ptr __top = _M_clone_node<_MoveValues>(__x, __node_gen);
	_Base_ptr __top_base = __top->_M_base_ptr();
	__top->_M_parent = __p;

	__try
	  {
	    if (__x->_M_right)
	      __top->_M_right =
		_M_copy<_MoveValues>(_S_right(__x), __top_base, __node_gen);
	    __p = __top_base;
	    __x = _S_left(__x);

	    while (__x)
	      {
		_Base_ptr __y =
		  _M_clone_node<_MoveValues>(__x, __node_gen)->_M_base_ptr();
		__p->_M_left = __y;
		__y->_M_parent = __p;
		if (__x->_M_right)
		  __y->_M_right = _M_copy<_MoveValues>(_S_right(__x),
						       __y, __node_gen);
		__p = __y;
		__x = _S_left(__x);
	      }
	  }
	__catch(...)
	  {
	    _M_erase(__top);
	    __throw_exception_again;
	  }
	return __top_base;
      }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_erase(_Node_ptr __x)
    {
      // Erase without rebalancing.
      while (__x)
	{
	  _M_erase(_S_right(__x));
	  _Node_ptr __y = _S_left(__x);
	  _M_drop_node(__x);
	  __x = __y;
	}
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue,
		      _Compare, _Alloc>::_Base_ptr
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_lower_bound(_Base_ptr __x, _Base_ptr __y,
		   const _Key& __k) const
    {
      while (__x)
	if (!_M_impl._M_key_compare(_S_key(__x), __k))
	  __y = __x, __x = _S_left(__x);
	else
	  __x = _S_right(__x);
      return __y;
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue,
		      _Compare, _Alloc>::_Base_ptr
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_upper_bound(_Base_ptr __x, _Base_ptr __y,
		   const _Key& __k) const
    {
      while (__x)
	if (_M_impl._M_key_compare(__k, _S_key(__x)))
	  __y = __x, __x = _S_left(__x);
	else
	  __x = _S_right(__x);
      return __y;
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::iterator,
	 typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::iterator>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    equal_range(const _Key& __k)
    {
      typedef pair<iterator, iterator> _Ret;

      _Base_ptr __x = _M_begin();
      _Base_ptr __y = _M_end();
      while (__x)
	{
	  if (_M_impl._M_key_compare(_S_key(__x), __k))
	    __x = _S_right(__x);
	  else if (_M_impl._M_key_compare(__k, _S_key(__x)))
	    __y = __x, __x = _S_left(__x);
	  else
	    {
	      _Base_ptr __xu(__x);
	      _Base_ptr __yu(__y);
	      __y = __x, __x = _S_left(__x);
	      __xu = _S_right(__xu);
	      return _Ret(iterator(_M_lower_bound(__x, __y, __k)),
			  iterator(_M_upper_bound(__xu, __yu, __k)));
	    }
	}
      return _Ret(iterator(__y), iterator(__y));
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::const_iterator,
	 typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::const_iterator>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    equal_range(const _Key& __k) const
    {
      typedef pair<const_iterator, const_iterator> _Ret;

      _Base_ptr __x = _M_begin();
      _Base_ptr __y = _M_end();
      while (__x)
	{
	  if (_M_impl._M_key_compare(_S_key(__x), __k))
	    __x = _S_right(__x);
	  else if (_M_impl._M_key_compare(__k, _S_key(__x)))
	    __y = __x, __x = _S_left(__x);
	  else
	    {
	      _Base_ptr __xu(__x);
	      _Base_ptr __yu(__y);
	      __y = __x, __x = _S_left(__x);
	      __xu = _S_right(__xu);
	      return _Ret(const_iterator(_M_lower_bound(__x, __y, __k)),
			  const_iterator(_M_upper_bound(__xu, __yu, __k)));
	    }
	}
      return _Ret(const_iterator(__y), const_iterator(__y));
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    swap(_Rb_tree& __t)
    _GLIBCXX_NOEXCEPT_IF(__is_nothrow_swappable<_Compare>::value)
    {
      if (!_M_root())
	{
	  if (__t._M_root())
	    _M_impl._M_move_data(__t._M_impl);
	}
      else if (!__t._M_root())
	__t._M_impl._M_move_data(_M_impl);
      else
	{
	  std::swap(_M_root(),__t._M_root());
	  std::swap(_M_leftmost(),__t._M_leftmost());
	  std::swap(_M_rightmost(),__t._M_rightmost());

	  _M_root()->_M_parent = _M_end();
	  __t._M_root()->_M_parent = __t._M_end();
	  std::swap(this->_M_impl._M_node_count, __t._M_impl._M_node_count);
	}
      // No need to swap header's color as it does not change.

      using std::swap;
      swap(this->_M_impl._M_key_compare, __t._M_impl._M_key_compare);

      _Node_alloc_traits::_S_on_swap(_M_get_Node_allocator(),
				     __t._M_get_Node_allocator());
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr,
	 typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_get_insert_unique_pos(const key_type& __k)
    {
      typedef pair<_Base_ptr, _Base_ptr> _Res;
      _Base_ptr __x = _M_begin();
      _Base_ptr __y = _M_end();
      bool __comp = true;
      while (__x)
	{
	  __y = __x;
	  __comp = _M_impl._M_key_compare(__k, _S_key(__x));
	  __x = __comp ? _S_left(__x) : _S_right(__x);
	}
      iterator __j = iterator(__y);
      if (__comp)
	{
	  if (__j == begin())
	    return _Res(__x, __y);
	  else
	    --__j;
	}
      if (_M_impl._M_key_compare(_S_key(__j._M_node), __k))
	return _Res(__x, __y);
      return _Res(__j._M_node, _Base_ptr());
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr,
	 typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_get_insert_equal_pos(const key_type& __k)
    {
      typedef pair<_Base_ptr, _Base_ptr> _Res;
      _Base_ptr __x = _M_begin();
      _Base_ptr __y = _M_end();
      while (__x)
	{
	  __y = __x;
	  __x = _M_impl._M_key_compare(__k, _S_key(__x)) ?
		_S_left(__x) : _S_right(__x);
	}
      return _Res(__x, __y);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
#if __cplusplus >= 201103L
    template<typename _Arg>
#endif
    pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::iterator, bool>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
#if __cplusplus >= 201103L
    _M_insert_unique(_Arg&& __v)
#else
    _M_insert_unique(const _Val& __v)
#endif
    {
      typedef pair<iterator, bool> _Res;
      pair<_Base_ptr, _Base_ptr> __res
	= _M_get_insert_unique_pos(_KeyOfValue()(__v));

      if (__res.second)
	{
	  _Alloc_node __an(*this);
	  return _Res(_M_insert_(__res.first, __res.second,
				 _GLIBCXX_FORWARD(_Arg, __v), __an),
		      true);
	}

      return _Res(iterator(__res.first), false);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
#if __cplusplus >= 201103L
    template<typename _Arg>
#endif
    typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
#if __cplusplus >= 201103L
    _M_insert_equal(_Arg&& __v)
#else
    _M_insert_equal(const _Val& __v)
#endif
    {
      pair<_Base_ptr, _Base_ptr> __res
	= _M_get_insert_equal_pos(_KeyOfValue()(__v));
      _Alloc_node __an(*this);
      return _M_insert_(__res.first, __res.second,
			_GLIBCXX_FORWARD(_Arg, __v), __an);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr,
	 typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_get_insert_hint_unique_pos(const_iterator __position,
				  const key_type& __k)
    {
      typedef pair<_Base_ptr, _Base_ptr> _Res;

      // end()
      if (__position._M_node == _M_end())
	{
	  if (size() > 0
	      && _M_impl._M_key_compare(_S_key(_M_rightmost()), __k))
	    return _Res(_Base_ptr(), _M_rightmost());
	  else
	    return _M_get_insert_unique_pos(__k);
	}
      else if (_M_impl._M_key_compare(__k, _S_key(__position._M_node)))
	{
	  // First, try before...
	  iterator __before(__position._M_node);
	  if (__position._M_node == _M_leftmost()) // begin()
	    return _Res(_M_leftmost(), _M_leftmost());
	  else if (_M_impl._M_key_compare(_S_key((--__before)._M_node), __k))
	    {
	      if (!_S_right(__before._M_node))
		return _Res(_Base_ptr(), __before._M_node);
	      else
		return _Res(__position._M_node, __position._M_node);
	    }
	  else
	    return _M_get_insert_unique_pos(__k);
	}
      else if (_M_impl._M_key_compare(_S_key(__position._M_node), __k))
	{
	  // ... then try after.
	  iterator __after(__position._M_node);
	  if (__position._M_node == _M_rightmost())
	    return _Res(_Base_ptr(), _M_rightmost());
	  else if (_M_impl._M_key_compare(__k, _S_key((++__after)._M_node)))
	    {
	      if (!_S_right(__position._M_node))
		return _Res(_Base_ptr(), __position._M_node);
	      else
		return _Res(__after._M_node, __after._M_node);
	    }
	  else
	    return _M_get_insert_unique_pos(__k);
	}
      else
	// Equivalent keys.
	return _Res(__position._M_node, _Base_ptr());
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
#if __cplusplus >= 201103L
    template<typename _Arg, typename _NodeGen>
#else
    template<typename _NodeGen>
#endif
      typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_insert_unique_(const_iterator __position,
#if __cplusplus >= 201103L
			_Arg&& __v,
#else
			const _Val& __v,
#endif
			_NodeGen& __node_gen)
    {
      pair<_Base_ptr, _Base_ptr> __res
	= _M_get_insert_hint_unique_pos(__position, _KeyOfValue()(__v));

      if (__res.second)
	return _M_insert_(__res.first, __res.second,
			  _GLIBCXX_FORWARD(_Arg, __v),
			  __node_gen);
      return iterator(__res.first);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr,
	 typename _Rb_tree<_Key, _Val, _KeyOfValue,
			   _Compare, _Alloc>::_Base_ptr>
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_get_insert_hint_equal_pos(const_iterator __position, const key_type& __k)
    {
      typedef pair<_Base_ptr, _Base_ptr> _Res;

      // end()
      if (__position._M_node == _M_end())
	{
	  if (size() > 0
	      && !_M_impl._M_key_compare(__k, _S_key(_M_rightmost())))
	    return _Res(_Base_ptr(), _M_rightmost());
	  else
	    return _M_get_insert_equal_pos(__k);
	}
      else if (!_M_impl._M_key_compare(_S_key(__position._M_node), __k))
	{
	  // First, try before...
	  iterator __before(__position._M_node);
	  if (__position._M_node == _M_leftmost()) // begin()
	    return _Res(_M_leftmost(), _M_leftmost());
	  else if (!_M_impl._M_key_compare(__k, _S_key((--__before)._M_node)))
	    {
	      if (!_S_right(__before._M_node))
		return _Res(_Base_ptr(), __before._M_node);
	      else
		return _Res(__position._M_node, __position._M_node);
	    }
	  else
	    return _M_get_insert_equal_pos(__k);
	}
      else
	{
	  // ... then try after.
	  iterator __after(__position._M_node);
	  if (__position._M_node == _M_rightmost())
	    return _Res(_Base_ptr(), _M_rightmost());
	  else if (!_M_impl._M_key_compare(_S_key((++__after)._M_node), __k))
	    {
	      if (!_S_right(__position._M_node))
		return _Res(_Base_ptr(), __position._M_node);
	      else
		return _Res(__after._M_node, __after._M_node);
	    }
	  else
	    return _Res(_Base_ptr(), _Base_ptr());
	}
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
#if __cplusplus >= 201103L
    template<typename _Arg, typename _NodeGen>
#else
    template<typename _NodeGen>
#endif
      typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_insert_equal_(const_iterator __position,
#if __cplusplus >= 201103L
		       _Arg&& __v,
#else
		       const _Val& __v,
#endif
		       _NodeGen& __node_gen)
      {
	pair<_Base_ptr, _Base_ptr> __res
	  = _M_get_insert_hint_equal_pos(__position, _KeyOfValue()(__v));

	if (__res.second)
	  return _M_insert_(__res.first, __res.second,
			    _GLIBCXX_FORWARD(_Arg, __v),
			    __node_gen);

	return _M_insert_equal_lower(_GLIBCXX_FORWARD(_Arg, __v));
      }

#if __cplusplus >= 201103L
  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    auto
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_insert_node(_Base_ptr __x, _Base_ptr __p, _Node_ptr __z)
    -> iterator
    {
      bool __insert_left = (__x || __p == _M_end()
			    || _M_impl._M_key_compare(_S_key(__z),
						      _S_key(__p)));

      _Base_ptr __base_z = __z->_M_base_ptr();
      _Node_traits::_S_insert_and_rebalance
	(__insert_left, __base_z, __p, this->_M_impl._M_header);
      ++_M_impl._M_node_count;
      return iterator(__base_z);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    auto
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_insert_lower_node(_Base_ptr __p, _Node_ptr __z)
    -> iterator
    {
      bool __insert_left = (__p == _M_end()
			    || !_M_impl._M_key_compare(_S_key(__p),
						       _S_key(__z)));

      _Base_ptr __base_z = __z->_M_base_ptr();
      _Node_traits::_S_insert_and_rebalance
	(__insert_left, __base_z, __p, this->_M_impl._M_header);
      ++_M_impl._M_node_count;
      return iterator(__base_z);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    auto
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_insert_equal_lower_node(_Node_ptr __z)
    -> iterator
    {
      _Base_ptr __x = _M_begin();
      _Base_ptr __y = _M_end();
      while (__x)
	{
	  __y = __x;
	  __x = !_M_impl._M_key_compare(_S_key(__x), _S_key(__z)) ?
		_S_left(__x) : _S_right(__x);
	}
      return _M_insert_lower_node(__y, __z);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    template<typename... _Args>
      auto
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_emplace_unique(_Args&&... __args)
      -> pair<iterator, bool>
      {
	_Auto_node __z(*this, std::forward<_Args>(__args)...);
	auto __res = _M_get_insert_unique_pos(__z._M_key());
	if (__res.second)
	  return {__z._M_insert(__res), true};
	return {iterator(__res.first), false};
      }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    template<typename... _Args>
      auto
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_emplace_equal(_Args&&... __args)
      -> iterator
      {
	_Auto_node __z(*this, std::forward<_Args>(__args)...);
	auto __res = _M_get_insert_equal_pos(__z._M_key());
	return __z._M_insert(__res);
      }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    template<typename... _Args>
      auto
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_emplace_hint_unique(const_iterator __pos, _Args&&... __args)
      -> iterator
      {
	_Auto_node __z(*this, std::forward<_Args>(__args)...);
	auto __res = _M_get_insert_hint_unique_pos(__pos, __z._M_key());
	if (__res.second)
	  return __z._M_insert(__res);
	return iterator(__res.first);
      }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    template<typename... _Args>
      auto
      _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
      _M_emplace_hint_equal(const_iterator __pos, _Args&&... __args)
      -> iterator
      {
	_Auto_node __z(*this, std::forward<_Args>(__args)...);
	auto __res = _M_get_insert_hint_equal_pos(__pos, __z._M_key());
	if (__res.second)
	  return __z._M_insert(__res);
	return __z._M_insert_equal_lower();
      }
#endif


  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_erase_aux(const_iterator __position)
    {
      _Base_ptr __y = _Node_traits::_S_rebalance_for_erase
	(__position._M_node, this->_M_impl._M_header);
      _M_drop_node(static_cast<_Node&>(*__y)._M_node_ptr());
      --_M_impl._M_node_count;
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_erase_aux(const_iterator __first, const_iterator __last)
    {
      if (__first == begin() && __last == end())
	clear();
      else
	while (__first != __last)
	  _M_erase_aux(__first++);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::size_type
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    erase(const _Key& __x)
    {
      pair<iterator, iterator> __p = equal_range(__x);
      const size_type __old_size = size();
      _M_erase_aux(__p.first, __p.second);
      return __old_size - size();
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue,
		      _Compare, _Alloc>::iterator
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    find(const _Key& __k)
    {
      iterator __j(_M_lower_bound(_M_begin(), _M_end(), __k));
      return (__j == end()
	      || _M_impl._M_key_compare(__k,
					_S_key(__j._M_node))) ? end() : __j;
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue,
		      _Compare, _Alloc>::const_iterator
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    find(const _Key& __k) const
    {
      const_iterator __j(_M_lower_bound(_M_begin(), _M_end(), __k));
      return (__j == end()
	      || _M_impl._M_key_compare(__k,
					_S_key(__j._M_node))) ? end() : __j;
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::size_type
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    count(const _Key& __k) const
    {
      pair<const_iterator, const_iterator> __p = equal_range(__k);
      const size_type __n = std::distance(__p.first, __p.second);
      return __n;
    }

  _GLIBCXX_PURE unsigned int
  _Rb_tree_black_count(const _Rb_tree_node_base* __node,
		       const _Rb_tree_node_base* __root) throw ();

  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc>
    bool
    _Rb_tree<_Key,_Val,_KeyOfValue,_Compare,_Alloc>::__rb_verify() const
    {
      if (_M_impl._M_node_count == 0 || begin() == end())
	return _M_impl._M_node_count == 0 && begin() == end()
	       && this->_M_impl._M_header._M_left == _M_end()
	       && this->_M_impl._M_header._M_right == _M_end();

      unsigned int __len = _Rb_tree_black_count(_M_leftmost(), _M_root());
      for (const_iterator __it = begin(); __it != end(); ++__it)
	{
	  _Base_ptr __x = __it._M_node;
	  _Base_ptr __L = _S_left(__x);
	  _Base_ptr __R = _S_right(__x);

	  if (__x->_M_color == _S_red)
	    if ((__L && __L->_M_color == _S_red)
		|| (__R && __R->_M_color == _S_red))
	      return false;

	  if (__L && _M_impl._M_key_compare(_S_key(__x), _S_key(__L)))
	    return false;
	  if (__R && _M_impl._M_key_compare(_S_key(__R), _S_key(__x)))
	    return false;

	  if (!__L && !__R && _Rb_tree_black_count(__x, _M_root()) != __len)
	    return false;
	}

      if (_M_leftmost() != _Node_base::_S_minimum(_M_root()))
	return false;
      if (_M_rightmost() != _Node_base::_S_maximum(_M_root()))
	return false;
      return true;
    }

#ifdef __glibcxx_node_extract // >= C++17
  // Allow access to internals of compatible _Rb_tree specializations.
  template<typename _Key, typename _Val, typename _Sel, typename _Cmp1,
	   typename _Alloc, typename _Cmp2>
    struct _Rb_tree_merge_helper<_Rb_tree<_Key, _Val, _Sel, _Cmp1, _Alloc>,
				 _Cmp2>
    {
    private:
      friend class _Rb_tree<_Key, _Val, _Sel, _Cmp1, _Alloc>;

      static auto&
      _S_get_impl(_Rb_tree<_Key, _Val, _Sel, _Cmp2, _Alloc>& __tree)
      { return __tree._M_impl; }
    };
#endif // C++17

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif
