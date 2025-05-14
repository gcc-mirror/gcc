// <forward_list.h> -*- C++ -*-

// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

/** @file bits/forward_list.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{forward_list}
 */

#ifndef _FORWARD_LIST_H
#define _FORWARD_LIST_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <initializer_list>
#include <bits/stl_iterator_base_types.h>
#include <bits/stl_iterator.h>
#include <bits/stl_algobase.h>
#include <bits/stl_function.h>
#include <bits/allocator.h>
#include <bits/allocated_ptr.h>
#include <bits/ptr_traits.h>
#include <debug/assertions.h>
#include <ext/alloc_traits.h>
#include <ext/aligned_buffer.h>
#include <debug/assertions.h>
#if __glibcxx_containers_ranges // C++ >= 23
# include <bits/ranges_base.h> // ranges::begin, ranges::distance etc.
# include <bits/ranges_util.h> // ranges::subrange
#endif

#if ! defined _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
# define _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST 1
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_BEGIN_NAMESPACE_CONTAINER

  /**
   *  @brief  A helper basic node class for %forward_list.
   *
   *          This is just a linked list with nothing inside it.
   *          There are purely list shuffling utility methods here.
   */
  struct _Fwd_list_node_base
  {
    using _Base_ptr = _Fwd_list_node_base*;

    _Fwd_list_node_base() = default;
    _Fwd_list_node_base(_Fwd_list_node_base&& __x) noexcept
      : _M_next(__x._M_next)
    { __x._M_next = nullptr; }

    _Fwd_list_node_base(const _Fwd_list_node_base&) = delete;
    _Fwd_list_node_base& operator=(const _Fwd_list_node_base&) = delete;

    _Fwd_list_node_base&
    operator=(_Fwd_list_node_base&& __x) noexcept
    {
      _M_next = __x._M_next;
      __x._M_next = nullptr;
      return *this;
    }

    _Fwd_list_node_base* _M_next = nullptr;

    _Fwd_list_node_base*
    _M_transfer_after(_Fwd_list_node_base* __begin,
		      _Fwd_list_node_base* __end) noexcept
    {
      _Fwd_list_node_base* __keep = __begin->_M_next;
      if (__end)
	{
	  __begin->_M_next = __end->_M_next;
	  __end->_M_next = _M_next;
	}
      else
	__begin->_M_next = nullptr;
      _M_next = __keep;
      return __end;
    }

    void
    _M_reverse_after() noexcept
    {
      _Fwd_list_node_base* __tail = _M_next;
      if (!__tail)
	return;
      while (_Fwd_list_node_base* __temp = __tail->_M_next)
	{
	  _Fwd_list_node_base* __keep = _M_next;
	  _M_next = __temp;
	  __tail->_M_next = __temp->_M_next;
	  _M_next->_M_next = __keep;
	}
    }

    _Fwd_list_node_base* _M_base_ptr() { return this; }
    const _Fwd_list_node_base* _M_base_ptr() const { return this; }
  };

  /**
   *  @brief  A helper node class for %forward_list.
   *          This is just a linked list with uninitialized storage for a
   *          data value in each node.
   *          There is a sorting utility method.
   */
  template<typename _Tp>
    struct _Fwd_list_node
    : public _Fwd_list_node_base
    {
      using _Node_ptr = _Fwd_list_node*;

      _Fwd_list_node() = default;

      __gnu_cxx::__aligned_buffer<_Tp> _M_storage;

      _Tp*
      _M_valptr() noexcept
      { return _M_storage._M_ptr(); }

      const _Tp*
      _M_valptr() const noexcept
      { return _M_storage._M_ptr(); }

      _Node_ptr
      _M_node_ptr()
      { return this; }
    };

  template<typename _Tp> struct _Fwd_list_const_iterator;

  /**
   *   @brief A forward_list::iterator.
   *
   *   All the functions are op overloads.
   */
  template<typename _Tp>
    struct _Fwd_list_iterator
    {
      typedef _Fwd_list_iterator<_Tp>		_Self;
      typedef _Fwd_list_node<_Tp>		_Node;

      typedef _Tp				value_type;
      typedef _Tp*				pointer;
      typedef _Tp&				reference;
      typedef ptrdiff_t				difference_type;
      typedef std::forward_iterator_tag		iterator_category;

      _Fwd_list_iterator() noexcept
      : _M_node() { }

      explicit
      _Fwd_list_iterator(_Fwd_list_node_base* __n) noexcept
      : _M_node(__n) { }

      [[__nodiscard__]]
      reference
      operator*() const noexcept
      { return *static_cast<_Node*>(this->_M_node)->_M_valptr(); }

      [[__nodiscard__]]
      pointer
      operator->() const noexcept
      { return static_cast<_Node*>(this->_M_node)->_M_valptr(); }

      _Self&
      operator++() noexcept
      {
	_M_node = _M_node->_M_next;
	return *this;
      }

      _Self
      operator++(int) noexcept
      {
	_Self __tmp(*this);
	_M_node = _M_node->_M_next;
	return __tmp;
      }

      /**
       *  @brief  Forward list iterator equality comparison.
       */
      [[__nodiscard__]]
      friend bool
      operator==(const _Self& __x, const _Self& __y) noexcept
      { return __x._M_node == __y._M_node; }

#if __cpp_impl_three_way_comparison < 201907L
      /**
       *  @brief  Forward list iterator inequality comparison.
       */
      [[__nodiscard__]]
      friend bool
      operator!=(const _Self& __x, const _Self& __y) noexcept
      { return __x._M_node != __y._M_node; }
#endif

    private:
      template<typename, typename>
	friend class forward_list;
      template<typename, typename>
	friend struct _Fwd_list_base;
      friend struct _Fwd_list_const_iterator<_Tp>;

      _Self
      _M_next() const noexcept
      {
	if (_M_node)
	  return _Fwd_list_iterator(_M_node->_M_next);
	else
	  return _Fwd_list_iterator(nullptr);
      }

      _Fwd_list_node_base* _M_node;
    };

  /**
   *   @brief A forward_list::const_iterator.
   *
   *   All the functions are op overloads.
   */
  template<typename _Tp>
    struct _Fwd_list_const_iterator
    {
      typedef _Fwd_list_const_iterator<_Tp>	_Self;
      typedef const _Fwd_list_node<_Tp>		_Node;
      typedef _Fwd_list_iterator<_Tp>		iterator;

      typedef _Tp				value_type;
      typedef const _Tp*			pointer;
      typedef const _Tp&			reference;
      typedef ptrdiff_t				difference_type;
      typedef std::forward_iterator_tag		iterator_category;

      _Fwd_list_const_iterator() noexcept
      : _M_node() { }

      explicit
      _Fwd_list_const_iterator(const _Fwd_list_node_base* __n)  noexcept
      : _M_node(__n) { }

      _Fwd_list_const_iterator(const iterator& __iter) noexcept
      : _M_node(__iter._M_node) { }

      [[__nodiscard__]]
      reference
      operator*() const noexcept
      { return *static_cast<_Node*>(this->_M_node)->_M_valptr(); }

      [[__nodiscard__]]
      pointer
      operator->() const noexcept
      { return static_cast<_Node*>(this->_M_node)->_M_valptr(); }

      _Self&
      operator++() noexcept
      {
	_M_node = _M_node->_M_next;
	return *this;
      }

      _Self
      operator++(int) noexcept
      {
	_Self __tmp(*this);
	_M_node = _M_node->_M_next;
	return __tmp;
      }

      /**
       *  @brief  Forward list const_iterator equality comparison.
       */
      [[__nodiscard__]]
      friend bool
      operator==(const _Self& __x, const _Self& __y) noexcept
      { return __x._M_node == __y._M_node; }

#if __cpp_impl_three_way_comparison < 201907L
      /**
       *  @brief  Forward list const_iterator inequality comparison.
       */
      [[__nodiscard__]]
      friend bool
      operator!=(const _Self& __x, const _Self& __y) noexcept
      { return __x._M_node != __y._M_node; }
#endif

    private:
      template<typename, typename>
	friend class forward_list;
      template<typename, typename>
	friend struct _Fwd_list_base;

      _Self
      _M_next() const noexcept
      {
	if (this->_M_node)
	  return _Fwd_list_const_iterator(_M_node->_M_next);
	else
	  return _Fwd_list_const_iterator(nullptr);
      }

      _Fwd_list_iterator<_Tp>
      _M_const_cast() const noexcept
      {
	return _Fwd_list_iterator<_Tp>(
		 const_cast<_Fwd_list_node_base*>(_M_node));
      }

      const _Fwd_list_node_base* _M_node;
    };

  template<typename _Tp, typename _Allocator> class forward_list;
  template<typename _Tp, typename _Allocator> struct _Fwd_list_base;

namespace __fwdlist
{
#if _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
  /// The node-base type for allocators that use fancy pointers.
  template<typename _VoidPtr>
    struct _Node_base
    {
      using _Base_ptr = __ptr_rebind<_VoidPtr, _Node_base>;

      _Node_base() = default;

      _Node_base(_Node_base&& __x) noexcept
      : _M_next(__x._M_next)
      { __x._M_next = nullptr; }

      _Node_base(const _Node_base&) = delete;
      _Node_base& operator=(const _Node_base&) = delete;

      _Node_base&
      operator=(_Node_base&& __x) noexcept
      {
	_M_next = __x._M_next;
	__x._M_next = nullptr;
	return *this;
      }

      _Base_ptr _M_next = nullptr;

      // Splice (begin,end) before _M_next.
      _Base_ptr
      _M_transfer_after(_Base_ptr __begin, _Base_ptr __end) noexcept
      {
	_Base_ptr __keep = __begin->_M_next;
	if (__end)
	  {
	    __begin->_M_next = __end->_M_next;
	    __end->_M_next = _M_next;
	  }
	else
	  __begin->_M_next = nullptr;
	_M_next = __keep;
	return __end;
      }

      void
      _M_reverse_after() noexcept
      {
	_Base_ptr __tail = _M_next;
	if (!__tail)
	  return;
	while (_Base_ptr __temp = __tail->_M_next)
	  {
	    _Base_ptr __keep = _M_next;
	    _M_next = __temp;
	    __tail->_M_next = __temp->_M_next;
	    _M_next->_M_next = __keep;
	  }
      }

      // This is not const-correct, but it's only used in a const access path
      // by std::forward_list::empty(), where it doesn't escape, and by
      // std::forward_list::before_begin() const, where the pointer is used
      // to initialize a const_iterator and so constness is restored.
      _Base_ptr
      _M_base_ptr() const
      {
	return pointer_traits<_Base_ptr>::
		 pointer_to(const_cast<_Node_base&>(*this));
      }
    };

  /**
   *  @brief  A helper node class for %forward_list.
   */
  template<typename _ValPtr>
    struct _Node
    : public _Node_base<__ptr_rebind<_ValPtr, void>>
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

#if ! _GLIBCXX_INLINE_VERSION
	// For ABI compatibility we need to overalign this member.
	alignas(__alignof__(value_type)) // XXX GLIBCXX_ABI Deprecated
#endif
	value_type _M_data;
      };
      _Uninit_storage _M_u;

      value_type*
      _M_valptr() noexcept
      { return std::__addressof(_M_u._M_data); }

      const value_type*
      _M_valptr() const noexcept
      { return std::__addressof(_M_u._M_data); }

      _Node_ptr
      _M_node_ptr()
      { return pointer_traits<_Node_ptr>::pointer_to(*this); }
    };

  /// A forward_list iterator when the allocator uses fancy pointers.
  template<bool _Const, typename _Ptr>
    class _Iterator
    {
      using _Node = __fwdlist::_Node<_Ptr>;
      using _Base_ptr
	= typename __fwdlist::_Node_base<__ptr_rebind<_Ptr, void>>::_Base_ptr;

      template<typename _Tp>
	using __maybe_const = __conditional_t<_Const, const _Tp, _Tp>;

    public:
      using value_type        = typename pointer_traits<_Ptr>::element_type;
      using difference_type   = ptrdiff_t;
      using iterator_category = forward_iterator_tag;
      using pointer           = __maybe_const<value_type>*;
      using reference         = __maybe_const<value_type>&;

      constexpr _Iterator() noexcept : _M_node() { }

      _Iterator(const _Iterator&) = default;
      _Iterator& operator=(const _Iterator&) = default;

#ifdef __glibcxx_concepts
      constexpr
      _Iterator(const _Iterator<false, _Ptr>& __i) requires _Const
#else
      template<bool _OtherConst,
	       typename = __enable_if_t<_Const && !_OtherConst>>
	constexpr
	_Iterator(const _Iterator<_OtherConst, _Ptr>& __i)
#endif
	: _M_node(__i._M_node) { }

      constexpr explicit
      _Iterator(_Base_ptr __x) noexcept
      : _M_node(__x) { }

      [[__nodiscard__]]
      constexpr reference
      operator*() const noexcept
      { return static_cast<_Node&>(*this->_M_node)._M_u._M_data; }

      [[__nodiscard__]]
      constexpr pointer
      operator->() const noexcept
      { return static_cast<_Node&>(*this->_M_node)._M_valptr(); }

      _GLIBCXX14_CONSTEXPR _Iterator&
      operator++() noexcept
      {
	_M_node = _M_node->_M_next;
	return *this;
      }

      _GLIBCXX14_CONSTEXPR _Iterator
      operator++(int) noexcept
      {
	_Iterator __tmp(*this);
	_M_node = _M_node->_M_next;
	return __tmp;
      }

      /**
       *  @brief  Forward list iterator equality comparison.
       */
      [[__nodiscard__]]
      friend constexpr bool
      operator==(const _Iterator& __x, const _Iterator& __y) noexcept
      { return __x._M_node == __y._M_node; }

#if __cpp_impl_three_way_comparison < 201907L
      /**
       *  @brief  Forward list iterator inequality comparison.
       */
      [[__nodiscard__]]
      friend constexpr bool
      operator!=(const _Iterator& __x, const _Iterator& __y) noexcept
      { return __x._M_node != __y._M_node; }
#endif

    private:
      template<typename _Tp, typename _Allocator>
	friend class _GLIBCXX_STD_C::forward_list;
      template<typename _Tp, typename _Allocator>
	friend struct _GLIBCXX_STD_C::_Fwd_list_base;

      constexpr _Iterator<false, _Ptr>
      _M_const_cast() const noexcept
      { return _Iterator<false, _Ptr>(_M_node); }

      friend _Iterator<!_Const, _Ptr>;

      constexpr _Iterator
      _M_next() const noexcept
      { return _Iterator(_M_node ? _M_node->_M_next : nullptr); }

      _Base_ptr _M_node;
    };
#endif // USE_ALLOC_PTR_FOR_FWD_LIST

  // Determine the node and iterator types used by std::forward_list.
  template<typename _Tp, typename _Ptr>
    struct _Node_traits;

#if _GLIBCXX_USE_ALLOC_PTR_FOR_LIST <= 9000
  // Specialization for the simple case where the allocator's pointer type
  // is the same type as value_type*.
  // For ABI compatibility we can't change the types used for this case.
  template<typename _Tp>
    struct _Node_traits<_Tp, _Tp*>
    {
      using _Node_base	    = _Fwd_list_node_base;
      using _Node	    = _Fwd_list_node<_Tp>;
      using _Iterator	    = _Fwd_list_iterator<_Tp>;
      using _Const_iterator = _Fwd_list_const_iterator<_Tp>;
    };
#endif

#if ! _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
  // Always use the T* specialization.
  template<typename _Tp, typename _Ptr>
    struct _Node_traits
    : _Node_traits<_Tp, _Tp*>
    { };
#else
  // Primary template used when the allocator uses fancy pointers.
  template<typename _Tp, typename _Ptr>
    struct _Node_traits
    {
    private:
      using _VoidPtr = __ptr_rebind<_Ptr, void>;
      using _ValPtr = __ptr_rebind<_Ptr, _Tp>;

    public:
      using _Node_base      = __fwdlist::_Node_base<_VoidPtr>;
      using _Node           = __fwdlist::_Node<_ValPtr>;
      using _Iterator       = __fwdlist::_Iterator<false, _ValPtr>;
      using _Const_iterator = __fwdlist::_Iterator<true, _ValPtr>;
    };
#endif // USE_ALLOC_PTR_FOR_FWD_LIST
} // namespace __fwdlist

  /**
   *  @brief  Base class for %forward_list.
   */
  template<typename _Tp, typename _Alloc>
    struct _Fwd_list_base
    {
#if __cplusplus > 201703L || defined __STRICT_ANSI__
      // The static_assert in forward_list ensures _Alloc::value_type is _Tp.
      using pointer = typename allocator_traits<_Alloc>::pointer;
#else
      using _Tp_alloc_traits
	= typename allocator_traits<_Alloc>::template rebind_traits<_Tp>;
      using pointer = typename _Tp_alloc_traits::pointer;
#endif

    protected:
      using _Node_traits = __fwdlist::_Node_traits<_Tp, pointer>;
      using _Node = typename _Node_traits::_Node;
      using _Node_alloc_type = __alloc_rebind<_Alloc, _Node>;
      using _Node_alloc_traits = __gnu_cxx::__alloc_traits<_Node_alloc_type>;
      using _Node_ptr = typename _Node_alloc_traits::pointer;
      using _Base_ptr = typename _Node_traits::_Node_base::_Base_ptr;

      struct _Fwd_list_impl
      : public _Node_alloc_type
      {
	typename _Node_traits::_Node_base _M_head;

	_Fwd_list_impl()
	  noexcept(is_nothrow_default_constructible<_Node_alloc_type>::value)
	: _Node_alloc_type(), _M_head()
	{ }

	_Fwd_list_impl(_Fwd_list_impl&&) = default;

	_Fwd_list_impl(_Fwd_list_impl&& __fl, _Node_alloc_type&& __a)
	: _Node_alloc_type(std::move(__a)), _M_head(std::move(__fl._M_head))
	{ }

	_Fwd_list_impl(_Node_alloc_type&& __a)
	: _Node_alloc_type(std::move(__a)), _M_head()
	{ }
      };

      _Fwd_list_impl _M_impl;

    public:
      using iterator = typename _Node_traits::_Iterator;
      using const_iterator = typename _Node_traits::_Const_iterator;

      _Node_alloc_type&
      _M_get_Node_allocator() noexcept
      { return this->_M_impl; }

      const _Node_alloc_type&
      _M_get_Node_allocator() const noexcept
      { return this->_M_impl; }

      _Fwd_list_base() = default;

      _Fwd_list_base(_Node_alloc_type&& __a)
      : _M_impl(std::move(__a)) { }

      // When allocators are always equal.
      _Fwd_list_base(_Fwd_list_base&& __lst, _Node_alloc_type&& __a,
		     std::true_type)
      : _M_impl(std::move(__lst._M_impl), std::move(__a))
      { }

      // When allocators are not always equal.
      _Fwd_list_base(_Fwd_list_base&& __lst, _Node_alloc_type&& __a);

      _Fwd_list_base(_Fwd_list_base&&) = default;

      ~_Fwd_list_base()
      { _M_erase_after(_M_impl._M_head._M_base_ptr(), nullptr); }

    protected:
#if ! _GLIBCXX_INLINE_VERSION
      // XXX GLIBCXX_ABI Deprecated
      _Node*
      _M_get_node()
      {
	auto __ptr = _Node_alloc_traits::allocate(_M_get_Node_allocator(), 1);
	return std::__to_address(__ptr);
      }
#endif

      void
      _M_put_node(_Node_ptr __p)
      {
#if _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
	_Node_alloc_traits::deallocate(_M_get_Node_allocator(), __p, 1);
#else
	typedef typename _Node_alloc_traits::pointer _Ptr;
	auto __ptr = std::pointer_traits<_Ptr>::pointer_to(*__p);
	_Node_alloc_traits::deallocate(_M_get_Node_allocator(), __ptr, 1);
#endif
      }

      template<typename... _Args>
	_Node_ptr
	_M_create_node(_Args&&... __args)
	{
	  auto& __alloc = _M_get_Node_allocator();
	  auto __guard = std::__allocate_guarded_obj(__alloc);
	  _Node_alloc_traits::construct(__alloc, __guard->_M_valptr(),
					std::forward<_Args>(__args)...);
	  auto __p = __guard.release();
#if _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
	  return __p;
#else
	  return std::__to_address(__p);
#endif
	}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
      void
      _M_destroy_node(_Node_ptr __p)
      {
	auto& __alloc = _M_get_Node_allocator();
	// Destroy the element
	_Node_alloc_traits::destroy(__alloc, __p->_M_valptr());
	// Only destroy the node if the pointers require it.
	if constexpr (!is_trivially_destructible<_Base_ptr>::value)
	  __p->~_Node();
	_M_put_node(__p);
      }
#pragma GCC diagnostic pop

      template<typename... _Args>
	_Base_ptr
	_M_insert_after(const_iterator __pos, _Args&&... __args);

      _Base_ptr
      _M_erase_after(_Base_ptr __pos);

      _Base_ptr
      _M_erase_after(_Base_ptr __pos, _Base_ptr __last);
    };

  /**
   *  @brief A standard container with linear time access to elements,
   *  and fixed time insertion/deletion at any point in the sequence.
   *
   *  @ingroup sequences
   *  @headerfile forward_list
   *  @since C++11
   *
   *  @tparam _Tp  Type of element.
   *  @tparam _Alloc  Allocator type, defaults to allocator<_Tp>.
   *
   *  Meets the requirements of a <a href="tables.html#65">container</a>, a
   *  <a href="tables.html#67">sequence</a>, including the
   *  <a href="tables.html#68">optional sequence requirements</a> with the
   *  %exception of `at` and `operator[]`.
   *
   *  This is a @e singly @e linked %list.  Traversal up the
   *  %list requires linear time, but adding and removing elements (or
   *  @e nodes) is done in constant time, regardless of where the
   *  change takes place.  Unlike std::vector and std::deque,
   *  random-access iterators are not provided, so subscripting (`[]`)
   *  access is not allowed.  For algorithms which only need
   *  sequential access, this lack makes no difference.
   *
   *  Also unlike the other standard containers, std::forward_list provides
   *  specialized algorithms %unique to linked lists, such as
   *  splicing, sorting, and in-place reversal.
   */
  template<typename _Tp, typename _Alloc = allocator<_Tp>>
    class forward_list : private _Fwd_list_base<_Tp, _Alloc>
    {
      static_assert(is_same<typename remove_cv<_Tp>::type, _Tp>::value,
	  "std::forward_list must have a non-const, non-volatile value_type");
#if __cplusplus > 201703L || defined __STRICT_ANSI__
      static_assert(is_same<typename _Alloc::value_type, _Tp>::value,
	  "std::forward_list must have the same value_type as its allocator");
#endif

    private:
      typedef _Fwd_list_base<_Tp, _Alloc>		_Base;
      typedef _Fwd_list_node_base			_Node_base;
      typedef typename _Base::_Node			_Node;
      typedef typename _Base::_Node_alloc_type		_Node_alloc_type;
      typedef typename _Base::_Node_alloc_traits	_Node_alloc_traits;
      typedef allocator_traits<__alloc_rebind<_Alloc, _Tp>>	_Alloc_traits;

    public:
      // types:
      typedef _Tp					value_type;
      typedef typename _Alloc_traits::pointer		pointer;
      typedef typename _Alloc_traits::const_pointer	const_pointer;
      typedef value_type&				reference;
      typedef const value_type&				const_reference;

      typedef typename _Base::iterator			iterator;
      typedef typename _Base::const_iterator		const_iterator;
      typedef std::size_t				size_type;
      typedef std::ptrdiff_t				difference_type;
      typedef _Alloc					allocator_type;

      // 23.3.4.2 construct/copy/destroy:

      /**
       *  @brief  Creates a %forward_list with no elements.
       */
      forward_list() = default;

      /**
       *  @brief  Creates a %forward_list with no elements.
       *  @param  __al  An allocator object.
       */
      explicit
      forward_list(const _Alloc& __al) noexcept
      : _Base(_Node_alloc_type(__al))
      { }

      /**
       *  @brief  Copy constructor with allocator argument.
       *  @param  __list  Input list to copy.
       *  @param  __al    An allocator object.
       */
      forward_list(const forward_list& __list,
		   const __type_identity_t<_Alloc>& __al)
      : _Base(_Node_alloc_type(__al))
      { _M_range_initialize(__list.begin(), __list.end()); }

    private:
      forward_list(forward_list&& __list, _Node_alloc_type&& __al,
		   false_type)
      : _Base(std::move(__list), std::move(__al))
      {
	// If __list is not empty it means its allocator is not equal to __a,
	// so we need to move from each element individually.
	insert_after(cbefore_begin(),
		     std::__make_move_if_noexcept_iterator(__list.begin()),
		     std::__make_move_if_noexcept_iterator(__list.end()));
      }

      forward_list(forward_list&& __list, _Node_alloc_type&& __al,
		   true_type)
      noexcept
      : _Base(std::move(__list), _Node_alloc_type(__al), true_type{})
      { }

    public:
      /**
       *  @brief  Move constructor with allocator argument.
       *  @param  __list  Input list to move.
       *  @param  __al    An allocator object.
       */
      forward_list(forward_list&& __list,
		   const __type_identity_t<_Alloc>& __al)
      noexcept(_Node_alloc_traits::_S_always_equal())
      : forward_list(std::move(__list), _Node_alloc_type(__al),
		     typename _Node_alloc_traits::is_always_equal{})
      { }

      /**
       *  @brief  Creates a %forward_list with default constructed elements.
       *  @param  __n   The number of elements to initially create.
       *  @param  __al  An allocator object.
       *
       *  This constructor creates the %forward_list with `__n` default
       *  constructed elements.
       */
      explicit
      forward_list(size_type __n, const _Alloc& __al = _Alloc())
      : _Base(_Node_alloc_type(__al))
      { _M_default_initialize(__n); }

      /**
       *  @brief  Creates a %forward_list with copies of an exemplar element.
       *  @param  __n      The number of elements to initially create.
       *  @param  __value  An element to copy.
       *  @param  __al     An allocator object.
       *
       *  This constructor fills the %forward_list with `__n` copies of
       *  `__value`.
       */
      forward_list(size_type __n, const _Tp& __value,
		   const _Alloc& __al = _Alloc())
      : _Base(_Node_alloc_type(__al))
      { _M_fill_initialize(__n, __value); }

      /**
       *  @brief  Builds a %forward_list from a range.
       *  @param  __first  An input iterator.
       *  @param  __last   An input iterator.
       *  @param  __al     An allocator object.
       *
       *  Create a %forward_list consisting of copies of the elements from
       *  `[__first,__last)`.  This is linear in N (where N is
       *  `distance(__first,__last)`).
       */
      template<typename _InputIterator,
	       typename = std::_RequireInputIter<_InputIterator>>
	forward_list(_InputIterator __first, _InputIterator __last,
		     const _Alloc& __al = _Alloc())
	: _Base(_Node_alloc_type(__al))
	{ _M_range_initialize(__first, __last); }

#if __glibcxx_containers_ranges // C++ >= 23
      /**
       * @brief Construct a forward_list from a range.
       * @param __rg An input range with elements that are convertible to
       *             the forward_list's value_type.
       * @param __a An allocator.
       *
       * @since C++23
       */
      template<__detail::__container_compatible_range<_Tp> _Rg>
	forward_list(from_range_t, _Rg&& __rg, const _Alloc& __a = _Alloc())
	: _Base(_Node_alloc_type(__a))
	{
	  auto __to = this->_M_impl._M_head._M_base_ptr();
	  auto __first = ranges::begin(__rg);
	  const auto __last = ranges::end(__rg);
	  for (; __first != __last; ++__first)
	    {
	      __to->_M_next = this->_M_create_node(*__first)->_M_base_ptr();
	      __to = __to->_M_next;
	    }
	}
#endif // containers_ranges

      /**
       *  @brief  The %forward_list copy constructor.
       *  @param  __list  A %forward_list of identical element and allocator
       *                  types.
       */
      forward_list(const forward_list& __list)
      : _Base(_Node_alloc_traits::_S_select_on_copy(
		__list._M_get_Node_allocator()))
      { _M_range_initialize(__list.begin(), __list.end()); }

      /**
       *  @brief  The %forward_list move constructor.
       *  @param  __list  A %forward_list of identical element and allocator
       *                  types.
       *
       *  The newly-created %forward_list contains the exact contents of the
       *  moved instance. The contents of the moved instance are a valid, but
       *  unspecified %forward_list.
       */
      forward_list(forward_list&&) = default;

      /**
       *  @brief  Builds a %forward_list from an initializer_list
       *  @param  __il  An initializer_list of value_type.
       *  @param  __al  An allocator object.
       *
       *  Create a %forward_list consisting of copies of the elements
       *  in the initializer_list `__il`.  This is linear in `__il.size()`.
       */
      forward_list(std::initializer_list<_Tp> __il,
		   const _Alloc& __al = _Alloc())
      : _Base(_Node_alloc_type(__al))
      { _M_range_initialize(__il.begin(), __il.end()); }

      /**
       *  @brief  The forward_list dtor.
       */
      ~forward_list() noexcept
      { }

      /**
       *  @brief  The %forward_list assignment operator.
       *  @param  __list  A %forward_list of identical element and allocator
       *                types.
       *
       *  All the elements of `__list` are copied.
       *
       *  Whether the allocator is copied depends on the allocator traits.
       */
      forward_list&
      operator=(const forward_list& __list);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
      /**
       *  @brief  The %forward_list move assignment operator.
       *  @param  __list  A %forward_list of identical element and allocator
       *                types.
       *
       *  The contents of `__list` are moved into this %forward_list
       *  (without copying, if the allocators permit it).
       *
       *  Afterwards @a __list is a valid, but unspecified %forward_list
       *
       *  Whether the allocator is moved depends on the allocator traits.
       */
      forward_list&
      operator=(forward_list&& __list)
      noexcept(_Node_alloc_traits::_S_nothrow_move())
      {
	constexpr bool __move_storage =
	  _Node_alloc_traits::_S_propagate_on_move_assign()
	  || _Node_alloc_traits::_S_always_equal();
	if constexpr (!__move_storage)
	  {
	    if (__list._M_get_Node_allocator() != this->_M_get_Node_allocator())
	      {
		// The rvalue's allocator cannot be moved, or is not equal,
		// so we need to individually move each element.
		this->assign(std::make_move_iterator(__list.begin()),
			     std::make_move_iterator(__list.end()));
		return *this;
	      }
	  }

	clear();
	this->_M_impl._M_head._M_next = __list._M_impl._M_head._M_next;
	__list._M_impl._M_head._M_next = nullptr;
	if constexpr (_Node_alloc_traits::_S_propagate_on_move_assign())
	  this->_M_get_Node_allocator()
	      = std::move(__list._M_get_Node_allocator());
	return *this;
      }

      /**
       *  @brief  The %forward_list initializer list assignment operator.
       *  @param  __il  An initializer_list of value_type.
       *
       *  Replace the contents of the %forward_list with copies of the
       *  elements in the initializer_list `__il`.  This is linear in
       *  `__il.size()`.
       */
      forward_list&
      operator=(std::initializer_list<_Tp> __il)
      {
	assign(__il);
	return *this;
      }

      /**
       *  @brief  Assigns a range to a %forward_list.
       *  @param  __first  An input iterator.
       *  @param  __last   An input iterator.
       *
       *  This function fills a %forward_list with copies of the elements
       *  in the range `[ __first,__last)`.
       *
       *  Note that the assignment completely changes the %forward_list and
       *  that the number of elements of the resulting %forward_list is the
       *  same as the number of elements assigned.
       */
      template<typename _InputIterator,
	       typename = std::_RequireInputIter<_InputIterator>>
	void
	assign(_InputIterator __first, _InputIterator __last)
	{
	  if constexpr (is_assignable<_Tp, decltype(*__first)>::value)
	    {
	      auto __prev = before_begin();
	      auto __curr = begin();
	      auto __end = end();
	      while (__curr != __end && __first != __last)
		{
		  *__curr = *__first;
		  ++__prev;
		  ++__curr;
		  ++__first;
		}
	      if (__first != __last)
		insert_after(__prev, __first, __last);
	      else if (__curr != __end)
		erase_after(__prev, __end);
	    }
	  else
	    {
	      clear();
	      insert_after(cbefore_begin(), __first, __last);
	    }
	}
#pragma GCC diagnostic pop

#if __glibcxx_containers_ranges // C++ >= 23
      /**
       * @brief Assign a range to a forward_list.
       * @since C++23
       */
      template<__detail::__container_compatible_range<_Tp> _Rg>
	void
	assign_range(_Rg&& __rg)
	{
	  static_assert(assignable_from<_Tp&, ranges::range_reference_t<_Rg>>);

	  auto __first = ranges::begin(__rg);
	  const auto __last = ranges::end(__rg);
	  iterator __prev = before_begin();
	  iterator __curr = begin();
	  const iterator __end = end();

	  while (__curr != __end && __first != __last)
	    {
	      *__curr = *__first;
	      __prev = __curr;
	      ++__first;
	      ++__curr;
	    }

	  if (__curr != __end)
	    erase_after(__prev, __end);
	  else
	    insert_range_after(__prev,
			       ranges::subrange(std::move(__first), __last));
	}
#endif // containers_ranges

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
      /**
       *  @brief  Assigns a given value to a %forward_list.
       *  @param  __n  Number of elements to be assigned.
       *  @param  __val  Value to be assigned.
       *
       *  This function fills a %forward_list with `__n` copies of the
       *  given value.  Note that the assignment completely changes the
       *  %forward_list, and that the resulting %forward_list has `__n`
       *  elements.
       */
      void
      assign(size_type __n, const _Tp& __val)
      {
	if constexpr (is_copy_assignable<_Tp>::value)
	  {
	    auto __prev = before_begin();
	    auto __curr = begin();
	    auto __end = end();
	    while (__curr != __end && __n > 0)
	      {
		*__curr = __val;
		++__prev;
		++__curr;
		--__n;
	      }
	    if (__n > 0)
	      insert_after(__prev, __n, __val);
	    else if (__curr != __end)
	      erase_after(__prev, __end);
	  }
	else
	  {
	    clear();
	    insert_after(cbefore_begin(), __n, __val);
	  }
      }
#pragma GCC diagnostic pop

      /**
       *  @brief  Assigns an initializer_list to a %forward_list.
       *  @param  __il  An initializer_list of value_type.
       *
       *  Replace the contents of the %forward_list with copies of the
       *  elements in the initializer_list `__il`.  This is linear in
       *  `__il.size()`.
       */
      void
      assign(std::initializer_list<_Tp> __il)
      { assign(__il.begin(), __il.end()); }

      /// Get a copy of the memory allocation object.
      allocator_type
      get_allocator() const noexcept
      { return allocator_type(this->_M_get_Node_allocator()); }

      // 23.3.4.3 iterators:

      /**
       *  Returns a read/write iterator that points before the first element
       *  in the %forward_list.  Iteration is done in ordinary element order.
       */
      [[__nodiscard__]]
      iterator
      before_begin() noexcept
      { return iterator(this->_M_impl._M_head._M_base_ptr()); }

      /**
       *  Returns a read-only (constant) iterator that points before the
       *  first element in the %forward_list.  Iteration is done in ordinary
       *  element order.
       */
      [[__nodiscard__]]
      const_iterator
      before_begin() const noexcept
      { return const_iterator(this->_M_impl._M_head._M_base_ptr()); }

      /**
       *  Returns a read/write iterator that points to the first element
       *  in the %forward_list.  Iteration is done in ordinary element order.
       */
      [[__nodiscard__]]
      iterator
      begin() noexcept
      { return iterator(this->_M_impl._M_head._M_next); }

      /**
       *  Returns a read-only (constant) iterator that points to the first
       *  element in the %forward_list.  Iteration is done in ordinary
       *  element order.
       */
      [[__nodiscard__]]
      const_iterator
      begin() const noexcept
      { return const_iterator(this->_M_impl._M_head._M_next); }

      /**
       *  Returns a read/write iterator that points one past the last
       *  element in the %forward_list.  Iteration is done in ordinary
       *  element order.
       */
      [[__nodiscard__]]
      iterator
      end() noexcept
      { return iterator(nullptr); }

      /**
       *  Returns a read-only iterator that points one past the last
       *  element in the %forward_list.  Iteration is done in ordinary
       *  element order.
       */
      [[__nodiscard__]]
      const_iterator
      end() const noexcept
      { return const_iterator(nullptr); }

      /**
       *  Returns a read-only (constant) iterator that points to the
       *  first element in the %forward_list.  Iteration is done in ordinary
       *  element order.
       */
      [[__nodiscard__]]
      const_iterator
      cbegin() const noexcept
      { return const_iterator(this->_M_impl._M_head._M_next); }

      /**
       *  Returns a read-only (constant) iterator that points before the
       *  first element in the %forward_list.  Iteration is done in ordinary
       *  element order.
       */
      [[__nodiscard__]]
      const_iterator
      cbefore_begin() const noexcept
      { return const_iterator(this->_M_impl._M_head._M_base_ptr()); }

      /**
       *  Returns a read-only (constant) iterator that points one past
       *  the last element in the %forward_list.  Iteration is done in
       *  ordinary element order.
       */
      [[__nodiscard__]]
      const_iterator
      cend() const noexcept
      { return const_iterator(nullptr); }

      /**
       *  Returns true if the %forward_list is empty.  (Thus begin() would
       *  equal end().)
       */
      [[__nodiscard__]]
      bool
      empty() const noexcept
      { return this->_M_impl._M_head._M_next == nullptr; }

      /**
       *  Returns the largest possible number of elements of %forward_list.
       */
      [[__nodiscard__]]
      size_type
      max_size() const noexcept
      { return _Node_alloc_traits::max_size(this->_M_get_Node_allocator()); }

      // 23.3.4.4 element access:

      /**
       *  Returns a read/write reference to the data at the first
       *  element of the %forward_list.
       */
      [[__nodiscard__]]
      reference
      front()
      {
	__glibcxx_requires_nonempty();
	_Node& __front = static_cast<_Node&>(*this->_M_impl._M_head._M_next);
	return *__front._M_valptr();
      }

      /**
       *  Returns a read-only (constant) reference to the data at the first
       *  element of the %forward_list.
       */
      [[__nodiscard__]]
      const_reference
      front() const
      {
	__glibcxx_requires_nonempty();
	_Node& __front = static_cast<_Node&>(*this->_M_impl._M_head._M_next);
	return *__front._M_valptr();
      }

      // 23.3.4.5 modifiers:

      /**
       *  @brief  Constructs object in %forward_list at the front of the
       *          list.
       *  @param  __args  Arguments.
       *
       *  This function will insert an object of type `Tp` constructed
       *  with `Tp(std::forward<Args>(args)...)` at the front of the list
       *  Due to the nature of a %forward_list this operation can
       *  be done in constant time, and does not invalidate iterators
       *  and references.
       */
      template<typename... _Args>
#if __cplusplus > 201402L
	reference
#else
	void
#endif
	emplace_front(_Args&&... __args)
	{
	  this->_M_insert_after(cbefore_begin(),
				std::forward<_Args>(__args)...);
#if __cplusplus > 201402L
	  return front();
#endif
	}

      /**
       *  @brief  Add data to the front of the %forward_list.
       *  @param  __val  Data to be added.
       *
       *  This is a typical stack operation.  The function creates an
       *  element at the front of the %forward_list and assigns the given
       *  data to it.  Due to the nature of a %forward_list this operation
       *  can be done in constant time, and does not invalidate iterators
       *  and references.
       */
      void
      push_front(const _Tp& __val)
      { this->_M_insert_after(cbefore_begin(), __val); }

      /**
       *
       */
      void
      push_front(_Tp&& __val)
      { this->_M_insert_after(cbefore_begin(), std::move(__val)); }

#if __glibcxx_containers_ranges // C++ >= 23
      /**
       * @brief Insert a range at the beginning of a forward_list.
       * @param __rg An input range with elements that are convertible to
       *             the forward_list's value_type.
       *
       * The inserted elements will be in the same order as in the range,
       * so they are not reversed as would happen with a simple loop calling
       * `emplace_front` for each element of the range.
       *
       * No iterators to existing elements are invalidated by this function.
       * If the insertion fails due to an exception, no elements will be added
       * and so the list will be unchanged.
       *
       * @since C++23
       */
      template<__detail::__container_compatible_range<_Tp> _Rg>
	void
	prepend_range(_Rg&& __rg)
	{
	  forward_list __tmp(from_range, std::forward<_Rg>(__rg),
			     get_allocator());
	  if (!__tmp.empty())
	    splice_after(before_begin(), __tmp);
	}
#endif // containers_ranges

      /**
       *  @brief  Removes first element.
       *
       *  This is a typical stack operation.  It shrinks the %forward_list
       *  by one.  Due to the nature of a %forward_list this operation can
       *  be done in constant time, and only invalidates iterators/references
       *  to the element being removed.
       *
       *  Note that no data is returned, and if the first element's data
       *  is needed, it should be retrieved before `pop_front()` is
       *  called.
       */
      void
      pop_front()
      {
	__glibcxx_requires_nonempty();
	this->_M_erase_after(this->_M_impl._M_head._M_base_ptr());
      }

      /**
       *  @brief  Constructs object in %forward_list after the specified
       *          iterator.
       *  @param  __pos  A const_iterator into the %forward_list.
       *  @param  __args  Arguments.
       *  @return  An iterator that points to the inserted data.
       *
       *  This function will insert an object of type `T` constructed
       *  with `T(std::forward<Args>(args)...)` after the specified
       *  location.  Due to the nature of a %forward_list this operation can
       *  be done in constant time, and does not invalidate iterators
       *  and references.
       */
      template<typename... _Args>
	iterator
	emplace_after(const_iterator __pos, _Args&&... __args)
	{ return iterator(this->_M_insert_after(__pos,
					  std::forward<_Args>(__args)...)); }

      /**
       *  @brief  Inserts given value into %forward_list after specified
       *          iterator.
       *  @param  __pos  An iterator into the %forward_list.
       *  @param  __val  Data to be inserted.
       *  @return  An iterator that points to the inserted data.
       *
       *  This function will insert a copy of the given value after
       *  the specified location.  Due to the nature of a %forward_list this
       *  operation can be done in constant time, and does not
       *  invalidate iterators and references.
       */
      iterator
      insert_after(const_iterator __pos, const _Tp& __val)
      { return iterator(this->_M_insert_after(__pos, __val)); }

      /**
       *
       */
      iterator
      insert_after(const_iterator __pos, _Tp&& __val)
      { return iterator(this->_M_insert_after(__pos, std::move(__val))); }

      /**
       *  @brief  Inserts a number of copies of given data into the
       *          %forward_list.
       *  @param  __pos  An iterator into the %forward_list.
       *  @param  __n  Number of elements to be inserted.
       *  @param  __val  Data to be inserted.
       *  @return  An iterator pointing to the last inserted copy of
       *           `val` or `pos` if `n == 0`.
       *
       *  This function will insert a specified number of copies of the
       *  given data after the location specified by `pos`.
       *
       *  This operation is linear in the number of elements inserted and
       *  does not invalidate iterators and references.
       */
      iterator
      insert_after(const_iterator __pos, size_type __n, const _Tp& __val);

      /**
       *  @brief  Inserts a range into the %forward_list.
       *  @param  __pos  An iterator into the %forward_list.
       *  @param  __first  An input iterator.
       *  @param  __last   An input iterator.
       *  @return  An iterator pointing to the last inserted element or
       *           `__pos` if `__first ==  __last`.
       *
       *  This function will insert copies of the data in the range
       *  `[ __first, __last)` into the %forward_list after the
       *  location specified by `__pos.
       *
       *  This operation is linear in the number of elements inserted and
       *  does not invalidate iterators and references.
       */
      template<typename _InputIterator,
	       typename = std::_RequireInputIter<_InputIterator>>
	iterator
	insert_after(const_iterator __pos,
		     _InputIterator __first, _InputIterator __last);

      /**
       *  @brief  Inserts the contents of an initializer_list into
       *          %forward_list after the specified iterator.
       *  @param  __pos  An iterator into the %forward_list.
       *  @param  __il  An initializer_list of value_type.
       *  @return  An iterator pointing to the last inserted element
       *           or `__pos` if `__il` is empty.
       *
       *  This function will insert copies of the data in the
       *  initializer_list `__il` into the %forward_list before the location
       *  specified by `__pos`.
       *
       *  This operation is linear in the number of elements inserted and
       *  does not invalidate iterators and references.
       */
      iterator
      insert_after(const_iterator __pos, std::initializer_list<_Tp> __il)
      { return insert_after(__pos, __il.begin(), __il.end()); }

#if __glibcxx_containers_ranges // C++ >= 23
      /**
       * @brief Insert a rangeinto a forward_list.
       * @param  __position An iterator.
       * @param  __rg An input range of elements that can be converted to
       *              the forward_list's value type.
       * @return An iterator pointing to the last element inserted,
       *         or `__position` if the range is empty.
       *
       * Inserts the elements of `__rg` after `__position`.
       * No iterators to existing elements are invalidated by this function.
       * If the insertion fails due to an exception, no elements will be added
       * and so the list will be unchanged.
       *
       * @since C++23
       */
      template<__detail::__container_compatible_range<_Tp> _Rg>
	iterator
	insert_range_after(const_iterator __position, _Rg&& __rg)
	{
	  forward_list __tmp(from_range, std::forward<_Rg>(__rg),
			     get_allocator());
	  return _M_splice_after(__position, __tmp.before_begin(), __tmp.end());
	}
#endif // containers_ranges

      /**
       *  @brief  Removes the element pointed to by the iterator following
       *          `pos`.
       *  @param  __pos  Iterator pointing before element to be erased.
       *  @return  An iterator pointing to the element following the one
       *           that was erased, or `end()` if no such element exists.
       *
       *  This function will erase the element at the given position and
       *  thus shorten the %forward_list by one.
       *
       *  Due to the nature of a %forward_list this operation can be done
       *  in constant time, and only invalidates iterators/references to
       *  the element being removed.  The user is also cautioned that
       *  this function only erases the element, and that if the element
       *  is itself a pointer, the pointed-to memory is not touched in
       *  any way.  Managing the pointer is the user's responsibility.
       */
      iterator
      erase_after(const_iterator __pos)
      { return iterator(this->_M_erase_after(__pos._M_const_cast()._M_node)); }

      /**
       *  @brief  Remove a range of elements.
       *  @param  __pos  Iterator pointing before the first element to be
       *                 erased.
       *  @param  __last  Iterator pointing to one past the last element to be
       *                  erased.
       *  @return  `__last`
       *
       *  This function will erase the elements in the range
       *  `(__pos,__last)` and shorten the %forward_list accordingly.
       *
       *  This operation is linear time in the size of the range and only
       *  invalidates iterators/references to the element being removed.
       *
       *  The user is also cautioned that this function only erases the
       *  elements, and that if the elements themselves are pointers, the
       *  pointed-to memory is not touched in any way.  Managing the pointer
       *  is the user's responsibility.
       */
      iterator
      erase_after(const_iterator __pos, const_iterator __last)
      {
	return iterator(this->_M_erase_after(__pos._M_const_cast()._M_node,
					     __last._M_const_cast()._M_node));
      }

      /**
       *  @brief  Swaps data with another %forward_list.
       *  @param  __list  A %forward_list of the same element and allocator
       *                  types.
       *
       *  This exchanges the elements between two lists in constant
       *  time.  Note that the global `std::swap()` function is
       *  overloaded such that `std::swap(l1, l2)` will feed to this
       *  function.
       *
       *  Whether the allocators are swapped depends on the allocator traits.
       */
      void
      swap(forward_list& __list) noexcept
      {
	std::swap(this->_M_impl._M_head._M_next,
		  __list._M_impl._M_head._M_next);
	_Node_alloc_traits::_S_on_swap(this->_M_get_Node_allocator(),
				       __list._M_get_Node_allocator());
      }

      /**
       *  @brief Resizes the %forward_list to the specified number of
       *         elements.
       *  @param __sz Number of elements the %forward_list should contain.
       *
       *  This function will %resize the %forward_list to the specified
       *  number of elements.  If the number is smaller than the
       *  %forward_list's current number of elements the %forward_list
       *  is truncated, otherwise the %forward_list is extended and the
       *  new elements are default constructed.
       */
      void
      resize(size_type __sz);

      /**
       *  @brief Resizes the %forward_list to the specified number of
       *         elements.
       *  @param __sz Number of elements the %forward_list should contain.
       *  @param __val Data with which new elements should be populated.
       *
       *  This function will %resize the %forward_list to the specified
       *  number of elements.  If the number is smaller than the
       *  %forward_list's current number of elements the %forward_list
       *  is truncated, otherwise the %forward_list is extended and new
       *  elements are populated with given data.
       */
      void
      resize(size_type __sz, const value_type& __val);

      /**
       *  @brief  Erases all the elements.
       *
       *  Note that this function only erases
       *  the elements, and that if the elements themselves are
       *  pointers, the pointed-to memory is not touched in any way.
       *  Managing the pointer is the user's responsibility.
       */
      void
      clear() noexcept
      { this->_M_erase_after(this->_M_impl._M_head._M_base_ptr(), nullptr); }

      // 23.3.4.6 forward_list operations:

      /**
       *  @brief  Insert contents of another %forward_list.
       *  @param  __pos  Iterator referencing the element to insert after.
       *  @param  __list  Source list.
       *
       *  The elements of `list` are inserted in constant time after
       *  the element referenced by `pos`.  `list` becomes an empty
       *  list.
       *
       *  Requires `this != &x`.
       */
      void
      splice_after(const_iterator __pos, forward_list&& __list) noexcept
      {
	if (!__list.empty())
	  _M_splice_after(__pos, __list.before_begin(), __list.end());
      }

      void
      splice_after(const_iterator __pos, forward_list& __list) noexcept
      { splice_after(__pos, std::move(__list)); }

      /**
       *  @brief  Insert element from another %forward_list.
       *  @param  __pos  Iterator referencing the element to insert after.
       *  @param  __list  Source list.
       *  @param  __i   Iterator referencing the element before the element
       *                to move.
       *
       *  Removes the element in list `__list` referenced by `__i` and
       *  inserts it into the current list after `__pos`.
       */
      void
      splice_after(const_iterator __pos, forward_list&& __list,
		   const_iterator __i) noexcept;

      void
      splice_after(const_iterator __pos, forward_list& __list,
		   const_iterator __i) noexcept
      { splice_after(__pos, std::move(__list), __i); }

      /**
       *  @brief  Insert range from another %forward_list.
       *  @param  __pos  Iterator referencing the element to insert after.
       *  @param  __list  Source list.
       *  @param  __before  Iterator referencing before the start of range
       *                    in `__list`.
       *  @param  __last  Iterator referencing the end of range in `__list`.
       *
       *  Removes elements in the range `(__before,__last)` and inserts them
       *  after `__pos` in constant time.
       *
       *  Undefined if `__pos` is in `(__before,__last)`.
       *  @{
       */
      void
      splice_after(const_iterator __pos, forward_list&&,
		   const_iterator __before, const_iterator __last) noexcept
      { _M_splice_after(__pos, __before, __last); }

      void
      splice_after(const_iterator __pos, forward_list&,
		   const_iterator __before, const_iterator __last) noexcept
      { _M_splice_after(__pos, __before, __last); }
      /// @}

    private:
#ifdef __glibcxx_list_remove_return_type // C++20 && HOSTED
      using __remove_return_type = size_type;
# define _GLIBCXX_FWDLIST_REMOVE_RETURN_TYPE_TAG \
      __attribute__((__abi_tag__("__cxx20")))
#else
      using __remove_return_type = void;
# define _GLIBCXX_FWDLIST_REMOVE_RETURN_TYPE_TAG
#endif
    public:

      /**
       *  @brief  Remove all elements equal to value.
       *  @param  __val  The value to remove.
       *
       *  Removes every element in the list equal to `__val`.
       *  Remaining elements stay in list order.  Note that this
       *  function only erases the elements, and that if the elements
       *  themselves are pointers, the pointed-to memory is not
       *  touched in any way.  Managing the pointer is the user's
       *  responsibility.
       */
      _GLIBCXX_FWDLIST_REMOVE_RETURN_TYPE_TAG
      __remove_return_type
      remove(const _Tp& __val);

      /**
       *  @brief  Remove all elements satisfying a predicate.
       *  @param  __pred  Unary predicate function or object.
       *
       *  Removes every element in the list for which the predicate
       *  returns true.  Remaining elements stay in list order.  Note
       *  that this function only erases the elements, and that if the
       *  elements themselves are pointers, the pointed-to memory is
       *  not touched in any way.  Managing the pointer is the user's
       *  responsibility.
       */
      template<typename _Pred>
	__remove_return_type
	remove_if(_Pred __pred);

      /**
       *  @brief  Remove consecutive duplicate elements.
       *
       *  For each consecutive set of elements with the same value,
       *  remove all but the first one.  Remaining elements stay in
       *  list order.  Note that this function only erases the
       *  elements, and that if the elements themselves are pointers,
       *  the pointed-to memory is not touched in any way.  Managing
       *  the pointer is the user's responsibility.
       */
      _GLIBCXX_FWDLIST_REMOVE_RETURN_TYPE_TAG
      __remove_return_type
      unique()
      { return unique(std::equal_to<_Tp>()); }

#undef _GLIBCXX_FWDLIST_REMOVE_RETURN_TYPE_TAG

      /**
       *  @brief  Remove consecutive elements satisfying a predicate.
       *  @param  __binary_pred  Binary predicate function or object.
       *
       *  For each consecutive set of elements [first,last) that
       *  satisfy predicate(first,i) where i is an iterator in
       *  [first,last), remove all but the first one.  Remaining
       *  elements stay in list order.  Note that this function only
       *  erases the elements, and that if the elements themselves are
       *  pointers, the pointed-to memory is not touched in any way.
       *  Managing the pointer is the user's responsibility.
       */
      template<typename _BinPred>
	__remove_return_type
	unique(_BinPred __binary_pred);

      /**
       *  @brief  Merge sorted lists.
       *  @param  __list  Sorted list to merge.
       *
       *  Assumes that both `__list` and this list are sorted according to
       *  operator<().  Merges elements of `__list` into this list in
       *  sorted order, leaving `__list` empty when complete.  Elements in
       *  this list precede elements in `__list` that are equal.
       */
      void
      merge(forward_list&& __list)
      { merge(std::move(__list), std::less<_Tp>()); }

      void
      merge(forward_list& __list)
      { merge(std::move(__list)); }

      /**
       *  @brief  Merge sorted lists according to comparison function.
       *  @param  __list  Sorted list to merge.
       *  @param  __comp Comparison function defining sort order.
       *
       *  Assumes that both `__list` and this list are sorted according to
       *  comp.  Merges elements of `__list` into this list
       *  in sorted order, leaving `__list` empty when complete.  Elements
       *  in this list precede elements in `__list` that are equivalent
       *  according to comp().
       */
      template<typename _Comp>
	void
	merge(forward_list&& __list, _Comp __comp);

      template<typename _Comp>
	void
	merge(forward_list& __list, _Comp __comp)
	{ merge(std::move(__list), __comp); }

      /**
       *  @brief  Sort the elements of the list.
       *
       *  Sorts the elements of this list in NlogN time.  Equivalent
       *  elements remain in list order.
       */
      void
      sort()
      { sort(std::less<_Tp>()); }

      /**
       *  @brief  Sort the forward_list using a comparison function.
       *
       *  Sorts the elements of this list in NlogN time.  Equivalent
       *  elements remain in list order.
       */
      template<typename _Comp>
	void
	sort(_Comp __comp);

      /**
       *  @brief  Reverse the elements in list.
       *
       *  Reverse the order of elements in the list in linear time.
       */
      void
      reverse() noexcept
      { this->_M_impl._M_head._M_reverse_after(); }

    private:
      // Called by the range constructor to implement [23.3.4.2]/9
      template<typename _InputIterator>
	void
	_M_range_initialize(_InputIterator __first, _InputIterator __last);

      // Called by forward_list(n,v,a), and the range constructor when it
      // turns out to be the same thing.
      void
      _M_fill_initialize(size_type __n, const value_type& __value);

      // Called by splice_after and insert_after.
      iterator
      _M_splice_after(const_iterator __pos, const_iterator __before,
		      const_iterator __last);

      // Called by forward_list(n).
      void
      _M_default_initialize(size_type __n);

      // Called by resize(sz).
      void
      _M_default_insert_after(const_iterator __pos, size_type __n);

#if ! _GLIBCXX_INLINE_VERSION
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
      // XXX GLIBCXX_ABI Deprecated
      // These members are unused by std::forward_list now, but we keep them
      // here so that an explicit instantiation will define them.
      // This ensures that explicit instantiations still define these symbols,
      // so that explicit instantiation declarations of std::forward_list that
      // were compiled with old versions of GCC can still find these symbols.

      // Use 'if constexpr' so that the functions don't do anything for
      // specializations using _Node_traits<T, fancy-pointer>, because any
      // old code referencing these symbols wasn't using the fancy-pointer
      // specializations.

      void
      _M_move_assign(forward_list&& __list, true_type) noexcept
      {
#if _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
       if constexpr (is_same<typename _Alloc_traits::pointer, _Tp*>::value)
#endif
	 {
	   clear();
	   this->_M_impl._M_head._M_next = __list._M_impl._M_head._M_next;
	   __list._M_impl._M_head._M_next = nullptr;
	   std::__alloc_on_move(this->_M_get_Node_allocator(),
				__list._M_get_Node_allocator());
	 }
      }

      void
      _M_move_assign(forward_list&& __list, false_type)
      {
#if _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
	if constexpr (is_same<typename _Alloc_traits::pointer, _Tp*>::value)
#endif
	  {
	    if (__list._M_get_Node_allocator() == this->_M_get_Node_allocator())
	      _M_move_assign(std::move(__list), true_type());
	    else
	      // The rvalue's allocator cannot be moved, or is not equal,
	      // so we need to individually move each element.
	      this->assign(std::make_move_iterator(__list.begin()),
			   std::make_move_iterator(__list.end()));
	  }
      }

      void
      _M_assign_n(size_type __n, const _Tp& __val, true_type)
      {
#if _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
	if constexpr (is_same<typename _Alloc_traits::pointer, _Tp*>::value)
#endif
	  {
	    auto __prev = before_begin();
	    auto __curr = begin();
	    auto __end = end();
	    while (__curr != __end && __n > 0)
	      {
		*__curr = __val;
		++__prev;
		++__curr;
		--__n;
	      }
	    if (__n > 0)
	      insert_after(__prev, __n, __val);
	    else if (__curr != __end)
	      erase_after(__prev, __end);
	  }
      }

      void
      _M_assign_n(size_type __n, const _Tp& __val, false_type)
      {
#if _GLIBCXX_USE_ALLOC_PTR_FOR_FWD_LIST
	if constexpr (is_same<typename _Alloc_traits::pointer, _Tp*>::value)
#endif
	  {
	    clear();
	    insert_after(cbefore_begin(), __n, __val);
	  }
      }
#pragma GCC diagnostic pop
#endif // ! _GLIBCXX_INLINE_VERSION
    };

#if __cpp_deduction_guides >= 201606
  template<typename _InputIterator, typename _ValT
	     = typename iterator_traits<_InputIterator>::value_type,
	   typename _Allocator = allocator<_ValT>,
	   typename = _RequireInputIter<_InputIterator>,
	   typename = _RequireAllocator<_Allocator>>
    forward_list(_InputIterator, _InputIterator, _Allocator = _Allocator())
      -> forward_list<_ValT, _Allocator>;

#if __glibcxx_containers_ranges // C++ >= 23
  template<ranges::input_range _Rg,
	   typename _Allocator = allocator<ranges::range_value_t<_Rg>>>
    forward_list(from_range_t, _Rg&&, _Allocator = _Allocator())
      -> forward_list<ranges::range_value_t<_Rg>, _Allocator>;
#endif
#endif

  /**
   *  @brief  Forward list equality comparison.
   *  @param  __lx  A %forward_list
   *  @param  __ly  A %forward_list of the same type as `__lx`.
   *  @return  True iff the elements of the forward lists are equal.
   *
   *  This is an equivalence relation.  It is linear in the number of
   *  elements of the forward lists.  Deques are considered equivalent
   *  if corresponding elements compare equal.
   */
  template<typename _Tp, typename _Alloc>
    [[__nodiscard__]]
    bool
    operator==(const forward_list<_Tp, _Alloc>& __lx,
	       const forward_list<_Tp, _Alloc>& __ly);

#if __cpp_lib_three_way_comparison
  /**
   *  @brief  Forward list ordering relation.
   *  @param  __x  A `forward_list`.
   *  @param  __y  A `forward_list` of the same type as `__x`.
   *  @return  A value indicating whether `__x` is less than, equal to,
   *           greater than, or incomparable with `__y`.
   *
   *  See `std::lexicographical_compare_three_way()` for how the determination
   *  is made. This operator is used to synthesize relational operators like
   *  `<` and `>=` etc.
  */
  template<typename _Tp, typename _Alloc>
    [[nodiscard]]
    inline __detail::__synth3way_t<_Tp>
    operator<=>(const forward_list<_Tp, _Alloc>& __x,
		const forward_list<_Tp, _Alloc>& __y)
    {
      return std::lexicographical_compare_three_way(__x.begin(), __x.end(),
						    __y.begin(), __y.end(),
						    __detail::__synth3way);
    }
#else
  /**
   *  @brief  Forward list ordering relation.
   *  @param  __lx  A %forward_list.
   *  @param  __ly  A %forward_list of the same type as `__lx`.
   *  @return  True iff `__lx` is lexicographically less than `__ly`.
   *
   *  This is a total ordering relation.  It is linear in the number of
   *  elements of the forward lists.  The elements must be comparable
   *  with `<`.
   *
   *  See std::lexicographical_compare() for how the determination is made.
   */
  template<typename _Tp, typename _Alloc>
    [[__nodiscard__]]
    inline bool
    operator<(const forward_list<_Tp, _Alloc>& __lx,
	      const forward_list<_Tp, _Alloc>& __ly)
    { return std::lexicographical_compare(__lx.cbegin(), __lx.cend(),
					  __ly.cbegin(), __ly.cend()); }

  /// Based on operator==
  template<typename _Tp, typename _Alloc>
    [[__nodiscard__]]
    inline bool
    operator!=(const forward_list<_Tp, _Alloc>& __lx,
	       const forward_list<_Tp, _Alloc>& __ly)
    { return !(__lx == __ly); }

  /// Based on operator<
  template<typename _Tp, typename _Alloc>
    [[__nodiscard__]]
    inline bool
    operator>(const forward_list<_Tp, _Alloc>& __lx,
	      const forward_list<_Tp, _Alloc>& __ly)
    { return (__ly < __lx); }

  /// Based on operator<
  template<typename _Tp, typename _Alloc>
    [[__nodiscard__]]
    inline bool
    operator>=(const forward_list<_Tp, _Alloc>& __lx,
	       const forward_list<_Tp, _Alloc>& __ly)
    { return !(__lx < __ly); }

  /// Based on operator<
  template<typename _Tp, typename _Alloc>
    [[__nodiscard__]]
    inline bool
    operator<=(const forward_list<_Tp, _Alloc>& __lx,
	       const forward_list<_Tp, _Alloc>& __ly)
    { return !(__ly < __lx); }
#endif // three-way comparison

  /// See std::forward_list::swap().
  template<typename _Tp, typename _Alloc>
    inline void
    swap(forward_list<_Tp, _Alloc>& __lx,
	 forward_list<_Tp, _Alloc>& __ly)
    noexcept(noexcept(__lx.swap(__ly)))
    { __lx.swap(__ly); }

_GLIBCXX_END_NAMESPACE_CONTAINER
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _FORWARD_LIST_H
