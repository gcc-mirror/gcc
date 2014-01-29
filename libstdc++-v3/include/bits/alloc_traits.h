// Allocator traits -*- C++ -*-

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

/** @file bits/alloc_traits.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _ALLOC_TRAITS_H
#define _ALLOC_TRAITS_H 1

#if __cplusplus >= 201103L

#include <bits/memoryfwd.h>
#include <bits/ptr_traits.h>
#include <ext/numeric_traits.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _Alloc, typename _Tp>
    class __alloctr_rebind_helper
    {
      template<typename _Alloc2, typename _Tp2>
	static constexpr true_type
	_S_chk(typename _Alloc2::template rebind<_Tp2>::other*);

      template<typename, typename>
	static constexpr false_type
	_S_chk(...);

    public:
      using __type = decltype(_S_chk<_Alloc, _Tp>(nullptr));
    };

  template<typename _Alloc, typename _Tp,
	   bool = __alloctr_rebind_helper<_Alloc, _Tp>::__type::value>
    struct __alloctr_rebind;

  template<typename _Alloc, typename _Tp>
    struct __alloctr_rebind<_Alloc, _Tp, true>
    {
      typedef typename _Alloc::template rebind<_Tp>::other __type;
    };

  template<template<typename, typename...> class _Alloc, typename _Tp,
	   typename _Up, typename... _Args>
    struct __alloctr_rebind<_Alloc<_Up, _Args...>, _Tp, false>
    {
      typedef _Alloc<_Tp, _Args...> __type;
    };

  /**
   * @brief  Uniform interface to all allocator types.
   * @ingroup allocators
  */
  template<typename _Alloc>
    struct allocator_traits
    {
      /// The allocator type
      typedef _Alloc allocator_type;
      /// The allocated type
      typedef typename _Alloc::value_type value_type;

#define _GLIBCXX_ALLOC_TR_NESTED_TYPE(_NTYPE, _ALT) \
  private: \
  template<typename _Tp> \
    static typename _Tp::_NTYPE _S_##_NTYPE##_helper(_Tp*); \
  static _ALT _S_##_NTYPE##_helper(...); \
    typedef decltype(_S_##_NTYPE##_helper((_Alloc*)0)) __##_NTYPE; \
  public:

_GLIBCXX_ALLOC_TR_NESTED_TYPE(pointer, value_type*)

      /**
       * @brief   The allocator's pointer type.
       *
       * @c Alloc::pointer if that type exists, otherwise @c value_type*
      */
      typedef __pointer pointer;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(const_pointer,
  typename pointer_traits<pointer>::template rebind<const value_type>)

      /**
       * @brief   The allocator's const pointer type.
       *
       * @c Alloc::const_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<const value_type> </tt>
      */
      typedef __const_pointer const_pointer;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(void_pointer,
  typename pointer_traits<pointer>::template rebind<void>)

      /**
       * @brief   The allocator's void pointer type.
       *
       * @c Alloc::void_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<void> </tt>
      */
      typedef __void_pointer void_pointer;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(const_void_pointer,
  typename pointer_traits<pointer>::template rebind<const void>)

      /**
       * @brief   The allocator's const void pointer type.
       *
       * @c Alloc::const_void_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<const void> </tt>
      */
      typedef __const_void_pointer const_void_pointer;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(difference_type,
			      typename pointer_traits<pointer>::difference_type)

      /**
       * @brief   The allocator's difference type
       *
       * @c Alloc::difference_type if that type exists, otherwise
       * <tt> pointer_traits<pointer>::difference_type </tt>
      */
      typedef __difference_type difference_type;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(size_type,
			      typename make_unsigned<difference_type>::type)

      /**
       * @brief   The allocator's size type
       *
       * @c Alloc::size_type if that type exists, otherwise
       * <tt> make_unsigned<difference_type>::type </tt>
      */
      typedef __size_type size_type;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(propagate_on_container_copy_assignment,
			      false_type)

      /**
       * @brief   How the allocator is propagated on copy assignment
       *
       * @c Alloc::propagate_on_container_copy_assignment if that type exists,
       * otherwise @c false_type
      */
      typedef __propagate_on_container_copy_assignment
	propagate_on_container_copy_assignment;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(propagate_on_container_move_assignment,
			      false_type)

      /**
       * @brief   How the allocator is propagated on move assignment
       *
       * @c Alloc::propagate_on_container_move_assignment if that type exists,
       * otherwise @c false_type
      */
      typedef __propagate_on_container_move_assignment
	propagate_on_container_move_assignment;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(propagate_on_container_swap,
			      false_type)

      /**
       * @brief   How the allocator is propagated on swap
       *
       * @c Alloc::propagate_on_container_swap if that type exists,
       * otherwise @c false_type
      */
      typedef __propagate_on_container_swap propagate_on_container_swap;

#undef _GLIBCXX_ALLOC_TR_NESTED_TYPE

      template<typename _Tp>
	using rebind_alloc = typename __alloctr_rebind<_Alloc, _Tp>::__type;
      template<typename _Tp>
	using rebind_traits = allocator_traits<rebind_alloc<_Tp>>;

    private:
      template<typename _Alloc2>
	struct __allocate_helper
	{
	  template<typename _Alloc3,
	    typename = decltype(std::declval<_Alloc3*>()->allocate(
		  std::declval<size_type>(),
		  std::declval<const_void_pointer>()))>
	    static true_type __test(int);

	  template<typename>
	    static false_type __test(...);

	  using type = decltype(__test<_Alloc>(0));
	};

      template<typename _Alloc2>
	using __has_allocate = typename __allocate_helper<_Alloc2>::type;

      template<typename _Alloc2,
	       typename = _Require<__has_allocate<_Alloc2>>>
	static pointer
	_S_allocate(_Alloc2& __a, size_type __n, const_void_pointer __hint)
	{ return __a.allocate(__n, __hint); }

      template<typename _Alloc2, typename _UnusedHint,
	       typename = _Require<__not_<__has_allocate<_Alloc2>>>>
	static pointer
	_S_allocate(_Alloc2& __a, size_type __n, _UnusedHint)
	{ return __a.allocate(__n); }

      template<typename _Tp, typename... _Args>
	struct __construct_helper
	{
	  template<typename _Alloc2,
	    typename = decltype(std::declval<_Alloc2*>()->construct(
		  std::declval<_Tp*>(), std::declval<_Args>()...))>
	    static true_type __test(int);

	  template<typename>
	    static false_type __test(...);

	  using type = decltype(__test<_Alloc>(0));
	};

      template<typename _Tp, typename... _Args>
	using __has_construct
	  = typename __construct_helper<_Tp, _Args...>::type;

      template<typename _Tp, typename... _Args>
	static _Require<__has_construct<_Tp, _Args...>>
	_S_construct(_Alloc& __a, _Tp* __p, _Args&&... __args)
	{ __a.construct(__p, std::forward<_Args>(__args)...); }

      template<typename _Tp, typename... _Args>
	static
	_Require<__and_<__not_<__has_construct<_Tp, _Args...>>,
			       is_constructible<_Tp, _Args...>>>
	_S_construct(_Alloc&, _Tp* __p, _Args&&... __args)
	{ ::new((void*)__p) _Tp(std::forward<_Args>(__args)...); }

      template<typename _Tp>
	struct __destroy_helper
	{
	  template<typename _Alloc2,
	    typename = decltype(std::declval<_Alloc2*>()->destroy(
		  std::declval<_Tp*>()))>
	    static true_type __test(int);

	  template<typename>
	    static false_type __test(...);

	  using type = decltype(__test<_Alloc>(0));
	};

      template<typename _Tp>
	using __has_destroy = typename __destroy_helper<_Tp>::type;

      template<typename _Tp>
	static _Require<__has_destroy<_Tp>>
	_S_destroy(_Alloc& __a, _Tp* __p)
	{ __a.destroy(__p); }

      template<typename _Tp>
	static _Require<__not_<__has_destroy<_Tp>>>
	_S_destroy(_Alloc&, _Tp* __p)
	{ __p->~_Tp(); }

      template<typename _Alloc2>
	struct __maxsize_helper
	{
	  template<typename _Alloc3,
	    typename = decltype(std::declval<_Alloc3*>()->max_size())>
	    static true_type __test(int);

	  template<typename>
	    static false_type __test(...);

	  using type = decltype(__test<_Alloc2>(0));
	};

      template<typename _Alloc2>
	using __has_max_size = typename __maxsize_helper<_Alloc2>::type;

      template<typename _Alloc2,
	       typename = _Require<__has_max_size<_Alloc2>>>
	static size_type
	_S_max_size(_Alloc2& __a, int)
	{ return __a.max_size(); }

      template<typename _Alloc2,
	       typename = _Require<__not_<__has_max_size<_Alloc2>>>>
	static size_type
	_S_max_size(_Alloc2&, ...)
	{ return __gnu_cxx::__numeric_traits<size_type>::__max; }

      template<typename _Alloc2>
	struct __select_helper
	{
	  template<typename _Alloc3, typename
	    = decltype(std::declval<_Alloc3*>()
		->select_on_container_copy_construction())>
	    static true_type __test(int);

	  template<typename>
	    static false_type __test(...);

	  using type = decltype(__test<_Alloc2>(0));
	};

      template<typename _Alloc2>
	using __has_soccc = typename __select_helper<_Alloc2>::type;

      template<typename _Alloc2,
	       typename = _Require<__has_soccc<_Alloc2>>>
	static _Alloc2
	_S_select(_Alloc2& __a, int)
	{ return __a.select_on_container_copy_construction(); }

      template<typename _Alloc2,
	       typename = _Require<__not_<__has_soccc<_Alloc2>>>>
	static _Alloc2
	_S_select(_Alloc2& __a, ...)
	{ return __a; }

    public:

      /**
       *  @brief  Allocate memory.
       *  @param  __a  An allocator.
       *  @param  __n  The number of objects to allocate space for.
       *
       *  Calls @c a.allocate(n)
      */
      static pointer
      allocate(_Alloc& __a, size_type __n)
      { return __a.allocate(__n); }

      /**
       *  @brief  Allocate memory.
       *  @param  __a  An allocator.
       *  @param  __n  The number of objects to allocate space for.
       *  @param  __hint Aid to locality.
       *  @return Memory of suitable size and alignment for @a n objects
       *          of type @c value_type
       *
       *  Returns <tt> a.allocate(n, hint) </tt> if that expression is
       *  well-formed, otherwise returns @c a.allocate(n)
      */
      static pointer
      allocate(_Alloc& __a, size_type __n, const_void_pointer __hint)
      { return _S_allocate(__a, __n, __hint); }

      /**
       *  @brief  Deallocate memory.
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the memory to deallocate.
       *  @param  __n  The number of objects space was allocated for.
       *
       *  Calls <tt> a.deallocate(p, n) </tt>
      */
      static void deallocate(_Alloc& __a, pointer __p, size_type __n)
      { __a.deallocate(__p, __n); }

      /**
       *  @brief  Construct an object of type @a _Tp
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to memory of suitable size and alignment for Tp
       *  @param  __args Constructor arguments.
       *
       *  Calls <tt> __a.construct(__p, std::forward<Args>(__args)...) </tt>
       *  if that expression is well-formed, otherwise uses placement-new
       *  to construct an object of type @a _Tp at location @a __p from the
       *  arguments @a __args...
      */
      template<typename _Tp, typename... _Args>
	static auto construct(_Alloc& __a, _Tp* __p, _Args&&... __args)
	-> decltype(_S_construct(__a, __p, std::forward<_Args>(__args)...))
	{ _S_construct(__a, __p, std::forward<_Args>(__args)...); }

      /**
       *  @brief  Destroy an object of type @a _Tp
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the object to destroy
       *
       *  Calls @c __a.destroy(__p) if that expression is well-formed,
       *  otherwise calls @c __p->~_Tp()
      */
      template <class _Tp>
	static void destroy(_Alloc& __a, _Tp* __p)
	{ _S_destroy(__a, __p); }

      /**
       *  @brief  The maximum supported allocation size
       *  @param  __a  An allocator.
       *  @return @c __a.max_size() or @c numeric_limits<size_type>::max()
       *
       *  Returns @c __a.max_size() if that expression is well-formed,
       *  otherwise returns @c numeric_limits<size_type>::max()
      */
      static size_type max_size(const _Alloc& __a) noexcept
      { return _S_max_size(__a, 0); }

      /**
       *  @brief  Obtain an allocator to use when copying a container.
       *  @param  __rhs  An allocator.
       *  @return @c __rhs.select_on_container_copy_construction() or @a __rhs
       *
       *  Returns @c __rhs.select_on_container_copy_construction() if that
       *  expression is well-formed, otherwise returns @a __rhs
      */
      static _Alloc
      select_on_container_copy_construction(const _Alloc& __rhs)
      { return _S_select(__rhs, 0); }
    };

  template<typename _Alloc>
    inline void
    __do_alloc_on_copy(_Alloc& __one, const _Alloc& __two, true_type)
    { __one = __two; }

  template<typename _Alloc>
    inline void
    __do_alloc_on_copy(_Alloc&, const _Alloc&, false_type)
    { }

  template<typename _Alloc>
    inline void __alloc_on_copy(_Alloc& __one, const _Alloc& __two)
    {
      typedef allocator_traits<_Alloc> __traits;
      typedef typename __traits::propagate_on_container_copy_assignment __pocca;
      __do_alloc_on_copy(__one, __two, __pocca());
    }

  template<typename _Alloc>
    inline _Alloc __alloc_on_copy(const _Alloc& __a)
    {
      typedef allocator_traits<_Alloc> __traits;
      return __traits::select_on_container_copy_construction(__a);
    }

  template<typename _Alloc>
    inline void __do_alloc_on_move(_Alloc& __one, _Alloc& __two, true_type)
    { __one = std::move(__two); }

  template<typename _Alloc>
    inline void __do_alloc_on_move(_Alloc&, _Alloc&, false_type)
    { }

  template<typename _Alloc>
    inline void __alloc_on_move(_Alloc& __one, _Alloc& __two)
    {
      typedef allocator_traits<_Alloc> __traits;
      typedef typename __traits::propagate_on_container_move_assignment __pocma;
      __do_alloc_on_move(__one, __two, __pocma());
    }

  template<typename _Alloc>
    inline void __do_alloc_on_swap(_Alloc& __one, _Alloc& __two, true_type)
    {
      using std::swap;
      swap(__one, __two);
    }

  template<typename _Alloc>
    inline void __do_alloc_on_swap(_Alloc&, _Alloc&, false_type)
    { }

  template<typename _Alloc>
    inline void __alloc_on_swap(_Alloc& __one, _Alloc& __two)
    {
      typedef allocator_traits<_Alloc> __traits;
      typedef typename __traits::propagate_on_container_swap __pocs;
      __do_alloc_on_swap(__one, __two, __pocs());
    }

  template<typename _Alloc>
    class __is_copy_insertable_impl
    {
      typedef allocator_traits<_Alloc> _Traits;

      template<typename _Up, typename
	       = decltype(_Traits::construct(std::declval<_Alloc&>(),
					     std::declval<_Up*>(),
					     std::declval<const _Up&>()))>
	static true_type
	_M_select(int);

      template<typename _Up>
	static false_type
	_M_select(...);

    public:
      typedef decltype(_M_select<typename _Alloc::value_type>(0)) type;
    };

  // true if _Alloc::value_type is CopyInsertable into containers using _Alloc
  template<typename _Alloc>
    struct __is_copy_insertable
    : __is_copy_insertable_impl<_Alloc>::type
    { };

  // std::allocator<_Tp> just requires CopyConstructible
  template<typename _Tp>
    struct __is_copy_insertable<allocator<_Tp>>
    : is_copy_constructible<_Tp>
    { };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif
#endif
