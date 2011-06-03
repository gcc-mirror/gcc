// Allocators -*- C++ -*-

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
// 2011 Free Software Foundation, Inc.
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
 * Copyright (c) 1996-1997
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

/** @file bits/allocator.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _ALLOCATOR_H
#define _ALLOCATOR_H 1

// Define the base class to std::allocator.
#include <bits/c++allocator.h>

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <bits/ptr_traits.h>
#include <bits/uses_allocator.h>
#include <limits>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @defgroup allocators Allocators
   * @ingroup memory
   *
   * Classes encapsulating memory operations.
   */

  template<typename _Tp>
    class allocator;

  /// allocator<void> specialization.
  template<>
    class allocator<void>
    {
    public:
      typedef size_t      size_type;
      typedef ptrdiff_t   difference_type;
      typedef void*       pointer;
      typedef const void* const_pointer;
      typedef void        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };
    };

  /**
   * @brief  The @a standard allocator, as per [20.4].
   * @ingroup allocators
   *
   *  See http://gcc.gnu.org/onlinedocs/libstdc++/manual/bk01pt04ch11.html
   *  for further details.
   */
  template<typename _Tp>
    class allocator: public __glibcxx_base_allocator<_Tp>
    {
   public:
      typedef size_t     size_type;
      typedef ptrdiff_t  difference_type;
      typedef _Tp*       pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp&       reference;
      typedef const _Tp& const_reference;
      typedef _Tp        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };

      allocator() throw() { }

      allocator(const allocator& __a) throw()
      : __glibcxx_base_allocator<_Tp>(__a) { }

      template<typename _Tp1>
        allocator(const allocator<_Tp1>&) throw() { }

      ~allocator() throw() { }

      // Inherit everything else.
    };

  template<typename _T1, typename _T2>
    inline bool
    operator==(const allocator<_T1>&, const allocator<_T2>&)
    { return true; }

  template<typename _Tp>
    inline bool
    operator==(const allocator<_Tp>&, const allocator<_Tp>&)
    { return true; }

  template<typename _T1, typename _T2>
    inline bool
    operator!=(const allocator<_T1>&, const allocator<_T2>&)
    { return false; }

  template<typename _Tp>
    inline bool
    operator!=(const allocator<_Tp>&, const allocator<_Tp>&)
    { return false; }

  // Inhibit implicit instantiations for required instantiations,
  // which are defined via explicit instantiations elsewhere.
#if _GLIBCXX_EXTERN_TEMPLATE
  extern template class allocator<char>;
  extern template class allocator<wchar_t>;
#endif

  // Undefine.
#undef __glibcxx_base_allocator

  // To implement Option 3 of DR 431.
  template<typename _Alloc, bool = __is_empty(_Alloc)>
    struct __alloc_swap
    { static void _S_do_it(_Alloc&, _Alloc&) { } };

  template<typename _Alloc>
    struct __alloc_swap<_Alloc, false>
    {
      static void
      _S_do_it(_Alloc& __one, _Alloc& __two)
      {
	// Precondition: swappable allocators.
	if (__one != __two)
	  swap(__one, __two);
      }
    };

  // Optimize for stateless allocators.
  template<typename _Alloc, bool = __is_empty(_Alloc)>
    struct __alloc_neq
    {
      static bool
      _S_do_it(const _Alloc&, const _Alloc&)
      { return false; }
    };

  template<typename _Alloc>
    struct __alloc_neq<_Alloc, false>
    {
      static bool
      _S_do_it(const _Alloc& __one, const _Alloc& __two)
      { return __one != __two; }
    };

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  // A very basic implementation for now.  In general we have to wait for
  // the availability of the infrastructure described in N2983:  we should
  // try when either T has a move constructor which cannot throw or T is
  // CopyConstructible.
  // NB: This code doesn't properly belong here, we should find a more
  // suited place common to std::vector and std::deque.
  template<typename _Tp,
	   bool = __has_trivial_copy(typename _Tp::value_type)>
    struct __shrink_to_fit
    { static void _S_do_it(_Tp&) { } };

  template<typename _Tp>
    struct __shrink_to_fit<_Tp, true>
    {
      static void
      _S_do_it(_Tp& __v)
      {
	__try
	  { _Tp(__v).swap(__v); }
	__catch(...) { }
      }
    };

  template<typename _Alloc, typename _Tp>
    class __alloctr_rebind_helper
    {
      template<typename _Alloc2, typename _Tp2>
	static constexpr bool
       	_S_chk(typename _Alloc2::template rebind<_Tp2>::other*)
	{ return true; }

      template<typename, typename>
        static constexpr bool
       	_S_chk(...)
       	{ return false; }

    public:
      static const bool __value = _S_chk<_Alloc, _Tp>(nullptr);
    };

  template<typename _Alloc, typename _Tp,
           bool = __alloctr_rebind_helper<_Alloc, _Tp>::__value>
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

// TODO: Use pointer_traits::rebind alias template.

_GLIBCXX_ALLOC_TR_NESTED_TYPE(const_pointer,
  typename pointer_traits<pointer>::template __rebind<const value_type>::__type)

      /**
       * @brief   The allocator's const pointer type.
       *
       * @c Alloc::const_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<const value_type> </tt>
      */
      typedef __const_pointer const_pointer;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(void_pointer,
  typename pointer_traits<pointer>::template __rebind<void>::__type)

      /**
       * @brief   The allocator's void pointer type.
       *
       * @c Alloc::void_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<void> </tt>
      */
      typedef __void_pointer void_pointer;

_GLIBCXX_ALLOC_TR_NESTED_TYPE(const_void_pointer,
  typename pointer_traits<pointer>::template __rebind<const void>::__type)

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

      /* TODO: use template alias 
      template<typename _Tp>
        using rebind_alloc = __alloctr_rebind<_Alloc, _Tp>::__type;
      template<typename _Tp>
        using rebind_traits = allocator_traits<rebind_alloc<_Tp>>;
      */
      template<typename _Tp>
        struct __rebind_alloc
	{
  	  typedef typename __alloctr_rebind<_Alloc, _Tp>::__type __type;
       	};

      template<typename _Tp>
        struct __rebind_traits
	{
	  typedef allocator_traits<typename __rebind_alloc<_Tp>::__type> __type;
	};

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

	  typedef decltype(__test<_Alloc>(0)) type;
	  static const bool value = type::value;
	};

      template<typename _Alloc2>
	static typename
       	enable_if<__allocate_helper<_Alloc2>::value, pointer>::type
       	_S_allocate(_Alloc2& __a, size_type __n, const_void_pointer __hint)
	{ return __a.allocate(__n, __hint); }

      template<typename _Alloc2>
	static typename
       	enable_if<!__allocate_helper<_Alloc2>::value, pointer>::type
       	_S_allocate(_Alloc2& __a, size_type __n, ...)
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

	  typedef decltype(__test<_Alloc>(0)) type;
	  static const bool value = type::value;
	};

      template<typename _Tp, typename... _Args>
	static typename
       	enable_if<__construct_helper<_Tp, _Args...>::value, void>::type
       	_S_construct(_Alloc& __a, _Tp* __p, _Args&&... __args)
	{ __a.construct(__p, std::forward<_Args>(__args)...); }

      template<typename _Tp, typename... _Args>
	static typename
       	enable_if<!__construct_helper<_Tp, _Args...>::value, void>::type
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

	  typedef decltype(__test<_Alloc>(0)) type;
	  static const bool value = type::value;
	};

      template<typename _Tp>
	static typename enable_if<__destroy_helper<_Tp>::value, void>::type
       	_S_destroy(_Alloc& __a, _Tp* __p)
	{ __a.destroy(__p); }

      template<typename _Tp>
	static typename enable_if<!__destroy_helper<_Tp>::value, void>::type
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

	  typedef decltype(__test<_Alloc2>(0)) type;
	  static const bool value = type::value;
	};

      template<typename _Alloc2>
	static typename
       	enable_if<__maxsize_helper<_Alloc2>::value, size_type>::type
       	_S_max_size(_Alloc2& __a)
	{ return __a.max_size(); }

      template<typename _Alloc2>
	static typename
       	enable_if<!__maxsize_helper<_Alloc2>::value, size_type>::type
	_S_max_size(_Alloc2&)
	{ return numeric_limits<size_type>::max(); }

      template<typename _Alloc2>
	struct __select_helper
	{
	  template<typename _Alloc3, typename
	    = decltype(std::declval<_Alloc3*>()
		->select_on_container_copy_construction())>
	    static true_type __test(int);

	  template<typename>
	    static false_type __test(...);

	  typedef decltype(__test<_Alloc2>(0)) type;
	  static const bool value = type::value;
	};
      template<typename _Alloc2>
	static typename
       	enable_if<__select_helper<_Alloc2>::value, _Alloc2>::type
       	_S_select(_Alloc2& __a)
	{ return __a.select_on_container_copy_construction(); }

      template<typename _Alloc2>
	static typename
       	enable_if<!__select_helper<_Alloc2>::value, _Alloc2>::type
       	_S_select(_Alloc2& __a)
	{ return __a; }

    public:

      /**
       *  @brief  Allocate memory.
       *  @param  a  An allocator.
       *  @param  n  The number of objects to allocate space for.
       *
       *  Calls @c a.allocate(n)
      */
      static pointer
      allocate(_Alloc& __a, size_type __n)
      { return __a.allocate(__n); }

      /**
       *  @brief  Allocate memory.
       *  @param  a  An allocator.
       *  @param  n  The number of objects to allocate space for.
       *  @param  hint Aid to locality.
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
       *  @param  a  An allocator.
       *  @param  p  Pointer to the memory to deallocate.
       *  @param  n  The number of objects space was allocated for.
       *
       *  Calls <tt> a.deallocate(p, n) </tt>
      */
      static void deallocate(_Alloc& __a, pointer __p, size_type __n)
      { __a.deallocate(__p, __n); }

      /**
       *  @brief  Construct an object of type @a Tp
       *  @param  a  An allocator.
       *  @param  p  Pointer to memory of suitable size and alignment for Tp
       *  @param  args Constructor arguments.
       *
       *  Calls <tt> a.construct(p, std::forward<Args>(args)...) </tt>
       *  if that expression is well-formed, otherwise uses placement-new
       *  to construct an object of type @a Tp at location @a p from the
       *  arguments @a args...
      */
      template<typename _Tp, typename... _Args>
	static void construct(_Alloc& __a, _Tp* __p, _Args&&... __args)
	{ _S_construct(__a, __p, std::forward<_Args>(__args)...); }

      /**
       *  @brief  Destroy an object of type @a Tp
       *  @param  a  An allocator.
       *  @param  p  Pointer to the object to destroy
       *
       *  Calls @c a.destroy(p) if that expression is well-formed,
       *  otherwise calls @c p->~Tp()
      */
      template <class _Tp>
	static void destroy(_Alloc& __a, _Tp* __p)
	{ _S_destroy(__a, __p); }

      /**
       *  @brief  The maximum supported allocation size
       *  @param  a  An allocator.
       *  @return @c a.max_size() or @c %numeric_limits<size_type>::max()
       *
       *  Returns @c a.max_size() if that expression is well-formed,
       *  otherwise returns @c %numeric_limits<size_type>::max()
      */
      static size_type max_size(const _Alloc& __a)
      { return _S_max_size(__a); }

      /**
       *  @brief  Obtain an allocator to use when copying a container.
       *  @param  rhs  An allocator.
       *  @return @c rhs.select_on_container_copy_construction() or @a rhs
       *
       *  Returns @c rhs.select_on_container_copy_construction() if that
       *  expression is well-formed, otherwise returns @a rhs
      */
      static _Alloc
      select_on_container_copy_construction(const _Alloc& __rhs)
      { return _S_select(__rhs); }
    };

#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif
