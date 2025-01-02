// Allocator traits -*- C++ -*-

// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

#include <bits/stl_construct.h>
#include <bits/memoryfwd.h>
#if __cplusplus >= 201103L
# include <bits/ptr_traits.h>
# include <ext/numeric_traits.h>
# if _GLIBCXX_HOSTED
#  include <bits/allocator.h>
# endif
# if __cpp_exceptions
#  include <bits/stl_iterator.h> // __make_move_if_noexcept_iterator
# endif
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#if __cplusplus >= 201103L

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++14-extensions" // for variable templates
#pragma GCC diagnostic ignored "-Wc++17-extensions" // for if-constexpr

  /// @cond undocumented
  struct __allocator_traits_base
  {
#if __cpp_concepts
    template<typename _Tp, typename _Up>
#else
    template<typename _Tp, typename _Up, typename = void>
#endif
      struct __rebind : __replace_first_arg<_Tp, _Up>
      {
	static_assert(is_same<
	  typename __replace_first_arg<_Tp, typename _Tp::value_type>::type,
			_Tp>::value,
	  "allocator_traits<A>::rebind_alloc<A::value_type> must be A");
      };

    template<typename _Tp, typename _Up>
#if __cpp_concepts
      requires requires { typename _Tp::template rebind<_Up>::other; }
      struct __rebind<_Tp, _Up>
#else
      struct __rebind<_Tp, _Up,
		      __void_t<typename _Tp::template rebind<_Up>::other>>
#endif
      {
	using type = typename _Tp::template rebind<_Up>::other;

	static_assert(is_same<
	  typename _Tp::template rebind<typename _Tp::value_type>::other,
			_Tp>::value,
	  "allocator_traits<A>::rebind_alloc<A::value_type> must be A");
      };

  protected:
    template<typename _Tp>
      using __pointer = typename _Tp::pointer;
    template<typename _Tp>
      using __c_pointer = typename _Tp::const_pointer;
    template<typename _Tp>
      using __v_pointer = typename _Tp::void_pointer;
    template<typename _Tp>
      using __cv_pointer = typename _Tp::const_void_pointer;
    template<typename _Tp>
      using __pocca = typename _Tp::propagate_on_container_copy_assignment;
    template<typename _Tp>
      using __pocma = typename _Tp::propagate_on_container_move_assignment;
    template<typename _Tp>
      using __pocs = typename _Tp::propagate_on_container_swap;
    template<typename _Tp>
      using __equal = __type_identity<typename _Tp::is_always_equal>;

    // __has_allocate_hint is true if a.allocate(n, hint) is well-formed.
#if __cpp_concepts
    template<typename _Alloc, typename _Sz, typename _Vp>
      static constexpr bool __has_allocate_hint
	= requires (_Alloc& __a, _Sz __n, _Vp __hint) {
	__a.allocate(__n, __hint);
      };
#else
    template<typename _Alloc, typename _Sz, typename _Vp>
      using __allocate_hint_t
	= decltype(std::declval<_Alloc&>()
		     .allocate(std::declval<_Sz>(), std::declval<_Vp>()));
    template<typename _Alloc, typename _Sz, typename _Vp, typename = void>
      static constexpr bool __has_allocate_hint = false;
    template<typename _Alloc, typename _Sz, typename _Vp>
      static constexpr bool
      __has_allocate_hint<_Alloc, _Sz, _Vp,
			  __void_t<__allocate_hint_t<_Alloc, _Sz, _Vp>>>
	= true;
#endif

    // __has_construct is true if a.construct(p, args...) is well-formed.
    // __can_construct is true if either __has_construct is true, or if
    // a placement new-expression for T(args...) is well-formed. We use this
    // to constrain allocator_traits::construct, as a libstdc++ extension.
#if __cpp_concepts
    template<typename _Alloc, typename _Tp, typename... _Args>
      static constexpr bool __has_construct
	= requires (_Alloc& __a, _Tp* __p, _Args&&... __args) {
	  __a.construct(__p, std::forward<_Args>(__args)...);
	};
    template<typename _Tp, typename... _Args>
      static constexpr bool __can_construct_at
	= requires (_Tp* __p, _Args&&... __args) {
#if __cpp_constexpr_dynamic_alloc
	  std::construct_at(__p, std::forward<_Args>(__args)...);
#else
	  ::new((void*)__p) _Tp(std::forward<_Args>(__args)...);
#endif
	};
    template<typename _Alloc, typename _Tp, typename... _Args>
      static constexpr bool __can_construct
	= __has_construct<_Alloc, _Tp, _Args...>
	    || __can_construct_at<_Tp, _Args...>;
#else
    template<typename _Alloc, typename _Tp, typename... _Args>
      using __construct_t
	= decltype(std::declval<_Alloc&>().construct(std::declval<_Tp*>(),
						     std::declval<_Args>()...));
    template<typename _Alloc, typename _Tp, typename, typename... _Args>
      static constexpr bool __has_construct_impl = false;
    template<typename _Alloc, typename _Tp, typename... _Args>
      static constexpr bool
      __has_construct_impl<_Alloc, _Tp,
			   __void_t<__construct_t<_Alloc, _Tp, _Args...>>,
			   _Args...>
	= true;
    template<typename _Alloc, typename _Tp, typename... _Args>
      static constexpr bool __has_construct
	= __has_construct_impl<_Alloc, _Tp, void, _Args...>;
    template<typename _Tp, typename... _Args>
      using __new_expr_t
	= decltype(::new((void*)0) _Tp(std::declval<_Args>()...));
    template<typename _Tp, typename, typename... _Args>
      static constexpr bool __has_new_expr = false;
    template<typename _Tp, typename... _Args>
      static constexpr bool
      __has_new_expr<_Tp, __void_t<__new_expr_t<_Tp, _Args...>>, _Args...>
	= true;
    template<typename _Alloc, typename _Tp, typename... _Args>
      static constexpr bool __can_construct
	= __has_construct<_Alloc, _Tp, _Args...>
	    || __has_new_expr<_Tp, void, _Args...>;
#endif

    // __has_destroy is true if a.destroy(p) is well-formed.
#if __cpp_concepts
    template<typename _Alloc, typename _Tp>
      static constexpr bool __has_destroy = requires (_Alloc& __a, _Tp* __p) {
	__a.destroy(__p);
      };
#else
    template<typename _Alloc, typename _Tp>
      using __destroy_t
	= decltype(std::declval<_Alloc&>().destroy(std::declval<_Tp*>()));
    template<typename _Alloc, typename _Tp, typename = void>
      static constexpr bool __has_destroy = false;
    template<typename _Alloc, typename _Tp>
      static constexpr bool __has_destroy<_Alloc, _Tp,
					  __void_t<__destroy_t<_Alloc, _Tp>>>
	= true;
#endif

    // __has_max_size is true if a.max_size() is well-formed.
#if __cpp_concepts
    template<typename _Alloc>
      static constexpr bool __has_max_size = requires (const _Alloc& __a) {
	__a.max_size();
      };
#else
    template<typename _Alloc>
      using __max_size_t = decltype(std::declval<const _Alloc&>().max_size());
    template<typename _Alloc, typename = void>
      static constexpr bool __has_max_size = false;
    template<typename _Alloc>
      static constexpr bool __has_max_size<_Alloc,
					   __void_t<__max_size_t<_Alloc>>>
	= true;
#endif

    // __has_soccc is true if a.select_on_container_copy_construction()
    // is well-formed.
#if __cpp_concepts
    template<typename _Alloc>
      static constexpr bool __has_soccc = requires (const _Alloc& __a) {
	__a.select_on_container_copy_construction();
      };
#else
    template<typename _Alloc>
      using __soccc_t
	= decltype(std::declval<const _Alloc&>()
		     .select_on_container_copy_construction());
    template<typename _Alloc, typename = void>
      static constexpr bool __has_soccc = false;
    template<typename _Alloc>
      static constexpr bool __has_soccc<_Alloc, __void_t<__soccc_t<_Alloc>>>
	= true;
#endif
  };

  template<typename _Alloc, typename _Up>
    using __alloc_rebind
      = typename __allocator_traits_base::template __rebind<_Alloc, _Up>::type;
  /// @endcond

  /**
   * @brief  Uniform interface to all allocator types.
   * @headerfile memory
   * @ingroup allocators
   * @since C++11
  */
  template<typename _Alloc>
    struct allocator_traits : __allocator_traits_base
    {
      /// The allocator type
      typedef _Alloc allocator_type;
      /// The allocated type
      typedef typename _Alloc::value_type value_type;

      /**
       * @brief   The allocator's pointer type.
       *
       * @c Alloc::pointer if that type exists, otherwise @c value_type*
      */
      using pointer = __detected_or_t<value_type*, __pointer, _Alloc>;

    private:
      // Select _Func<_Alloc> or pointer_traits<pointer>::rebind<_Tp>
      template<template<typename> class _Func, typename _Tp, typename = void>
	struct _Ptr
	{
	  using type = typename pointer_traits<pointer>::template rebind<_Tp>;
	};

      template<template<typename> class _Func, typename _Tp>
	struct _Ptr<_Func, _Tp, __void_t<_Func<_Alloc>>>
	{
	  using type = _Func<_Alloc>;
	};

      // Select _A2::difference_type or pointer_traits<_Ptr>::difference_type
      template<typename _A2, typename _PtrT, typename = void>
	struct _Diff
	{ using type = typename pointer_traits<_PtrT>::difference_type; };

      template<typename _A2, typename _PtrT>
	struct _Diff<_A2, _PtrT, __void_t<typename _A2::difference_type>>
	{ using type = typename _A2::difference_type; };

      // Select _A2::size_type or make_unsigned<_DiffT>::type
      template<typename _A2, typename _DiffT, typename = void>
	struct _Size : make_unsigned<_DiffT> { };

      template<typename _A2, typename _DiffT>
	struct _Size<_A2, _DiffT, __void_t<typename _A2::size_type>>
	{ using type = typename _A2::size_type; };

    public:
      /**
       * @brief   The allocator's const pointer type.
       *
       * @c Alloc::const_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<const value_type> </tt>
      */
      using const_pointer = typename _Ptr<__c_pointer, const value_type>::type;

      /**
       * @brief   The allocator's void pointer type.
       *
       * @c Alloc::void_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<void> </tt>
      */
      using void_pointer = typename _Ptr<__v_pointer, void>::type;

      /**
       * @brief   The allocator's const void pointer type.
       *
       * @c Alloc::const_void_pointer if that type exists, otherwise
       * <tt> pointer_traits<pointer>::rebind<const void> </tt>
      */
      using const_void_pointer = typename _Ptr<__cv_pointer, const void>::type;

      /**
       * @brief   The allocator's difference type
       *
       * @c Alloc::difference_type if that type exists, otherwise
       * <tt> pointer_traits<pointer>::difference_type </tt>
      */
      using difference_type = typename _Diff<_Alloc, pointer>::type;

      /**
       * @brief   The allocator's size type
       *
       * @c Alloc::size_type if that type exists, otherwise
       * <tt> make_unsigned<difference_type>::type </tt>
      */
      using size_type = typename _Size<_Alloc, difference_type>::type;

      /**
       * @brief   How the allocator is propagated on copy assignment
       *
       * @c Alloc::propagate_on_container_copy_assignment if that type exists,
       * otherwise @c false_type
      */
      using propagate_on_container_copy_assignment
	= __detected_or_t<false_type, __pocca, _Alloc>;

      /**
       * @brief   How the allocator is propagated on move assignment
       *
       * @c Alloc::propagate_on_container_move_assignment if that type exists,
       * otherwise @c false_type
      */
      using propagate_on_container_move_assignment
	= __detected_or_t<false_type, __pocma, _Alloc>;

      /**
       * @brief   How the allocator is propagated on swap
       *
       * @c Alloc::propagate_on_container_swap if that type exists,
       * otherwise @c false_type
      */
      using propagate_on_container_swap
	= __detected_or_t<false_type, __pocs, _Alloc>;

      /**
       * @brief   Whether all instances of the allocator type compare equal.
       *
       * @c Alloc::is_always_equal if that type exists,
       * otherwise @c is_empty<Alloc>::type
      */
      using is_always_equal
	= typename __detected_or_t<is_empty<_Alloc>, __equal, _Alloc>::type;

      template<typename _Tp>
	using rebind_alloc = __alloc_rebind<_Alloc, _Tp>;
      template<typename _Tp>
	using rebind_traits = allocator_traits<rebind_alloc<_Tp>>;

      /**
       *  @brief  Allocate memory.
       *  @param  __a  An allocator.
       *  @param  __n  The number of objects to allocate space for.
       *
       *  Calls @c a.allocate(n)
      */
      _GLIBCXX_NODISCARD static _GLIBCXX20_CONSTEXPR pointer
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
      _GLIBCXX_NODISCARD static _GLIBCXX20_CONSTEXPR pointer
      allocate(_Alloc& __a, size_type __n, const_void_pointer __hint)
      {
	if constexpr (__has_allocate_hint<_Alloc, size_type, const_void_pointer>)
	  return __a.allocate(__n, __hint);
	else
	  return __a.allocate(__n);
      }

      /**
       *  @brief  Deallocate memory.
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the memory to deallocate.
       *  @param  __n  The number of objects space was allocated for.
       *
       *  Calls <tt> a.deallocate(p, n) </tt>
      */
      static _GLIBCXX20_CONSTEXPR void
      deallocate(_Alloc& __a, pointer __p, size_type __n)
      { __a.deallocate(__p, __n); }

      /**
       *  @brief  Construct an object of type `_Tp`
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
#if __cpp_concepts && __cpp_constexpr_dynamic_alloc
	requires __can_construct<_Alloc, _Tp, _Args...>
	static constexpr void
#else
	static __enable_if_t<__can_construct<_Alloc, _Tp, _Args...>>
#endif
	construct(_Alloc& __a, _Tp* __p, _Args&&... __args)
	noexcept(_S_nothrow_construct<_Tp, _Args...>())
	{
	  if constexpr (__has_construct<_Alloc, _Tp, _Args...>)
	    __a.construct(__p, std::forward<_Args>(__args)...);
	  else
	    std::_Construct(__p, std::forward<_Args>(__args)...);
	}

      /**
       *  @brief  Destroy an object of type @a _Tp
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the object to destroy
       *
       *  Calls @c __a.destroy(__p) if that expression is well-formed,
       *  otherwise calls @c __p->~_Tp()
      */
      template<typename _Tp>
	static _GLIBCXX20_CONSTEXPR void
	destroy(_Alloc& __a, _Tp* __p)
	noexcept(_S_nothrow_destroy<_Tp>())
	{
	  if constexpr (__has_destroy<_Alloc, _Tp>)
	    __a.destroy(__p);
	  else
	    std::_Destroy(__p);
	}

      /**
       *  @brief  The maximum supported allocation size
       *  @param  __a  An allocator.
       *  @return @c __a.max_size() or @c numeric_limits<size_type>::max()
       *
       *  Returns @c __a.max_size() if that expression is well-formed,
       *  otherwise returns @c numeric_limits<size_type>::max()
      */
      static _GLIBCXX20_CONSTEXPR size_type
      max_size(const _Alloc& __a) noexcept
      {
	if constexpr (__has_max_size<_Alloc>)
	  return __a.max_size();
	else
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 2466. allocator_traits::max_size() default behavior is incorrect
	  return __gnu_cxx::__numeric_traits<size_type>::__max
	    / sizeof(value_type);
      }

      /**
       *  @brief  Obtain an allocator to use when copying a container.
       *  @param  __rhs  An allocator.
       *  @return @c __rhs.select_on_container_copy_construction() or @a __rhs
       *
       *  Returns @c __rhs.select_on_container_copy_construction() if that
       *  expression is well-formed, otherwise returns @a __rhs
      */
      static _GLIBCXX20_CONSTEXPR _Alloc
      select_on_container_copy_construction(const _Alloc& __rhs)
      {
	if constexpr (__has_soccc<_Alloc>)
	  return __rhs.select_on_container_copy_construction();
	else
	  return __rhs;
      }

    private:
#if __cpp_constexpr >= 201304 // >= C++14
      template<typename _Tp, typename... _Args>
	static constexpr bool
	_S_nothrow_construct(_Alloc* __a = nullptr, _Tp* __p = nullptr)
	{
	  if constexpr (__has_construct<_Alloc, _Tp, _Args...>)
	    return noexcept(__a->construct(__p, std::declval<_Args>()...));
	  else
	    return __is_nothrow_new_constructible<_Tp, _Args...>;
	}

      template<typename _Tp>
	static constexpr bool
	_S_nothrow_destroy(_Alloc* __a = nullptr, _Tp* __p = nullptr)
	{
	  if constexpr (__has_destroy<_Alloc, _Tp>)
	    return noexcept(__a->destroy(__p));
	  else
	    return is_nothrow_destructible<_Tp>::value;
	}
#else
      template<typename _Tp, typename... _Args>
	static constexpr
	__enable_if_t<__has_construct<_Alloc, _Tp, _Args...>, bool>
	_S_nothrow_construct(_Alloc* __a = nullptr, _Tp* __p = nullptr)
	{ return noexcept(__a->construct(__p, std::declval<_Args>()...)); }

      template<typename _Tp, typename... _Args>
	static constexpr
	__enable_if_t<!__has_construct<_Alloc, _Tp, _Args...>, bool>
	_S_nothrow_construct(_Alloc* = nullptr, _Tp* __p = nullptr)
	{ return __is_nothrow_new_constructible<_Tp, _Args...>; }

      template<typename _Tp>
	static constexpr
	__enable_if_t<__has_destroy<_Alloc, _Tp>, bool>
	_S_nothrow_destroy(_Alloc* __a = nullptr, _Tp* __p = nullptr)
	{ return noexcept(__a->destroy(__p)); }

      template<typename _Tp>
	static constexpr
	__enable_if_t<!__has_destroy<_Alloc, _Tp>, bool>
	_S_nothrow_destroy(_Alloc* = nullptr, _Tp* __p = nullptr)
	{ return is_nothrow_destructible<_Tp>::value; }
#endif
    };
#pragma GCC diagnostic pop

#if _GLIBCXX_HOSTED
  /**
   * @brief  Partial specialization for `std::allocator`
   * @headerfile memory
   * @ingroup allocators
   * @since C++11
   * @see std::allocator_traits
  */
  template<typename _Tp>
    struct allocator_traits<allocator<_Tp>>
    {
      /// The allocator type
      using allocator_type = allocator<_Tp>;

      /// The allocated type
      using value_type = _Tp;

      /// The allocator's pointer type.
      using pointer = _Tp*;

      /// The allocator's const pointer type.
      using const_pointer = const _Tp*;

      /// The allocator's void pointer type.
      using void_pointer = void*;

      /// The allocator's const void pointer type.
      using const_void_pointer = const void*;

      /// The allocator's difference type
      using difference_type = std::ptrdiff_t;

      /// The allocator's size type
      using size_type = std::size_t;

      /// How the allocator is propagated on copy assignment
      using propagate_on_container_copy_assignment = false_type;

      /// How the allocator is propagated on move assignment
      using propagate_on_container_move_assignment = true_type;

      /// How the allocator is propagated on swap
      using propagate_on_container_swap = false_type;

      /// Whether all instances of the allocator type compare equal.
      using is_always_equal = true_type;

      template<typename _Up>
	using rebind_alloc = allocator<_Up>;

      template<typename _Up>
	using rebind_traits = allocator_traits<allocator<_Up>>;

      /**
       *  @brief  Allocate memory.
       *  @param  __a  An allocator.
       *  @param  __n  The number of objects to allocate space for.
       *
       *  Calls @c a.allocate(n)
      */
      [[__nodiscard__,__gnu__::__always_inline__]]
      static _GLIBCXX20_CONSTEXPR pointer
      allocate(allocator_type& __a, size_type __n)
      { return __a.allocate(__n); }

      /**
       *  @brief  Allocate memory.
       *  @param  __a  An allocator.
       *  @param  __n  The number of objects to allocate space for.
       *  @param  __hint Aid to locality.
       *  @return Memory of suitable size and alignment for @a n objects
       *          of type @c value_type
       *
       *  Returns <tt> a.allocate(n, hint) </tt>
      */
      [[__nodiscard__,__gnu__::__always_inline__]]
      static _GLIBCXX20_CONSTEXPR pointer
      allocate(allocator_type& __a, size_type __n,
	       [[maybe_unused]] const_void_pointer __hint)
      {
#if __cplusplus <= 201703L
	return __a.allocate(__n, __hint);
#else
	return __a.allocate(__n);
#endif
      }

      /**
       *  @brief  Deallocate memory.
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the memory to deallocate.
       *  @param  __n  The number of objects space was allocated for.
       *
       *  Calls <tt> a.deallocate(p, n) </tt>
      */
      [[__gnu__::__always_inline__]]
      static _GLIBCXX20_CONSTEXPR void
      deallocate(allocator_type& __a, pointer __p, size_type __n)
      { __a.deallocate(__p, __n); }

      /**
       *  @brief  Construct an object of type `_Up`
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to memory of suitable size and alignment for
       *	       an object of type `_Up`.
       *  @param  __args Constructor arguments.
       *
       *  Calls `__a.construct(__p, std::forward<_Args>(__args)...)`
       *  in C++11, C++14 and C++17. Changed in C++20 to call
       *  `std::construct_at(__p, std::forward<_Args>(__args)...)` instead.
      */
      template<typename _Up, typename... _Args>
	[[__gnu__::__always_inline__]]
	static _GLIBCXX20_CONSTEXPR void
	construct(allocator_type& __a __attribute__((__unused__)),
		  _Up* __p, _Args&&... __args)
#if __cplusplus <= 201703L
	noexcept(noexcept(__a.construct(__p, std::forward<_Args>(__args)...)))
#else
	noexcept(__is_nothrow_new_constructible<_Up, _Args...>)
#endif
	{
#if __cplusplus <= 201703L
	  __a.construct(__p, std::forward<_Args>(__args)...);
#elif __cpp_constexpr_dynamic_alloc // >= C++20
	  std::construct_at(__p, std::forward<_Args>(__args)...);
#else
	  std::_Construct(__p, std::forward<_Args>(__args)...);
#endif
	}

      /**
       *  @brief  Destroy an object of type @a _Up
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the object to destroy
       *
       *  Calls @c __a.destroy(__p).
      */
      template<typename _Up>
	[[__gnu__::__always_inline__]]
	static _GLIBCXX20_CONSTEXPR void
	destroy(allocator_type& __a __attribute__((__unused__)), _Up* __p)
	noexcept(is_nothrow_destructible<_Up>::value)
	{
#if __cplusplus <= 201703L
	  __a.destroy(__p);
#else
	  std::destroy_at(__p);
#endif
	}

      /**
       *  @brief  The maximum supported allocation size
       *  @param  __a  An allocator.
       *  @return @c __a.max_size()
      */
      [[__gnu__::__always_inline__]]
      static _GLIBCXX20_CONSTEXPR size_type
      max_size(const allocator_type& __a __attribute__((__unused__))) noexcept
      {
#if __cplusplus <= 201703L
	return __a.max_size();
#else
	return size_t(-1) / sizeof(value_type);
#endif
      }

      /**
       *  @brief  Obtain an allocator to use when copying a container.
       *  @param  __rhs  An allocator.
       *  @return @c __rhs
      */
      [[__gnu__::__always_inline__]]
      static _GLIBCXX20_CONSTEXPR allocator_type
      select_on_container_copy_construction(const allocator_type& __rhs)
      { return __rhs; }
    };

  /**
   * @brief  Explicit specialization for `std::allocator<void>`
   * @headerfile memory
   * @ingroup allocators
   * @since C++11
   * @see std::allocator_traits
  */
  template<>
    struct allocator_traits<allocator<void>>
    {
      /// The allocator type
      using allocator_type = allocator<void>;

      /// The allocated type
      using value_type = void;

      /// The allocator's pointer type.
      using pointer = void*;

      /// The allocator's const pointer type.
      using const_pointer = const void*;

      /// The allocator's void pointer type.
      using void_pointer = void*;

      /// The allocator's const void pointer type.
      using const_void_pointer = const void*;

      /// The allocator's difference type
      using difference_type = std::ptrdiff_t;

      /// The allocator's size type
      using size_type = std::size_t;

      /// How the allocator is propagated on copy assignment
      using propagate_on_container_copy_assignment = false_type;

      /// How the allocator is propagated on move assignment
      using propagate_on_container_move_assignment = true_type;

      /// How the allocator is propagated on swap
      using propagate_on_container_swap = false_type;

      /// Whether all instances of the allocator type compare equal.
      using is_always_equal = true_type;

      template<typename _Up>
	using rebind_alloc = allocator<_Up>;

      template<typename _Up>
	using rebind_traits = allocator_traits<allocator<_Up>>;

      /// allocate is ill-formed for allocator<void>
      static void*
      allocate(allocator_type&, size_type, const void* = nullptr) = delete;

      /// deallocate is ill-formed for allocator<void>
      static void
      deallocate(allocator_type&, void*, size_type) = delete;

      /**
       *  @brief  Construct an object of type `_Up`
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to memory of suitable size and alignment for
       *	       an object of type `_Up`.
       *  @param  __args Constructor arguments.
       *
       *  Calls `__a.construct(__p, std::forward<_Args>(__args)...)`
       *  in C++11, C++14 and C++17. Changed in C++20 to call
       *  `std::construct_at(__p, std::forward<_Args>(__args)...)` instead.
      */
      template<typename _Up, typename... _Args>
	[[__gnu__::__always_inline__]]
	static _GLIBCXX20_CONSTEXPR void
	construct(allocator_type&, _Up* __p, _Args&&... __args)
	noexcept(__is_nothrow_new_constructible<_Up, _Args...>)
	{ std::_Construct(__p, std::forward<_Args>(__args)...); }

      /**
       *  @brief  Destroy an object of type `_Up`
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the object to destroy
       *
       *  Invokes the destructor for `*__p`.
      */
      template<typename _Up>
	[[__gnu__::__always_inline__]]
	static _GLIBCXX20_CONSTEXPR void
	destroy(allocator_type&, _Up* __p)
	noexcept(is_nothrow_destructible<_Up>::value)
	{ std::_Destroy(__p); }

      /// max_size is ill-formed for allocator<void>
      static size_type
      max_size(const allocator_type&) = delete;

      /**
       *  @brief  Obtain an allocator to use when copying a container.
       *  @param  __rhs  An allocator.
       *  @return `__rhs`
      */
      [[__gnu__::__always_inline__]]
      static _GLIBCXX20_CONSTEXPR allocator_type
      select_on_container_copy_construction(const allocator_type& __rhs)
      { return __rhs; }
    };
#endif

  /// @cond undocumented
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
  template<typename _Alloc>
    [[__gnu__::__always_inline__]]
    _GLIBCXX14_CONSTEXPR inline void
    __alloc_on_copy(_Alloc& __one, const _Alloc& __two)
    {
      using __traits = allocator_traits<_Alloc>;
      using __pocca =
	typename __traits::propagate_on_container_copy_assignment::type;
      if constexpr (__pocca::value)
	__one = __two;
    }

  template<typename _Alloc>
    [[__gnu__::__always_inline__]]
    constexpr _Alloc
    __alloc_on_copy(const _Alloc& __a)
    {
      typedef allocator_traits<_Alloc> __traits;
      return __traits::select_on_container_copy_construction(__a);
    }

  template<typename _Alloc>
    [[__gnu__::__always_inline__]]
    _GLIBCXX14_CONSTEXPR inline void
    __alloc_on_move(_Alloc& __one, _Alloc& __two)
    {
      using __traits = allocator_traits<_Alloc>;
      using __pocma
	= typename __traits::propagate_on_container_move_assignment::type;
      if constexpr (__pocma::value)
	__one = std::move(__two);
    }

  template<typename _Alloc>
    [[__gnu__::__always_inline__]]
    _GLIBCXX14_CONSTEXPR inline void
    __alloc_on_swap(_Alloc& __one, _Alloc& __two)
    {
      using __traits = allocator_traits<_Alloc>;
      using __pocs = typename __traits::propagate_on_container_swap::type;
      if constexpr (__pocs::value)
	{
	  using std::swap;
	  swap(__one, __two);
	}
    }
#pragma GCC diagnostic pop

  template<typename _Alloc, typename _Tp,
	   typename _ValueT = __remove_cvref_t<typename _Alloc::value_type>,
	   typename = void>
    struct __is_alloc_insertable_impl
    : false_type
    { };

  template<typename _Alloc, typename _Tp, typename _ValueT>
    struct __is_alloc_insertable_impl<_Alloc, _Tp, _ValueT,
      __void_t<decltype(allocator_traits<_Alloc>::construct(
		   std::declval<_Alloc&>(), std::declval<_ValueT*>(),
		   std::declval<_Tp>()))>>
    : true_type
    { };

  // true if _Alloc::value_type is CopyInsertable into containers using _Alloc
  // (might be wrong if _Alloc::construct exists but is not constrained,
  // i.e. actually trying to use it would still be invalid. Use with caution.)
  template<typename _Alloc>
    struct __is_copy_insertable
    : __is_alloc_insertable_impl<_Alloc,
				 typename _Alloc::value_type const&>::type
    { };

#if _GLIBCXX_HOSTED
  // std::allocator<_Tp> just requires CopyConstructible
  template<typename _Tp>
    struct __is_copy_insertable<allocator<_Tp>>
    : is_copy_constructible<_Tp>
    { };
#endif

  // true if _Alloc::value_type is MoveInsertable into containers using _Alloc
  // (might be wrong if _Alloc::construct exists but is not constrained,
  // i.e. actually trying to use it would still be invalid. Use with caution.)
  template<typename _Alloc>
    struct __is_move_insertable
    : __is_alloc_insertable_impl<_Alloc, typename _Alloc::value_type>::type
    { };

#if _GLIBCXX_HOSTED
  // std::allocator<_Tp> just requires MoveConstructible
  template<typename _Tp>
    struct __is_move_insertable<allocator<_Tp>>
    : is_move_constructible<_Tp>
    { };
#endif

  // Trait to detect Allocator-like types.
  template<typename _Alloc, typename = void>
    struct __is_allocator : false_type { };

  template<typename _Alloc>
    struct __is_allocator<_Alloc,
      __void_t<typename _Alloc::value_type,
	       decltype(std::declval<_Alloc&>().allocate(size_t{}))>>
    : true_type { };

  template<typename _Alloc>
    using _RequireAllocator
      = typename enable_if<__is_allocator<_Alloc>::value, _Alloc>::type;

  template<typename _Alloc>
    using _RequireNotAllocator
      = typename enable_if<!__is_allocator<_Alloc>::value, _Alloc>::type;

#if __cpp_concepts >= 201907L
  template<typename _Alloc>
    concept __allocator_like = requires (_Alloc& __a) {
      typename _Alloc::value_type;
      __a.deallocate(__a.allocate(1u), 1u);
    };

  template<typename _Alloc>
    concept __not_allocator_like = !__allocator_like<_Alloc>;
#endif
  /// @endcond
#endif // C++11

  /// @cond undocumented

  // To implement Option 3 of DR 431.
  template<typename _Alloc, bool = __is_empty(_Alloc)>
    struct __alloc_swap
    { static void _S_do_it(_Alloc&, _Alloc&) _GLIBCXX_NOEXCEPT { } };

  template<typename _Alloc>
    struct __alloc_swap<_Alloc, false>
    {
      static void
      _S_do_it(_Alloc& __one, _Alloc& __two) _GLIBCXX_NOEXCEPT
      {
	// Precondition: swappable allocators.
	if (__one != __two)
	  swap(__one, __two);
      }
    };

#if __cplusplus >= 201103L
  template<typename _Tp, bool
    = __or_<is_copy_constructible<typename _Tp::value_type>,
            is_nothrow_move_constructible<typename _Tp::value_type>>::value>
    struct __shrink_to_fit_aux
    { static bool _S_do_it(_Tp&) noexcept { return false; } };

  template<typename _Tp>
    struct __shrink_to_fit_aux<_Tp, true>
    {
      _GLIBCXX20_CONSTEXPR
      static bool
      _S_do_it(_Tp& __c) noexcept
      {
#if __cpp_exceptions
	try
	  {
	    _Tp(__make_move_if_noexcept_iterator(__c.begin()),
		__make_move_if_noexcept_iterator(__c.end()),
		__c.get_allocator()).swap(__c);
	    return true;
	  }
	catch(...)
	  { return false; }
#else
	return false;
#endif
      }
    };
#endif

  /**
   * Destroy a range of objects using the supplied allocator.  For
   * non-default allocators we do not optimize away invocation of
   * destroy() even if _Tp has a trivial destructor.
   */

  template<typename _ForwardIterator, typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    void
    _Destroy(_ForwardIterator __first, _ForwardIterator __last,
	     _Allocator& __alloc)
    {
      for (; __first != __last; ++__first)
#if __cplusplus < 201103L
	__alloc.destroy(std::__addressof(*__first));
#else
	allocator_traits<_Allocator>::destroy(__alloc,
					      std::__addressof(*__first));
#endif
    }

#if _GLIBCXX_HOSTED
  template<typename _ForwardIterator, typename _Tp>
    __attribute__((__always_inline__)) _GLIBCXX20_CONSTEXPR
    inline void
    _Destroy(_ForwardIterator __first, _ForwardIterator __last,
	     allocator<_Tp>&)
    {
      std::_Destroy(__first, __last);
    }
#endif

  /// @endcond

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // _ALLOC_TRAITS_H
