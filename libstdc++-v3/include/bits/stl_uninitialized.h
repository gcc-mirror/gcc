// Raw memory manipulators -*- C++ -*-

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

/** @file bits/stl_uninitialized.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _STL_UNINITIALIZED_H
#define _STL_UNINITIALIZED_H 1

#if __cplusplus >= 201103L
# include <type_traits>
# include <bits/ptr_traits.h>      // to_address
# include <bits/stl_pair.h>        // pair
# include <bits/stl_algobase.h>    // fill, fill_n
#endif

#include <bits/cpp_type_traits.h> // __is_pointer
#include <bits/stl_iterator_base_funcs.h> // distance, advance
#include <bits/stl_iterator.h>    // __niter_base
#include <ext/alloc_traits.h>     // __alloc_traits

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /** @addtogroup memory
   *  @{
   */

  /// @cond undocumented

  template<typename _ForwardIterator, typename _Alloc = void>
    struct _UninitDestroyGuard
    {
      _GLIBCXX20_CONSTEXPR
      explicit
      _UninitDestroyGuard(_ForwardIterator& __first, _Alloc& __a)
      : _M_first(__first), _M_cur(__builtin_addressof(__first)), _M_alloc(__a)
      { }

      _GLIBCXX20_CONSTEXPR
      ~_UninitDestroyGuard()
      {
	if (__builtin_expect(_M_cur != 0, 0))
	  std::_Destroy(_M_first, *_M_cur, _M_alloc);
      }

      _GLIBCXX20_CONSTEXPR
      void release() { _M_cur = 0; }

    private:
      _ForwardIterator const _M_first;
      _ForwardIterator* _M_cur;
      _Alloc& _M_alloc;

      _UninitDestroyGuard(const _UninitDestroyGuard&);
    };

  template<typename _ForwardIterator>
    struct _UninitDestroyGuard<_ForwardIterator, void>
    {
      _GLIBCXX20_CONSTEXPR
      explicit
      _UninitDestroyGuard(_ForwardIterator& __first)
      : _M_first(__first), _M_cur(__builtin_addressof(__first))
      { }

      _GLIBCXX20_CONSTEXPR
      ~_UninitDestroyGuard()
      {
	if (__builtin_expect(_M_cur != 0, 0))
	  std::_Destroy(_M_first, *_M_cur);
      }

      _GLIBCXX20_CONSTEXPR
      void release() { _M_cur = 0; }

      _ForwardIterator const _M_first;
      _ForwardIterator* _M_cur;

    private:
      _UninitDestroyGuard(const _UninitDestroyGuard&);
    };

  // This is the default implementation of std::uninitialized_copy.
  // This can be used with C++20 iterators and non-common ranges.
  template<typename _InputIterator, typename _Sentinel,
	   typename _ForwardIterator>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator
    __do_uninit_copy(_InputIterator __first, _Sentinel __last,
		     _ForwardIterator __result)
    {
      _UninitDestroyGuard<_ForwardIterator> __guard(__result);
      for (; __first != __last; ++__first, (void)++__result)
	std::_Construct(std::__addressof(*__result), *__first);
      __guard.release();
      return __result;
    }

#if __cplusplus < 201103L

  // True if we can unwrap _Iter to get a pointer by using std::__niter_base.
  template<typename _Iter,
	   typename _Base = __decltype(std::__niter_base(*(_Iter*)0))>
    struct __unwrappable_niter
    { enum { __value = false }; };

  template<typename _Iter, typename _Tp>
    struct __unwrappable_niter<_Iter, _Tp*>
    { enum { __value = true }; };

  // Use template specialization for C++98 when 'if constexpr' can't be used.
  template<bool _CanMemcpy>
    struct __uninitialized_copy
    {
      template<typename _InputIterator, typename _ForwardIterator>
        static _ForwardIterator
        __uninit_copy(_InputIterator __first, _InputIterator __last,
		      _ForwardIterator __result)
	{ return std::__do_uninit_copy(__first, __last, __result); }
    };

  template<>
    struct __uninitialized_copy<true>
    {
      // Overload for generic iterators.
      template<typename _InputIterator, typename _ForwardIterator>
        static _ForwardIterator
        __uninit_copy(_InputIterator __first, _InputIterator __last,
		      _ForwardIterator __result)
	{
	  if (__unwrappable_niter<_InputIterator>::__value
		&& __unwrappable_niter<_ForwardIterator>::__value)
	    {
	      __uninit_copy(std::__niter_base(__first),
			    std::__niter_base(__last),
			    std::__niter_base(__result));
	      std::advance(__result, std::distance(__first, __last));
	      return __result;
	    }
	  else
	    return std::__do_uninit_copy(__first, __last, __result);
	}

      // Overload for pointers.
      template<typename _Tp, typename _Up>
	static _Up*
	__uninit_copy(_Tp* __first, _Tp* __last, _Up* __result)
	{
	  // Ensure that we don't successfully memcpy in cases that should be
	  // ill-formed because is_constructible<_Up, _Tp&> is false.
	  typedef __typeof__(static_cast<_Up>(*__first)) __check
	    __attribute__((__unused__));

	  const ptrdiff_t __n = __last - __first;
	  if (__builtin_expect(__n > 0, true))
	    {
	      __builtin_memcpy(__result, __first, __n * sizeof(_Tp));
	      __result += __n;
	    }
	  return __result;
	}
    };
#endif
  /// @endcond

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
  /**
   *  @brief Copies the range [first,last) into result.
   *  @param  __first  An input iterator.
   *  @param  __last   An input iterator.
   *  @param  __result A forward iterator.
   *  @return   __result + (__last - __first)
   *
   *  Like std::copy, but does not require an initialized output range.
  */
  template<typename _InputIterator, typename _ForwardIterator>
    inline _ForwardIterator
    uninitialized_copy(_InputIterator __first, _InputIterator __last,
		       _ForwardIterator __result)
    {
      // We can use memcpy to copy the ranges under these conditions:
      //
      // _ForwardIterator and _InputIterator are both contiguous iterators,
      // so that we can turn them into pointers to pass to memcpy.
      // Before C++20 we can't detect all contiguous iterators, so we only
      // handle built-in pointers and __normal_iterator<T*, C> types.
      //
      // The value types of both iterators are trivially-copyable types,
      // so that memcpy is not undefined and can begin the lifetime of
      // new objects in the output range.
      //
      // Finally, memcpy from the source type, S, to the destination type, D,
      // must give the same value as initialization of D from S would give.
      // We require is_trivially_constructible<D, S> to be true, but that is
      // not sufficient. Some cases of trivial initialization are not just a
      // bitwise copy, even when sizeof(D) == sizeof(S),
      // e.g. bit_cast<unsigned>(1.0f) != 1u because the corresponding bits
      // of the value representations do not have the same meaning.
      // We cannot tell when this condition is true in general,
      // so we rely on the __memcpyable trait.

#if __cplusplus >= 201103L
      using _Dest = decltype(std::__niter_base(__result));
      using _Src = decltype(std::__niter_base(__first));
      using _ValT = typename iterator_traits<_ForwardIterator>::value_type;

      if constexpr (!__is_trivially_constructible(_ValT, decltype(*__first)))
	return std::__do_uninit_copy(__first, __last, __result);
      else if constexpr (__memcpyable<_Dest, _Src>::__value)
	{
	  ptrdiff_t __n = __last - __first;
	  if (__n > 0) [[__likely__]]
	    {
	      using _ValT = typename remove_pointer<_Src>::type;
	      __builtin_memcpy(std::__niter_base(__result),
			       std::__niter_base(__first),
			       __n * sizeof(_ValT));
	      __result += __n;
	    }
	  return __result;
	}
#if __cpp_lib_concepts
      else if constexpr (contiguous_iterator<_ForwardIterator>
			   && contiguous_iterator<_InputIterator>)
	{
	  using _DestPtr = decltype(std::to_address(__result));
	  using _SrcPtr = decltype(std::to_address(__first));
	  if constexpr (__memcpyable<_DestPtr, _SrcPtr>::__value)
	    {
	      if (auto __n = __last - __first; __n > 0) [[likely]]
		{
		  void* __dest = std::to_address(__result);
		  const void* __src = std::to_address(__first);
		  size_t __nbytes = __n * sizeof(remove_pointer_t<_DestPtr>);
		  __builtin_memcpy(__dest, __src, __nbytes);
		  __result += __n;
		}
	      return __result;
	    }
	  else
	    return std::__do_uninit_copy(__first, __last, __result);
	}
#endif
      else
	return std::__do_uninit_copy(__first, __last, __result);
#else // C++98
      typedef typename iterator_traits<_InputIterator>::value_type
	_ValueType1;
      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType2;

      const bool __can_memcpy
	= __memcpyable<_ValueType1*, _ValueType2*>::__value
	    && __is_trivially_constructible(_ValueType2, __decltype(*__first));

      return __uninitialized_copy<__can_memcpy>::
	       __uninit_copy(__first, __last, __result);
#endif
    }
#pragma GCC diagnostic pop

  /// @cond undocumented

  // This is the default implementation of std::uninitialized_fill.
  template<typename _ForwardIterator, typename _Tp>
    _GLIBCXX20_CONSTEXPR void
    __do_uninit_fill(_ForwardIterator __first, _ForwardIterator __last,
		     const _Tp& __x)
    {
      _UninitDestroyGuard<_ForwardIterator> __guard(__first);
      for (; __first != __last; ++__first)
	std::_Construct(std::__addressof(*__first), __x);
      __guard.release();
    }

#if __cplusplus < 201103L
  // Use template specialization for C++98 when 'if constexpr' can't be used.
  template<bool _CanMemset>
    struct __uninitialized_fill
    {
      template<typename _ForwardIterator, typename _Tp>
	static void
	__uninit_fill(_ForwardIterator __first, _ForwardIterator __last,
		      const _Tp& __x)
	{ std::__do_uninit_fill(__first, __last, __x); }
    };

  template<>
    struct __uninitialized_fill<true>
    {
      // Overload for generic iterators.
      template<typename _ForwardIterator, typename _Tp>
	static void
	__uninit_fill(_ForwardIterator __first, _ForwardIterator __last,
		      const _Tp& __x)
	{
	  if (__unwrappable_niter<_ForwardIterator>::__value)
	    __uninit_fill(std::__niter_base(__first),
			  std::__niter_base(__last),
			  __x);
	  else
	    std::__do_uninit_copy(__first, __last, __x);
	}

      // Overload for pointers.
      template<typename _Up, typename _Tp>
	static void
	__uninit_fill(_Up* __first, _Up* __last, const _Tp& __x)
	{
	  // Ensure that we don't successfully memset in cases that should be
	  // ill-formed because is_constructible<_Up, const _Tp&> is false.
	  typedef __typeof__(static_cast<_Up>(__x)) __check
	    __attribute__((__unused__));

	  if (__first != __last)
	    __builtin_memset(__first, (unsigned char)__x, __last - __first);
	}
    };
#endif
  /// @endcond

  /**
   *  @brief Copies the value x into the range [first,last).
   *  @param  __first  A forward iterator.
   *  @param  __last   A forward iterator.
   *  @param  __x      The source value.
   *  @return   Nothing.
   *
   *  Like std::fill, but does not require an initialized output range.
  */
  template<typename _ForwardIterator, typename _Tp>
    inline void
    uninitialized_fill(_ForwardIterator __first, _ForwardIterator __last,
		       const _Tp& __x)
    {
      // We would like to use memset to optimize this loop when possible.
      // As for std::uninitialized_copy, the optimization requires
      // contiguous iterators and trivially copyable value types,
      // with the additional requirement that sizeof(_Tp) == 1 because
      // memset only writes single bytes.

      // FIXME: We could additionally enable this for 1-byte enums.
      // Maybe any 1-byte Val if is_trivially_constructible<Val, const T&>?

      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType;

#if __cplusplus >= 201103L
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
      if constexpr (__is_byte<_ValueType>::__value)
	if constexpr (is_same<_ValueType, _Tp>::value
			|| is_integral<_Tp>::value)
	  {
	    using _BasePtr = decltype(std::__niter_base(__first));
	    if constexpr (is_pointer<_BasePtr>::value)
	      {
		void* __dest = std::__niter_base(__first);
		ptrdiff_t __n = __last - __first;
		if (__n > 0) [[__likely__]]
		  __builtin_memset(__dest, (unsigned char)__x, __n);
		return;
	      }
#if __cpp_lib_concepts
	    else if constexpr (contiguous_iterator<_ForwardIterator>)
	      {
		auto __dest = std::to_address(__first);
		auto __n = __last - __first;
		if (__n > 0) [[__likely__]]
		  __builtin_memset(__dest, (unsigned char)__x, __n);
		return;
	      }
#endif
	  }
      std::__do_uninit_fill(__first, __last, __x);
#pragma GCC diagnostic pop
#else // C++98
      const bool __can_memset = __is_byte<_ValueType>::__value
				  && __is_integer<_Tp>::__value;

      __uninitialized_fill<__can_memset>::__uninit_fill(__first, __last, __x);
#endif
    }

  /// @cond undocumented

  // This is the default implementation of std::uninitialized_fill_n.
  template<typename _ForwardIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator
    __do_uninit_fill_n(_ForwardIterator __first, _Size __n, const _Tp& __x)
    {
      _UninitDestroyGuard<_ForwardIterator> __guard(__first);
#if __cplusplus >= 201103L
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
      if constexpr (is_integral<_Size>::value)
	// Loop will never terminate if __n is negative.
	__glibcxx_assert(__n >= 0);
      else if constexpr (is_floating_point<_Size>::value)
	// Loop will never terminate if __n is not an integer.
	__glibcxx_assert(__n >= 0 && static_cast<size_t>(__n) == __n);
#pragma GCC diagnostic pop
#endif
      for (; __n--; ++__first)
	std::_Construct(std::__addressof(*__first), __x);
      __guard.release();
      return __first;
    }

#if __cplusplus < 201103L
  // Use template specialization for C++98 when 'if constexpr' can't be used.
  template<bool _CanMemset>
    struct __uninitialized_fill_n
    {
      template<typename _ForwardIterator, typename _Size, typename _Tp>
	static _ForwardIterator
        __uninit_fill_n(_ForwardIterator __first, _Size __n,
			const _Tp& __x)
	{ return std::__do_uninit_fill_n(__first, __n, __x); }
    };

  template<>
    struct __uninitialized_fill_n<true>
    {
      // Overload for generic iterators.
      template<typename _ForwardIterator, typename _Size, typename _Tp>
	static _ForwardIterator
        __uninit_fill_n(_ForwardIterator __first, _Size __n,
			const _Tp& __x)
	{
	  if (__unwrappable_niter<_ForwardIterator>::__value)
	    {
	      _ForwardIterator __last = __first;
	      std::advance(__last, __n);
	      __uninitialized_fill<true>::__uninit_fill(__first, __last, __x);
	      return __last;
	    }
	  else
	    return std::__do_uninit_fill_n(__first, __n, __x);
	}
    };
#endif
  /// @endcond

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
   // _GLIBCXX_RESOLVE_LIB_DEFECTS
   // DR 1339. uninitialized_fill_n should return the end of its range
  /**
   *  @brief Copies the value x into the range [first,first+n).
   *  @param  __first  A forward iterator.
   *  @param  __n      The number of copies to make.
   *  @param  __x      The source value.
   *  @return   __first + __n.
   *
   *  Like std::fill_n, but does not require an initialized output range.
  */
  template<typename _ForwardIterator, typename _Size, typename _Tp>
    inline _ForwardIterator
    uninitialized_fill_n(_ForwardIterator __first, _Size __n, const _Tp& __x)
    {
      // See uninitialized_fill conditions. We also require _Size to be
      // an integer. The standard only requires _Size to be decrementable
      // and contextually convertible to bool, so don't assume first+n works.

      // FIXME: We could additionally enable this for 1-byte enums.

      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType;

#if __cplusplus >= 201103L
      if constexpr (__is_byte<_ValueType>::__value)
	if constexpr (is_integral<_Tp>::value)
	  if constexpr (is_integral<_Size>::value)
	    {
	      using _BasePtr = decltype(std::__niter_base(__first));
	      if constexpr (is_pointer<_BasePtr>::value)
		{
		  void* __dest = std::__niter_base(__first);
		  if (__n > 0) [[__likely__]]
		    {
		      __builtin_memset(__dest, (unsigned char)__x, __n);
		      __first += __n;
		    }
		  return __first;
		}
#if __cpp_lib_concepts
	      else if constexpr (contiguous_iterator<_ForwardIterator>)
		{
		  auto __dest = std::to_address(__first);
		  if (__n > 0) [[__likely__]]
		    {
		      __builtin_memset(__dest, (unsigned char)__x, __n);
		      __first += __n;
		    }
		  return __first;
		}
#endif
	    }
      return std::__do_uninit_fill_n(__first, __n, __x);
#else // C++98
      const bool __can_memset = __is_byte<_ValueType>::__value
				  && __is_integer<_Tp>::__value
				  && __is_integer<_Size>::__value;

      return __uninitialized_fill_n<__can_memset>::
	__uninit_fill_n(__first, __n, __x);
#endif
    }
#pragma GCC diagnostic pop

  /// @cond undocumented

  // Extensions: versions of uninitialized_copy, uninitialized_fill,
  //  and uninitialized_fill_n that take an allocator parameter.
  //  We dispatch back to the standard versions when we're given the
  //  default allocator.  For nondefault allocators we do not use
  //  any of the POD optimizations.

  template<typename _InputIterator, typename _Sentinel,
	   typename _ForwardIterator, typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator
    __uninitialized_copy_a(_InputIterator __first, _Sentinel __last,
			   _ForwardIterator __result, _Allocator& __alloc)
    {
      _UninitDestroyGuard<_ForwardIterator, _Allocator>
	__guard(__result, __alloc);

      typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
      for (; __first != __last; ++__first, (void)++__result)
	__traits::construct(__alloc, std::__addressof(*__result), *__first);
      __guard.release();
      return __result;
    }

#if _GLIBCXX_HOSTED
  template<typename _InputIterator, typename _Sentinel,
	   typename _ForwardIterator, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __uninitialized_copy_a(_InputIterator __first, _Sentinel __last,
			   _ForwardIterator __result, allocator<_Tp>&)
    {
#ifdef __cpp_lib_is_constant_evaluated
      if (std::is_constant_evaluated())
	return std::__do_uninit_copy(std::move(__first), __last, __result);
#endif

#ifdef __glibcxx_ranges
      if constexpr (!is_same_v<_InputIterator, _Sentinel>)
	{
	  // Convert to a common range if possible, to benefit from memcpy
	  // optimizations that std::uninitialized_copy might use.
	  if constexpr (sized_sentinel_for<_Sentinel, _InputIterator>
			  && random_access_iterator<_InputIterator>)
	    return std::uninitialized_copy(__first,
					   __first + (__last - __first),
					   __result);
	  else // Just use default implementation.
	    return std::__do_uninit_copy(std::move(__first), __last, __result);
	}
      else
	return std::uninitialized_copy(std::move(__first), __last, __result);
#else
      return std::uninitialized_copy(__first, __last, __result);
#endif
    }
#endif

  template<typename _InputIterator, typename _ForwardIterator,
	   typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __uninitialized_move_a(_InputIterator __first, _InputIterator __last,
			   _ForwardIterator __result, _Allocator& __alloc)
    {
      return std::__uninitialized_copy_a(_GLIBCXX_MAKE_MOVE_ITERATOR(__first),
					 _GLIBCXX_MAKE_MOVE_ITERATOR(__last),
					 __result, __alloc);
    }

  template<typename _InputIterator, typename _ForwardIterator,
	   typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __uninitialized_move_if_noexcept_a(_InputIterator __first,
				       _InputIterator __last,
				       _ForwardIterator __result,
				       _Allocator& __alloc)
    {
      return std::__uninitialized_copy_a
	(_GLIBCXX_MAKE_MOVE_IF_NOEXCEPT_ITERATOR(__first),
	 _GLIBCXX_MAKE_MOVE_IF_NOEXCEPT_ITERATOR(__last), __result, __alloc);
    }

  template<typename _ForwardIterator, typename _Tp, typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    void
    __uninitialized_fill_a(_ForwardIterator __first, _ForwardIterator __last,
			   const _Tp& __x, _Allocator& __alloc)
    {
      _UninitDestroyGuard<_ForwardIterator, _Allocator>
	__guard(__first, __alloc);

      typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
      for (; __first != __last; ++__first)
	__traits::construct(__alloc, std::__addressof(*__first), __x);

      __guard.release();
    }

#if _GLIBCXX_HOSTED
  template<typename _ForwardIterator, typename _Tp, typename _Tp2>
    _GLIBCXX20_CONSTEXPR
    inline void
    __uninitialized_fill_a(_ForwardIterator __first, _ForwardIterator __last,
			   const _Tp& __x, allocator<_Tp2>&)
    {
#ifdef __cpp_lib_is_constant_evaluated
      if (std::is_constant_evaluated())
	return std::__do_uninit_fill(__first, __last, __x);
#endif
      std::uninitialized_fill(__first, __last, __x);
    }
#endif

  template<typename _ForwardIterator, typename _Size, typename _Tp,
	   typename _Allocator>
     _GLIBCXX20_CONSTEXPR
    _ForwardIterator
    __uninitialized_fill_n_a(_ForwardIterator __first, _Size __n,
			     const _Tp& __x, _Allocator& __alloc)
    {
      _UninitDestroyGuard<_ForwardIterator, _Allocator>
	__guard(__first, __alloc);
      typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
      for (; __n > 0; --__n, (void) ++__first)
	__traits::construct(__alloc, std::__addressof(*__first), __x);
      __guard.release();
      return __first;
    }

#if _GLIBCXX_HOSTED
  template<typename _ForwardIterator, typename _Size, typename _Tp,
	   typename _Tp2>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __uninitialized_fill_n_a(_ForwardIterator __first, _Size __n,
			     const _Tp& __x, allocator<_Tp2>&)
    {
#ifdef __cpp_lib_is_constant_evaluated
      if (std::is_constant_evaluated())
	return std::__do_uninit_fill_n(__first, __n, __x);
#endif
      return std::uninitialized_fill_n(__first, __n, __x);
    }
#endif

  // Extensions: __uninitialized_copy_move, __uninitialized_move_copy,
  // __uninitialized_fill_move, __uninitialized_move_fill.
  // All of these algorithms take a user-supplied allocator, which is used
  // for construction and destruction.

  // __uninitialized_copy_move
  // Copies [first1, last1) into [result, result + (last1 - first1)), and
  //  move [first2, last2) into
  //  [result, result + (last1 - first1) + (last2 - first2)).
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _ForwardIterator, typename _Allocator>
    inline _ForwardIterator
    __uninitialized_copy_move(_InputIterator1 __first1,
			      _InputIterator1 __last1,
			      _InputIterator2 __first2,
			      _InputIterator2 __last2,
			      _ForwardIterator __result,
			      _Allocator& __alloc)
    {
      _ForwardIterator __mid = std::__uninitialized_copy_a(__first1, __last1,
							   __result, __alloc);
      _UninitDestroyGuard<_ForwardIterator, _Allocator> __guard(__result,
								__alloc);
      __result = __mid; // Everything up to __mid is now guarded.
      __result = std::__uninitialized_move_a(__first2, __last2, __mid, __alloc);
      __guard.release();
      return __result;
    }

  // __uninitialized_move_copy
  // Moves [first1, last1) into [result, result + (last1 - first1)), and
  //  copies [first2, last2) into
  //  [result, result + (last1 - first1) + (last2 - first2)).
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _ForwardIterator, typename _Allocator>
    inline _ForwardIterator
    __uninitialized_move_copy(_InputIterator1 __first1,
			      _InputIterator1 __last1,
			      _InputIterator2 __first2,
			      _InputIterator2 __last2,
			      _ForwardIterator __result,
			      _Allocator& __alloc)
    {
      _ForwardIterator __mid = std::__uninitialized_move_a(__first1, __last1,
							   __result, __alloc);
      _UninitDestroyGuard<_ForwardIterator, _Allocator> __guard(__result,
								__alloc);
      __result = __mid; // Everything up to __mid is now guarded.
      __result = std::__uninitialized_copy_a(__first2, __last2, __mid, __alloc);
      __guard.release();
      return __result;
    }

  // __uninitialized_fill_move
  // Fills [result, mid) with x, and moves [first, last) into
  //  [mid, mid + (last - first)).
  template<typename _ForwardIterator, typename _Tp, typename _InputIterator,
	   typename _Allocator>
    inline _ForwardIterator
    __uninitialized_fill_move(_ForwardIterator __result, _ForwardIterator __mid,
			      const _Tp& __x, _InputIterator __first,
			      _InputIterator __last, _Allocator& __alloc)
    {
      std::__uninitialized_fill_a(__result, __mid, __x, __alloc);
      _UninitDestroyGuard<_ForwardIterator, _Allocator> __guard(__result,
								__alloc);
      __result = __mid; // Everything up to __mid is now guarded.
      __result = std::__uninitialized_move_a(__first, __last, __mid, __alloc);
      __guard.release();
      return __result;
    }

  // __uninitialized_move_fill
  // Moves [first1, last1) into [first2, first2 + (last1 - first1)), and
  //  fills [first2 + (last1 - first1), last2) with x.
  template<typename _InputIterator, typename _ForwardIterator, typename _Tp,
	   typename _Allocator>
    inline void
    __uninitialized_move_fill(_InputIterator __first1, _InputIterator __last1,
			      _ForwardIterator __first2,
			      _ForwardIterator __last2, const _Tp& __x,
			      _Allocator& __alloc)
    {
      _ForwardIterator __mid2 = std::__uninitialized_move_a(__first1, __last1,
							    __first2,
							    __alloc);
      _UninitDestroyGuard<_ForwardIterator, _Allocator> __guard(__first2,
								__alloc);
      __first2 = __mid2; // Everything up to __mid2 is now guarded.
      std::__uninitialized_fill_a(__mid2, __last2, __x, __alloc);
      __guard.release();
    }

  /// @endcond

#if __cplusplus >= 201103L
  /// @cond undocumented

  // Extensions: __uninitialized_default, __uninitialized_default_n,
  // __uninitialized_default_a, __uninitialized_default_n_a.

  template<bool _TrivialValueType>
    struct __uninitialized_default_1
    {
      template<typename _ForwardIterator>
        static void
        __uninit_default(_ForwardIterator __first, _ForwardIterator __last)
        {
	  _UninitDestroyGuard<_ForwardIterator> __guard(__first);
	  for (; __first != __last; ++__first)
	    std::_Construct(std::__addressof(*__first));
	  __guard.release();
	}
    };

  template<>
    struct __uninitialized_default_1<true>
    {
      template<typename _ForwardIterator>
        static void
        __uninit_default(_ForwardIterator __first, _ForwardIterator __last)
        {
	  if (__first == __last)
	    return;

	  typename iterator_traits<_ForwardIterator>::value_type* __val
	    = std::__addressof(*__first);
	  std::_Construct(__val);
	  if (++__first != __last)
	    std::fill(__first, __last, *__val);
	}
    };

  template<bool _TrivialValueType>
    struct __uninitialized_default_n_1
    {
      template<typename _ForwardIterator, typename _Size>
	_GLIBCXX20_CONSTEXPR
        static _ForwardIterator
        __uninit_default_n(_ForwardIterator __first, _Size __n)
        {
	  _UninitDestroyGuard<_ForwardIterator> __guard(__first);
	  for (; __n > 0; --__n, (void) ++__first)
	    std::_Construct(std::__addressof(*__first));
	  __guard.release();
	  return __first;
	}
    };

  template<>
    struct __uninitialized_default_n_1<true>
    {
      template<typename _ForwardIterator, typename _Size>
	_GLIBCXX20_CONSTEXPR
        static _ForwardIterator
        __uninit_default_n(_ForwardIterator __first, _Size __n)
        {
	  if (__n > 0)
	    {
	      typename iterator_traits<_ForwardIterator>::value_type* __val
		= std::__addressof(*__first);
	      std::_Construct(__val);
	      ++__first;
	      __first = std::fill_n(__first, __n - 1, *__val);
	    }
	  return __first;
	}
    };

  // __uninitialized_default
  // Fills [first, last) with value-initialized value_types.
  template<typename _ForwardIterator>
    inline void
    __uninitialized_default(_ForwardIterator __first,
			    _ForwardIterator __last)
    {
      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType;
      // trivial types can have deleted assignment
      const bool __assignable = is_copy_assignable<_ValueType>::value;

      std::__uninitialized_default_1<__is_trivial(_ValueType)
				     && __assignable>::
	__uninit_default(__first, __last);
    }

  // __uninitialized_default_n
  // Fills [first, first + n) with value-initialized value_types.
  template<typename _ForwardIterator, typename _Size>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __uninitialized_default_n(_ForwardIterator __first, _Size __n)
    {
#ifdef __cpp_lib_is_constant_evaluated
      if (std::is_constant_evaluated())
	return __uninitialized_default_n_1<false>::
		 __uninit_default_n(__first, __n);
#endif

      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType;
      // See uninitialized_fill_n for the conditions for using std::fill_n.
      constexpr bool __can_fill
	= __and_<is_integral<_Size>, is_copy_assignable<_ValueType>>::value;

      return __uninitialized_default_n_1<__is_trivial(_ValueType)
					 && __can_fill>::
	__uninit_default_n(__first, __n);
    }


  // __uninitialized_default_a
  // Fills [first, last) with value_types constructed by the allocator
  // alloc, with no arguments passed to the construct call.
  template<typename _ForwardIterator, typename _Allocator>
    void
    __uninitialized_default_a(_ForwardIterator __first,
			      _ForwardIterator __last,
			      _Allocator& __alloc)
    {
      _UninitDestroyGuard<_ForwardIterator, _Allocator> __guard(__first,
								__alloc);
      typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
      for (; __first != __last; ++__first)
	__traits::construct(__alloc, std::__addressof(*__first));
      __guard.release();
    }

#if _GLIBCXX_HOSTED
  template<typename _ForwardIterator, typename _Tp>
    inline void
    __uninitialized_default_a(_ForwardIterator __first,
			      _ForwardIterator __last,
			      allocator<_Tp>&)
    { std::__uninitialized_default(__first, __last); }
#endif

  // __uninitialized_default_n_a
  // Fills [first, first + n) with value_types constructed by the allocator
  // alloc, with no arguments passed to the construct call.
  template<typename _ForwardIterator, typename _Size, typename _Allocator>
    _GLIBCXX20_CONSTEXPR _ForwardIterator
    __uninitialized_default_n_a(_ForwardIterator __first, _Size __n,
				_Allocator& __alloc)
    {
      _UninitDestroyGuard<_ForwardIterator, _Allocator> __guard(__first,
								__alloc);
      typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
      for (; __n > 0; --__n, (void) ++__first)
	__traits::construct(__alloc, std::__addressof(*__first));
      __guard.release();
      return __first;
    }

#if _GLIBCXX_HOSTED
  // __uninitialized_default_n_a specialization for std::allocator,
  // which ignores the allocator and value-initializes the elements.
  template<typename _ForwardIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __uninitialized_default_n_a(_ForwardIterator __first, _Size __n,
				allocator<_Tp>&)
    { return std::__uninitialized_default_n(__first, __n); }
#endif

  template<bool _TrivialValueType>
    struct __uninitialized_default_novalue_1
    {
      template<typename _ForwardIterator>
	static void
	__uninit_default_novalue(_ForwardIterator __first,
				 _ForwardIterator __last)
	{
	  _UninitDestroyGuard<_ForwardIterator> __guard(__first);
	  for (; __first != __last; ++__first)
	    std::_Construct_novalue(std::__addressof(*__first));
	  __guard.release();
	}
    };

  template<>
    struct __uninitialized_default_novalue_1<true>
    {
      template<typename _ForwardIterator>
        static void
        __uninit_default_novalue(_ForwardIterator, _ForwardIterator)
	{
	}
    };

  template<bool _TrivialValueType>
    struct __uninitialized_default_novalue_n_1
    {
      template<typename _ForwardIterator, typename _Size>
	static _ForwardIterator
	__uninit_default_novalue_n(_ForwardIterator __first, _Size __n)
	{
	  _UninitDestroyGuard<_ForwardIterator> __guard(__first);
	  for (; __n > 0; --__n, (void) ++__first)
	    std::_Construct_novalue(std::__addressof(*__first));
	  __guard.release();
	  return __first;
	}
    };

  template<>
    struct __uninitialized_default_novalue_n_1<true>
    {
      template<typename _ForwardIterator, typename _Size>
	static _ForwardIterator
	__uninit_default_novalue_n(_ForwardIterator __first, _Size __n)
	{ return std::next(__first, __n); }
    };

  // __uninitialized_default_novalue
  // Fills [first, last) with default-initialized value_types.
  template<typename _ForwardIterator>
    inline void
    __uninitialized_default_novalue(_ForwardIterator __first,
				    _ForwardIterator __last)
    {
      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType;

      std::__uninitialized_default_novalue_1<
	is_trivially_default_constructible<_ValueType>::value>::
	__uninit_default_novalue(__first, __last);
    }

  // __uninitialized_default_novalue_n
  // Fills [first, first + n) with default-initialized value_types.
  template<typename _ForwardIterator, typename _Size>
    inline _ForwardIterator
    __uninitialized_default_novalue_n(_ForwardIterator __first, _Size __n)
    {
      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType;

      return __uninitialized_default_novalue_n_1<
	is_trivially_default_constructible<_ValueType>::value>::
	__uninit_default_novalue_n(__first, __n);
    }

  template<typename _InputIterator, typename _Size,
	   typename _ForwardIterator>
    _ForwardIterator
    __uninitialized_copy_n(_InputIterator __first, _Size __n,
			   _ForwardIterator __result, input_iterator_tag)
    {
      _UninitDestroyGuard<_ForwardIterator> __guard(__result);
      for (; __n > 0; --__n, (void) ++__first, ++__result)
	std::_Construct(std::__addressof(*__result), *__first);
      __guard.release();
      return __result;
    }

  template<typename _RandomAccessIterator, typename _Size,
	   typename _ForwardIterator>
    inline _ForwardIterator
    __uninitialized_copy_n(_RandomAccessIterator __first, _Size __n,
			   _ForwardIterator __result,
			   random_access_iterator_tag)
    { return std::uninitialized_copy(__first, __first + __n, __result); }

  template<typename _InputIterator, typename _Size,
	   typename _ForwardIterator>
    pair<_InputIterator, _ForwardIterator>
    __uninitialized_copy_n_pair(_InputIterator __first, _Size __n,
				_ForwardIterator __result, input_iterator_tag)
    {
      _UninitDestroyGuard<_ForwardIterator> __guard(__result);
      for (; __n > 0; --__n, (void) ++__first, ++__result)
	std::_Construct(std::__addressof(*__result), *__first);
      __guard.release();
      return {__first, __result};
    }

  template<typename _RandomAccessIterator, typename _Size,
	   typename _ForwardIterator>
    inline pair<_RandomAccessIterator, _ForwardIterator>
    __uninitialized_copy_n_pair(_RandomAccessIterator __first, _Size __n,
			   _ForwardIterator __result,
			   random_access_iterator_tag)
    {
      auto __second_res = uninitialized_copy(__first, __first + __n, __result);
      auto __first_res = std::next(__first, __n);
      return {__first_res, __second_res};
    }

  /// @endcond

  /**
   *  @brief Copies the range [first,first+n) into result.
   *  @param  __first  An input iterator.
   *  @param  __n      The number of elements to copy.
   *  @param  __result An output iterator.
   *  @return  __result + __n
   *  @since C++11
   *
   *  Like copy_n(), but does not require an initialized output range.
  */
  template<typename _InputIterator, typename _Size, typename _ForwardIterator>
    inline _ForwardIterator
    uninitialized_copy_n(_InputIterator __first, _Size __n,
			 _ForwardIterator __result)
    { return std::__uninitialized_copy_n(__first, __n, __result,
					 std::__iterator_category(__first)); }

  /// @cond undocumented
  template<typename _InputIterator, typename _Size, typename _ForwardIterator>
    inline pair<_InputIterator, _ForwardIterator>
    __uninitialized_copy_n_pair(_InputIterator __first, _Size __n,
			      _ForwardIterator __result)
    {
      return
	std::__uninitialized_copy_n_pair(__first, __n, __result,
					 std::__iterator_category(__first));
    }
  /// @endcond
#endif

#ifdef __glibcxx_raw_memory_algorithms // C++ >= 17
  /**
   *  @brief Default-initializes objects in the range [first,last).
   *  @param  __first  A forward iterator.
   *  @param  __last   A forward iterator.
   *  @since C++17
  */
  template <typename _ForwardIterator>
    inline void
    uninitialized_default_construct(_ForwardIterator __first,
				    _ForwardIterator __last)
    {
      std::__uninitialized_default_novalue(__first, __last);
    }

  /**
   *  @brief Default-initializes objects in the range [first,first+count).
   *  @param  __first  A forward iterator.
   *  @param  __count  The number of objects to construct.
   *  @return   __first + __count
   *  @since C++17
  */
  template <typename _ForwardIterator, typename _Size>
    inline _ForwardIterator
    uninitialized_default_construct_n(_ForwardIterator __first, _Size __count)
    {
      return std::__uninitialized_default_novalue_n(__first, __count);
    }

  /**
   *  @brief Value-initializes objects in the range [first,last).
   *  @param  __first  A forward iterator.
   *  @param  __last   A forward iterator.
   *  @since C++17
  */
  template <typename _ForwardIterator>
    inline void
    uninitialized_value_construct(_ForwardIterator __first,
				  _ForwardIterator __last)
    {
      return std::__uninitialized_default(__first, __last);
    }

  /**
   *  @brief Value-initializes objects in the range [first,first+count).
   *  @param  __first  A forward iterator.
   *  @param  __count  The number of objects to construct.
   *  @return   __result + __count
   *  @since C++17
  */
  template <typename _ForwardIterator, typename _Size>
    inline _ForwardIterator
    uninitialized_value_construct_n(_ForwardIterator __first, _Size __count)
    {
      return std::__uninitialized_default_n(__first, __count);
    }

  /**
   *  @brief Move-construct from the range [first,last) into result.
   *  @param  __first  An input iterator.
   *  @param  __last   An input iterator.
   *  @param  __result An output iterator.
   *  @return   __result + (__first - __last)
   *  @since C++17
  */
  template <typename _InputIterator, typename _ForwardIterator>
    inline _ForwardIterator
    uninitialized_move(_InputIterator __first, _InputIterator __last,
		       _ForwardIterator __result)
    {
      return std::uninitialized_copy
	(_GLIBCXX_MAKE_MOVE_ITERATOR(__first),
	 _GLIBCXX_MAKE_MOVE_ITERATOR(__last), __result);
    }

  /**
   *  @brief Move-construct from the range [first,first+count) into result.
   *  @param  __first  An input iterator.
   *  @param  __count  The number of objects to initialize.
   *  @param  __result An output iterator.
   *  @return  __result + __count
   *  @since C++17
  */
  template <typename _InputIterator, typename _Size, typename _ForwardIterator>
    inline pair<_InputIterator, _ForwardIterator>
    uninitialized_move_n(_InputIterator __first, _Size __count,
			 _ForwardIterator __result)
    {
      auto __res = std::__uninitialized_copy_n_pair
	(_GLIBCXX_MAKE_MOVE_ITERATOR(__first),
	 __count, __result);
      return {__res.first.base(), __res.second};
    }
#endif // __glibcxx_raw_memory_algorithms

#if __cplusplus >= 201103L
  /// @cond undocumented

  template<typename _Tp, typename _Up, typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    inline void
    __relocate_object_a(_Tp* __restrict __dest, _Up* __restrict __orig,
			_Allocator& __alloc)
    noexcept(noexcept(std::allocator_traits<_Allocator>::construct(__alloc,
			 __dest, std::move(*__orig)))
	     && noexcept(std::allocator_traits<_Allocator>::destroy(
			    __alloc, std::__addressof(*__orig))))
    {
      typedef std::allocator_traits<_Allocator> __traits;
      __traits::construct(__alloc, __dest, std::move(*__orig));
      __traits::destroy(__alloc, std::__addressof(*__orig));
    }

  // This class may be specialized for specific types.
  // Also known as is_trivially_relocatable.
  template<typename _Tp, typename = void>
    struct __is_bitwise_relocatable
    : is_trivial<_Tp> { };

  template <typename _InputIterator, typename _ForwardIterator,
	    typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __relocate_a_1(_InputIterator __first, _InputIterator __last,
		   _ForwardIterator __result, _Allocator& __alloc)
    noexcept(noexcept(std::__relocate_object_a(std::addressof(*__result),
					       std::addressof(*__first),
					       __alloc)))
    {
      typedef typename iterator_traits<_InputIterator>::value_type
	_ValueType;
      typedef typename iterator_traits<_ForwardIterator>::value_type
	_ValueType2;
      static_assert(std::is_same<_ValueType, _ValueType2>::value,
	  "relocation is only possible for values of the same type");
      _ForwardIterator __cur = __result;
      for (; __first != __last; ++__first, (void)++__cur)
	std::__relocate_object_a(std::__addressof(*__cur),
				 std::__addressof(*__first), __alloc);
      return __cur;
    }

#if _GLIBCXX_HOSTED
  template <typename _Tp, typename _Up>
    _GLIBCXX20_CONSTEXPR
    inline __enable_if_t<std::__is_bitwise_relocatable<_Tp>::value, _Tp*>
    __relocate_a_1(_Tp* __first, _Tp* __last,
		   _Tp* __result,
		   [[__maybe_unused__]] allocator<_Up>& __alloc) noexcept
    {
      ptrdiff_t __count = __last - __first;
      if (__count > 0)
	{
#ifdef __cpp_lib_is_constant_evaluated
	  if (std::is_constant_evaluated())
	    {
	      // Can't use memcpy. Wrap the pointer so that __relocate_a_1
	      // resolves to the non-trivial overload above.
	      __gnu_cxx::__normal_iterator<_Tp*, void> __out(__result);
	      __out = std::__relocate_a_1(__first, __last, __out, __alloc);
	      return __out.base();
	    }
#endif
	  __builtin_memcpy(__result, __first, __count * sizeof(_Tp));
	}
      return __result + __count;
    }
#endif

  template <typename _InputIterator, typename _ForwardIterator,
	    typename _Allocator>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    __relocate_a(_InputIterator __first, _InputIterator __last,
		 _ForwardIterator __result, _Allocator& __alloc)
    noexcept(noexcept(__relocate_a_1(std::__niter_base(__first),
				     std::__niter_base(__last),
				     std::__niter_base(__result), __alloc)))
    {
      return std::__relocate_a_1(std::__niter_base(__first),
				 std::__niter_base(__last),
				 std::__niter_base(__result), __alloc);
    }

  /// @endcond
#endif // C++11

  /// @} group memory

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif /* _STL_UNINITIALIZED_H */
