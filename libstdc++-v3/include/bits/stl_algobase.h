// Core algorithmic facilities -*- C++ -*-

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
 * Copyright (c) 1996-1998
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

/** @file bits/stl_algobase.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{algorithm}
 */

#ifndef _STL_ALGOBASE_H
#define _STL_ALGOBASE_H 1

#include <bits/c++config.h>
#include <bits/functexcept.h>
#include <bits/cpp_type_traits.h>
#include <ext/type_traits.h>
#include <ext/numeric_traits.h>
#include <bits/stl_pair.h>
#include <bits/stl_iterator_base_types.h>
#include <bits/stl_iterator_base_funcs.h>
#include <bits/stl_iterator.h>
#include <bits/concept_check.h>
#include <debug/debug.h>
#include <bits/move.h> // For std::swap
#include <bits/predefined_ops.h>
#if __cplusplus >= 201103L
# include <type_traits>
#endif
#if __cplusplus >= 201402L
# include <bit> // std::__bit_width
#endif
#if __cplusplus >= 202002L
# include <compare>
# include <bits/ptr_traits.h> // std::to_address
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /*
   * A constexpr wrapper for __builtin_memcmp.
   * @param __num The number of elements of type _Tp (not bytes).
   */
  template<typename _Tp, typename _Up>
    _GLIBCXX14_CONSTEXPR
    inline int
    __memcmp(const _Tp* __first1, const _Up* __first2, size_t __num)
    {
#if __cplusplus >= 201103L
      static_assert(sizeof(_Tp) == sizeof(_Up), "can be compared with memcmp");
#endif
#ifdef __cpp_lib_is_constant_evaluated
      if (std::is_constant_evaluated())
	{
	  for(; __num > 0; ++__first1, ++__first2, --__num)
	    if (*__first1 != *__first2)
	      return *__first1 < *__first2 ? -1 : 1;
	  return 0;
	}
      else
#endif
	return __builtin_memcmp(__first1, __first2, sizeof(_Tp) * __num);
    }

#if __cplusplus < 201103L
  // See http://gcc.gnu.org/ml/libstdc++/2004-08/msg00167.html: in a
  // nutshell, we are partially implementing the resolution of DR 187,
  // when it's safe, i.e., the value_types are equal.
  template<bool _BoolType>
    struct __iter_swap
    {
      template<typename _ForwardIterator1, typename _ForwardIterator2>
	static void
	iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
	{
	  typedef typename iterator_traits<_ForwardIterator1>::value_type
	    _ValueType1;
	  _ValueType1 __tmp = *__a;
	  *__a = *__b;
	  *__b = __tmp;
	}
    };

  template<>
    struct __iter_swap<true>
    {
      template<typename _ForwardIterator1, typename _ForwardIterator2>
	static void
	iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
	{
	  swap(*__a, *__b);
	}
    };
#endif // C++03

  /**
   *  @brief Swaps the contents of two iterators.
   *  @ingroup mutating_algorithms
   *  @param  __a  An iterator.
   *  @param  __b  Another iterator.
   *  @return   Nothing.
   *
   *  This function swaps the values pointed to by two iterators, not the
   *  iterators themselves.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _GLIBCXX20_CONSTEXPR
    inline void
    iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator1>)
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator2>)

#if __cplusplus < 201103L
      typedef typename iterator_traits<_ForwardIterator1>::value_type
	_ValueType1;
      typedef typename iterator_traits<_ForwardIterator2>::value_type
	_ValueType2;

      __glibcxx_function_requires(_ConvertibleConcept<_ValueType1,
				  _ValueType2>)
      __glibcxx_function_requires(_ConvertibleConcept<_ValueType2,
				  _ValueType1>)

      typedef typename iterator_traits<_ForwardIterator1>::reference
	_ReferenceType1;
      typedef typename iterator_traits<_ForwardIterator2>::reference
	_ReferenceType2;
      std::__iter_swap<__are_same<_ValueType1, _ValueType2>::__value
	&& __are_same<_ValueType1&, _ReferenceType1>::__value
	&& __are_same<_ValueType2&, _ReferenceType2>::__value>::
	iter_swap(__a, __b);
#else
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 187. iter_swap underspecified
      swap(*__a, *__b);
#endif
    }

  /**
   *  @brief Swap the elements of two sequences.
   *  @ingroup mutating_algorithms
   *  @param  __first1  A forward iterator.
   *  @param  __last1   A forward iterator.
   *  @param  __first2  A forward iterator.
   *  @return   An iterator equal to @p first2+(last1-first1).
   *
   *  Swaps each element in the range @p [first1,last1) with the
   *  corresponding element in the range @p [first2,(last1-first1)).
   *  The ranges must not overlap.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator2
    swap_ranges(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
		_ForwardIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator1>)
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for (; __first1 != __last1; ++__first1, (void)++__first2)
	std::iter_swap(__first1, __first2);
      return __first2;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @return   The lesser of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
    _GLIBCXX_NODISCARD _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return __b < __a ? __b : __a;
      if (__b < __a)
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @return   The greater of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
    _GLIBCXX_NODISCARD _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return  __a < __b ? __b : __a;
      if (__a < __b)
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @param  __comp  A @link comparison_functors comparison functor@endlink.
   *  @return   The lesser of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
    _GLIBCXX_NODISCARD _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__b, __a) ? __b : __a;
      if (__comp(__b, __a))
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @ingroup sorting_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @param  __comp  A @link comparison_functors comparison functor@endlink.
   *  @return   The greater of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
    _GLIBCXX_NODISCARD _GLIBCXX14_CONSTEXPR
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__a, __b) ? __b : __a;
      if (__comp(__a, __b))
	return __b;
      return __a;
    }

_GLIBCXX_BEGIN_NAMESPACE_CONTAINER

  template<typename _Tp, typename _Ref, typename _Ptr>
    struct _Deque_iterator;

  struct _Bit_iterator;

_GLIBCXX_END_NAMESPACE_CONTAINER

#if _GLIBCXX_HOSTED
  // Helpers for streambuf iterators (either istream or ostream).
  // NB: avoid including <iosfwd>, relatively large.
  template<typename _CharT>
    struct char_traits;

  template<typename _CharT, typename _Traits>
    class istreambuf_iterator;

  template<typename _CharT, typename _Traits>
    class ostreambuf_iterator;

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
	     ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_move_a2(_CharT*, _CharT*,
		   ostreambuf_iterator<_CharT, char_traits<_CharT> >);

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
	     ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_move_a2(const _CharT*, const _CharT*,
		   ostreambuf_iterator<_CharT, char_traits<_CharT> >);

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
				    _CharT*>::__type
    __copy_move_a2(istreambuf_iterator<_CharT, char_traits<_CharT> >,
		   istreambuf_iterator<_CharT, char_traits<_CharT> >, _CharT*);

  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<
      __is_char<_CharT>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*> >::__type
    __copy_move_a2(
	istreambuf_iterator<_CharT, char_traits<_CharT> >,
	istreambuf_iterator<_CharT, char_traits<_CharT> >,
	_GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*>);
#endif // HOSTED

#if __cpp_lib_concepts
  template<typename _OutIter, typename _InIter, typename _Sent = _InIter>
    concept __memcpyable_iterators
      = contiguous_iterator<_OutIter> && contiguous_iterator<_InIter>
	  && sized_sentinel_for<_Sent, _InIter>
	  && requires (_OutIter __o, _InIter __i) {
	    requires !!__memcpyable<decltype(std::to_address(__o)),
				    decltype(std::to_address(__i))>::__value;
	  };
#endif

#if __cplusplus < 201103L
  // Used by __copy_move_a2, __copy_n_a and __copy_move_backward_a2 to
  // get raw pointers so that calls to __builtin_memmove will compile,
  // because C++98 can't use 'if constexpr' so statements that use memmove
  // with pointer arguments need to also compile for arbitrary iterator types.
  template<typename _Iter> __attribute__((__always_inline__))
    inline void* __ptr_or_null(_Iter) { return 0; }
  template<typename _Tp> __attribute__((__always_inline__))
  inline void* __ptr_or_null(_Tp* __p) { return (void*)__p; }
# define _GLIBCXX_TO_ADDR(P) std::__ptr_or_null(P)
  // Used to advance output iterators (std::advance requires InputIterator).
  template<typename _Iter> __attribute__((__always_inline__))
    inline void __ptr_advance(_Iter&, ptrdiff_t) { }
  template<typename _Tp> __attribute__((__always_inline__))
    inline void __ptr_advance(_Tp*& __p, ptrdiff_t __n) { __p += __n; }
# define _GLIBCXX_ADVANCE(P, N) std::__ptr_advance(P, N)
#else
  // For C++11 mode the __builtin_memmove calls are guarded by 'if constexpr'
  // so we know the iterators used with memmove are guaranteed to be pointers.
# define _GLIBCXX_TO_ADDR(P) P
# define _GLIBCXX_ADVANCE(P, N) P += N
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
  template<bool _IsMove, typename _OutIter, typename _InIter>
    __attribute__((__always_inline__)) _GLIBCXX20_CONSTEXPR
    inline void
    __assign_one(_OutIter& __out, _InIter& __in)
    {
#if __cplusplus >= 201103L
      if constexpr (_IsMove)
	*__out = std::move(*__in);
      else
#endif
	*__out = *__in;
    }

  template<bool _IsMove, typename _InIter, typename _Sent, typename _OutIter>
    _GLIBCXX20_CONSTEXPR
    inline _OutIter
    __copy_move_a2(_InIter __first, _Sent __last, _OutIter __result)
    {
      typedef __decltype(*__first) _InRef;
      typedef __decltype(*__result) _OutRef;
      if _GLIBCXX_CONSTEXPR (!__is_trivially_assignable(_OutRef, _InRef))
	{ } /* Skip the optimizations and use the loop at the end. */
      else if (std::__is_constant_evaluated())
	{ } /* Skip the optimizations and use the loop at the end. */
      else if _GLIBCXX_CONSTEXPR (__memcpyable<_OutIter, _InIter>::__value)
	{
	  ptrdiff_t __n = std::distance(__first, __last);
	  if (__builtin_expect(__n > 1, true))
	    {
	      __builtin_memmove(_GLIBCXX_TO_ADDR(__result),
				_GLIBCXX_TO_ADDR(__first),
				__n * sizeof(*__first));
	      _GLIBCXX_ADVANCE(__result, __n);
	    }
	  else if (__n == 1)
	    {
	      std::__assign_one<_IsMove>(__result, __first);
	      ++__result;
	    }
	  return __result;
	}
#if __cpp_lib_concepts
      else if constexpr (__memcpyable_iterators<_OutIter, _InIter, _Sent>)
	{
	  if (auto __n = __last - __first; __n > 1) [[likely]]
	    {
	      void* __dest = std::to_address(__result);
	      const void* __src = std::to_address(__first);
	      size_t __nbytes = __n * sizeof(iter_value_t<_InIter>);
	      // Advance the iterators and convert to pointers first.
	      // This gives the iterators a chance to do bounds checking.
	      (void) std::to_address(__result += __n);
	      (void) std::to_address(__first += __n);
	      __builtin_memmove(__dest, __src, __nbytes);
	    }
	  else if (__n == 1)
	    {
	      std::__assign_one<_IsMove>(__result, __first);
	      ++__result;
	    }
	  return __result;
	}
#endif

      for (; __first != __last; ++__result, (void)++__first)
	std::__assign_one<_IsMove>(__result, __first);
      return __result;
    }
#pragma GCC diagnostic pop

  template<bool _IsMove,
	   typename _Tp, typename _Ref, typename _Ptr, typename _OI>
    _OI
    __copy_move_a1(_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		   _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		   _OI);

  template<bool _IsMove,
	   typename _ITp, typename _IRef, typename _IPtr, typename _OTp>
    _GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>
    __copy_move_a1(_GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
		   _GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
		   _GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>);

  template<bool _IsMove, typename _II, typename _Tp>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*> >::__type
    __copy_move_a1(_II, _II, _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>);

  template<bool _IsMove, typename _II, typename _OI>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _OI
    __copy_move_a1(_II __first, _II __last, _OI __result)
    { return std::__copy_move_a2<_IsMove>(__first, __last, __result); }

  template<bool _IsMove, typename _II, typename _OI>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _OI
    __copy_move_a(_II __first, _II __last, _OI __result)
    {
      return std::__niter_wrap(__result,
		std::__copy_move_a1<_IsMove>(std::__niter_base(__first),
					     std::__niter_base(__last),
					     std::__niter_base(__result)));
    }

  template<bool _IsMove,
	   typename _Ite, typename _Seq, typename _Cat, typename _OI>
    _GLIBCXX20_CONSTEXPR
    _OI
    __copy_move_a(const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		  const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		  _OI);

  template<bool _IsMove,
	   typename _II, typename _Ite, typename _Seq, typename _Cat>
    _GLIBCXX20_CONSTEXPR
    __gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>
    __copy_move_a(_II, _II,
		  const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&);

  template<bool _IsMove,
	   typename _IIte, typename _ISeq, typename _ICat,
	   typename _OIte, typename _OSeq, typename _OCat>
    _GLIBCXX20_CONSTEXPR
    ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>
    __copy_move_a(const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		  const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		  const ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>&);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // for if-constexpr
  template<typename _InputIterator, typename _Size, typename _OutputIterator>
    _GLIBCXX20_CONSTEXPR
    _OutputIterator
    __copy_n_a(_InputIterator __first, _Size __n, _OutputIterator __result,
	       bool)
    {
      typedef __decltype(*__first) _InRef;
      typedef __decltype(*__result) _OutRef;
      if _GLIBCXX_CONSTEXPR (!__is_trivially_assignable(_OutRef, _InRef))
	{ } /* Skip the optimizations and use the loop at the end. */
#ifdef __cpp_lib_is_constant_evaluated
      else if (std::is_constant_evaluated())
	{ } /* Skip the optimizations and use the loop at the end. */
#endif
      else if _GLIBCXX_CONSTEXPR (__memcpyable<_OutputIterator,
					       _InputIterator>::__value)
	{
	  if (__builtin_expect(__n > 1, true))
	    {
	      __builtin_memmove(_GLIBCXX_TO_ADDR(__result),
				_GLIBCXX_TO_ADDR(__first),
				__n * sizeof(*__first));
	      _GLIBCXX_ADVANCE(__result, __n);
	    }
	  else if (__n == 1)
	    *__result++ = *__first;
	  return __result;
	}
#if __cpp_lib_concepts
      else if constexpr (__memcpyable_iterators<_OutputIterator,
						_InputIterator>)
	{
	  if (__n > 1) [[likely]]
	    {
	      void* __dest = std::to_address(__result);
	      const void* __src = std::to_address(__first);
	      size_t __nbytes = __n * sizeof(iter_value_t<_InputIterator>);
	      // Advance the iterators and convert to pointers first.
	      // This gives the iterators a chance to do bounds checking.
	      (void) std::to_address(__result += __n);
	      (void) std::to_address(__first += __n);
	      __builtin_memmove(__dest, __src, __nbytes);
	    }
	  else if (__n == 1)
	    *__result++ = *__first;
	  return __result;
	}
#endif

      if (__n > 0)
	{
	  while (true)
	    {
	      *__result = *__first;
	      ++__result;
	      if (--__n > 0)
		++__first;
	      else
		break;
	    }
	}
      return __result;
    }
#pragma GCC diagnostic pop

#if _GLIBCXX_HOSTED
  template<typename _CharT, typename _Size>
    typename __gnu_cxx::__enable_if<
      __is_char<_CharT>::__value, _CharT*>::__type
    __copy_n_a(istreambuf_iterator<_CharT, char_traits<_CharT> >,
	       _Size, _CharT*, bool);

  template<typename _CharT, typename _Size>
    typename __gnu_cxx::__enable_if<
      __is_char<_CharT>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*> >::__type
    __copy_n_a(istreambuf_iterator<_CharT, char_traits<_CharT> >, _Size,
	       _GLIBCXX_STD_C::_Deque_iterator<_CharT, _CharT&, _CharT*>,
	       bool);
#endif

  /**
   *  @brief Copies the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  An input iterator.
   *  @param  __last   An input iterator.
   *  @param  __result An output iterator.
   *  @return   result + (last - first)
   *
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).  Result may not be contained within
   *  [first,last); the copy_backward function should be used instead.
   *
   *  Note that the end of the output range is permitted to be contained
   *  within [first,last).
  */
  template<typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    copy(_II __first, _II __last, _OI __result)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II>)
      __glibcxx_function_requires(_OutputIteratorConcept<_OI,
	    typename iterator_traits<_II>::reference>)
      __glibcxx_requires_can_increment_range(__first, __last, __result);

      return std::__copy_move_a<__is_move_iterator<_II>::__value>
	     (std::__miter_base(__first), std::__miter_base(__last), __result);
    }

#if __cplusplus >= 201103L
  /**
   *  @brief Moves the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  An input iterator.
   *  @param  __last   An input iterator.
   *  @param  __result An output iterator.
   *  @return   result + (last - first)
   *
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).  Result may not be contained within
   *  [first,last); the move_backward function should be used instead.
   *
   *  Note that the end of the output range is permitted to be contained
   *  within [first,last).
  */
  template<typename _II, typename _OI>
    _GLIBCXX20_CONSTEXPR
    inline _OI
    move(_II __first, _II __last, _OI __result)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II>)
      __glibcxx_function_requires(_OutputIteratorConcept<_OI,
	    typename iterator_traits<_II>::value_type&&>)
      __glibcxx_requires_can_increment_range(__first, __last, __result);

      return std::__copy_move_a<true>(std::__miter_base(__first),
				      std::__miter_base(__last), __result);
    }

#define _GLIBCXX_MOVE3(_Tp, _Up, _Vp) std::move(_Tp, _Up, _Vp)
#else
#define _GLIBCXX_MOVE3(_Tp, _Up, _Vp) std::copy(_Tp, _Up, _Vp)
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
  template<bool _IsMove, typename _BI1, typename _BI2>
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    __copy_move_backward_a2(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      typedef __decltype(*__first) _InRef;
      typedef __decltype(*__result) _OutRef;
      if _GLIBCXX_CONSTEXPR (!__is_trivially_assignable(_OutRef, _InRef))
       { } /* Skip the optimizations and use the loop at the end. */
#ifdef __cpp_lib_is_constant_evaluated
      else if (std::is_constant_evaluated())
       { } /* Skip the optimizations and use the loop at the end. */
#endif
      else if _GLIBCXX_CONSTEXPR (__memcpyable<_BI2, _BI1>::__value)
	{
	  ptrdiff_t __n = std::distance(__first, __last);
	  std::advance(__result, -__n);
	  if (__builtin_expect(__n > 1, true))
	    {
	      __builtin_memmove(_GLIBCXX_TO_ADDR(__result),
				_GLIBCXX_TO_ADDR(__first),
				__n * sizeof(*__first));
	    }
	  else if (__n == 1)
	    std::__assign_one<_IsMove>(__result, __first);
	  return __result;
	}
#if __cpp_lib_concepts
      else if constexpr (__memcpyable_iterators<_BI2, _BI1>)
	{
	  if (auto __n = __last - __first; __n > 1) [[likely]]
	    {
	      const void* __src = std::to_address(__first);
	      // Advance the iterators and convert to pointers first.
	      // This gives the iterators a chance to do bounds checking.
	      (void) std::to_address(__result -= __n);
	      (void) std::to_address(__first += __n);
	      void* __dest = std::to_address(__result);
	      size_t __nbytes = __n * sizeof(iter_value_t<_BI1>);
	      __builtin_memmove(__dest, __src, __nbytes);
	    }
	  else if (__n == 1)
	    {
	      --__result;
	      std::__assign_one<_IsMove>(__result, __first);
	    }
	  return __result;
	}
#endif

      while (__first != __last)
	{
	  --__last;
	  --__result;
	  std::__assign_one<_IsMove>(__result, __last);
	}
      return __result;
    }
#pragma GCC diagnostic pop

#undef _GLIBCXX_TO_ADDR
#undef _GLIBCXX_ADVANCE

  template<bool _IsMove, typename _BI1, typename _BI2>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    __copy_move_backward_a1(_BI1 __first, _BI1 __last, _BI2 __result)
    { return std::__copy_move_backward_a2<_IsMove>(__first, __last, __result); }

  template<bool _IsMove,
	   typename _Tp, typename _Ref, typename _Ptr, typename _OI>
    _OI
    __copy_move_backward_a1(_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
			    _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
			    _OI);

  template<bool _IsMove,
	   typename _ITp, typename _IRef, typename _IPtr, typename _OTp>
    _GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>
    __copy_move_backward_a1(
			_GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
			_GLIBCXX_STD_C::_Deque_iterator<_ITp, _IRef, _IPtr>,
			_GLIBCXX_STD_C::_Deque_iterator<_OTp, _OTp&, _OTp*>);

  template<bool _IsMove, typename _II, typename _Tp>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value,
      _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*> >::__type
    __copy_move_backward_a1(_II, _II,
			    _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>);

  template<bool _IsMove, typename _II, typename _OI>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _OI
    __copy_move_backward_a(_II __first, _II __last, _OI __result)
    {
      return std::__niter_wrap(__result,
		std::__copy_move_backward_a1<_IsMove>
		  (std::__niter_base(__first), std::__niter_base(__last),
		   std::__niter_base(__result)));
    }

  template<bool _IsMove,
	   typename _Ite, typename _Seq, typename _Cat, typename _OI>
    _GLIBCXX20_CONSTEXPR
    _OI
    __copy_move_backward_a(
		const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
		_OI);

  template<bool _IsMove,
	   typename _II, typename _Ite, typename _Seq, typename _Cat>
    _GLIBCXX20_CONSTEXPR
    __gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>
    __copy_move_backward_a(_II, _II,
		const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&);

  template<bool _IsMove,
	   typename _IIte, typename _ISeq, typename _ICat,
	   typename _OIte, typename _OSeq, typename _OCat>
    _GLIBCXX20_CONSTEXPR
    ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>
    __copy_move_backward_a(
		const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		const ::__gnu_debug::_Safe_iterator<_IIte, _ISeq, _ICat>&,
		const ::__gnu_debug::_Safe_iterator<_OIte, _OSeq, _OCat>&);

  /**
   *  @brief Copies the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  A bidirectional iterator.
   *  @param  __last   A bidirectional iterator.
   *  @param  __result A bidirectional iterator.
   *  @return   result - (last - first)
   *
   *  The function has the same effect as copy, but starts at the end of the
   *  range and works its way to the start, returning the start of the result.
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).
   *
   *  Result may not be in the range (first,last].  Use copy instead.  Note
   *  that the start of the output range may overlap [first,last).
  */
  template<typename _BI1, typename _BI2>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    copy_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<_BI1>)
      __glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<_BI2>)
      __glibcxx_function_requires(_OutputIteratorConcept<_BI2,
	    typename iterator_traits<_BI1>::reference>)
      __glibcxx_requires_can_decrement_range(__first, __last, __result);

      return std::__copy_move_backward_a<__is_move_iterator<_BI1>::__value>
	     (std::__miter_base(__first), std::__miter_base(__last), __result);
    }

#if __cplusplus >= 201103L
  /**
   *  @brief Moves the range [first,last) into result.
   *  @ingroup mutating_algorithms
   *  @param  __first  A bidirectional iterator.
   *  @param  __last   A bidirectional iterator.
   *  @param  __result A bidirectional iterator.
   *  @return   result - (last - first)
   *
   *  The function has the same effect as move, but starts at the end of the
   *  range and works its way to the start, returning the start of the result.
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).
   *
   *  Result may not be in the range (first,last].  Use move instead.  Note
   *  that the start of the output range may overlap [first,last).
  */
  template<typename _BI1, typename _BI2>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _BI2
    move_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<_BI1>)
      __glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<_BI2>)
      __glibcxx_function_requires(_OutputIteratorConcept<_BI2,
	    typename iterator_traits<_BI1>::value_type&&>)
      __glibcxx_requires_can_decrement_range(__first, __last, __result);

      return std::__copy_move_backward_a<true>(std::__miter_base(__first),
					       std::__miter_base(__last),
					       __result);
    }

#define _GLIBCXX_MOVE_BACKWARD3(_Tp, _Up, _Vp) std::move_backward(_Tp, _Up, _Vp)
#else
#define _GLIBCXX_MOVE_BACKWARD3(_Tp, _Up, _Vp) std::copy_backward(_Tp, _Up, _Vp)
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
  template<typename _ForwardIterator, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline void
    __fill_a1(_ForwardIterator __first, _ForwardIterator __last,
	      const _Tp& __value)
    {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wlong-long"
      // We can optimize this loop by moving the load from __value outside
      // the loop, but only if we know that making that copy is trivial,
      // and the assignment in the loop is also trivial (so that the identity
      // of the operand doesn't matter).
      const bool __load_outside_loop =
#if __has_builtin(__is_trivially_constructible) \
      && __has_builtin(__is_trivially_assignable)
	    __is_trivially_constructible(_Tp, const _Tp&)
	    && __is_trivially_assignable(__decltype(*__first), const _Tp&)
#else
	    __is_trivially_copyable(_Tp)
	    && __is_same(_Tp, __typeof__(*__first))
#endif
	    && sizeof(_Tp) <= sizeof(long long);
#pragma GCC diagnostic pop

      // When the condition is true, we use a copy of __value,
      // otherwise we just use another reference.
      typedef typename __gnu_cxx::__conditional_type<__load_outside_loop,
						     const _Tp,
						     const _Tp&>::__type _Up;
      _Up __val(__value);
      for (; __first != __last; ++__first)
	*__first = __val;
    }
#pragma GCC diagnostic pop

  // Specialization: for char types we can use memset.
  template<typename _Up, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline typename
    __gnu_cxx::__enable_if<__is_byte<_Up>::__value
			     && (__are_same<_Up, _Tp>::__value // for std::byte
				   || __memcpyable_integer<_Tp>::__width),
			   void>::__type
    __fill_a1(_Up* __first, _Up* __last, const _Tp& __x)
    {
      // This hoists the load out of the loop and also ensures that we don't
      // use memset for cases where the assignment would be ill-formed.
      const _Up __val = __x;
#if __cpp_lib_is_constant_evaluated
      if (std::is_constant_evaluated())
	{
	  for (; __first != __last; ++__first)
	    *__first = __val;
	  return;
	}
#endif
      if (const size_t __len = __last - __first)
	__builtin_memset(__first, static_cast<unsigned char>(__val), __len);
    }

  template<typename _Ite, typename _Cont, typename _Tp>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline void
    __fill_a1(::__gnu_cxx::__normal_iterator<_Ite, _Cont> __first,
	      ::__gnu_cxx::__normal_iterator<_Ite, _Cont> __last,
	      const _Tp& __value)
    { std::__fill_a1(__first.base(), __last.base(), __value); }

  template<typename _Tp, typename _VTp>
    void
    __fill_a1(const _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>&,
	      const _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Tp&, _Tp*>&,
	      const _VTp&);

  _GLIBCXX20_CONSTEXPR
  void
  __fill_a1(_GLIBCXX_STD_C::_Bit_iterator, _GLIBCXX_STD_C::_Bit_iterator,
	    const bool&);

  template<typename _FIte, typename _Tp>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline void
    __fill_a(_FIte __first, _FIte __last, const _Tp& __value)
    { std::__fill_a1(__first, __last, __value); }

  template<typename _Ite, typename _Seq, typename _Cat, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    void
    __fill_a(const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
	     const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>&,
	     const _Tp&);

  /**
   *  @brief Fills the range [first,last) with copies of value.
   *  @ingroup mutating_algorithms
   *  @param  __first  A forward iterator.
   *  @param  __last   A forward iterator.
   *  @param  __value  A reference-to-const of arbitrary type.
   *  @return   Nothing.
   *
   *  This function fills a range with copies of the same value.  For char
   *  types filling contiguous areas of memory, this becomes an inline call
   *  to @c memset or @c wmemset.
  */
  template<typename _ForwardIterator, typename _Tp>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline void
    fill(_ForwardIterator __first, _ForwardIterator __last, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator>)
      __glibcxx_requires_valid_range(__first, __last);

      std::__fill_a(__first, __last, __value);
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wlong-long"
  // Used by fill_n, generate_n, etc. to convert _Size to an integral type:
  inline _GLIBCXX_CONSTEXPR int
  __size_to_integer(int __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR unsigned
  __size_to_integer(unsigned __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR long
  __size_to_integer(long __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR unsigned long
  __size_to_integer(unsigned long __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(long long __n) { return __n; }
  inline _GLIBCXX_CONSTEXPR unsigned long long
  __size_to_integer(unsigned long long __n) { return __n; }

#if defined(__GLIBCXX_TYPE_INT_N_0)
  __extension__ inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_0
  __size_to_integer(__GLIBCXX_TYPE_INT_N_0 __n) { return __n; }
  __extension__ inline _GLIBCXX_CONSTEXPR unsigned __GLIBCXX_TYPE_INT_N_0
  __size_to_integer(unsigned __GLIBCXX_TYPE_INT_N_0 __n) { return __n; }
#endif
#if defined(__GLIBCXX_TYPE_INT_N_1)
  __extension__ inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_1
  __size_to_integer(__GLIBCXX_TYPE_INT_N_1 __n) { return __n; }
  __extension__ inline _GLIBCXX_CONSTEXPR unsigned __GLIBCXX_TYPE_INT_N_1
  __size_to_integer(unsigned __GLIBCXX_TYPE_INT_N_1 __n) { return __n; }
#endif
#if defined(__GLIBCXX_TYPE_INT_N_2)
  __extension__ inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_2
  __size_to_integer(__GLIBCXX_TYPE_INT_N_2 __n) { return __n; }
  __extension__ inline _GLIBCXX_CONSTEXPR unsigned __GLIBCXX_TYPE_INT_N_2
  __size_to_integer(unsigned __GLIBCXX_TYPE_INT_N_2 __n) { return __n; }
#endif
#if defined(__GLIBCXX_TYPE_INT_N_3)
  __extension__ inline _GLIBCXX_CONSTEXPR unsigned __GLIBCXX_TYPE_INT_N_3
  __size_to_integer(__GLIBCXX_TYPE_INT_N_3 __n) { return __n; }
  __extension__ inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_3
  __size_to_integer(unsigned __GLIBCXX_TYPE_INT_N_3 __n) { return __n; }
#endif

  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(float __n) { return (long long)__n; }
  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(double __n) { return (long long)__n; }
  inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(long double __n) { return (long long)__n; }
#if !defined(__STRICT_ANSI__) && defined(_GLIBCXX_USE_FLOAT128)
  __extension__ inline _GLIBCXX_CONSTEXPR long long
  __size_to_integer(__float128 __n) { return (long long)__n; }
#endif
#pragma GCC diagnostic pop

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
#pragma GCC diagnostic ignored "-Wlong-long"
  template<typename _OutputIterator, typename _Size, typename _Tp>
    _GLIBCXX20_CONSTEXPR
    inline _OutputIterator
    __fill_n_a1(_OutputIterator __first, _Size __n, const _Tp& __value)
    {
      // See std::__fill_a1 for explanation of this condition.
      const bool __load_outside_loop =
#if __has_builtin(__is_trivially_constructible) \
      && __has_builtin(__is_trivially_assignable)
	    __is_trivially_constructible(_Tp, const _Tp&)
	    && __is_trivially_assignable(__decltype(*__first), const _Tp&)
#else
	    __is_trivially_copyable(_Tp)
	    && __is_same(_Tp, __typeof__(*__first))
#endif
	    && sizeof(_Tp) <= sizeof(long long);

      // When the condition is true, we use a copy of __value,
      // otherwise we just use another reference.
      typedef typename __gnu_cxx::__conditional_type<__load_outside_loop,
						     const _Tp,
						     const _Tp&>::__type _Up;
      _Up __val(__value);
      for (; __n > 0; --__n, (void) ++__first)
	*__first = __val;
      return __first;
    }
#pragma GCC diagnostic pop

  template<typename _Ite, typename _Seq, typename _Cat, typename _Size,
	   typename _Tp>
    _GLIBCXX20_CONSTEXPR
    ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>
    __fill_n_a(const ::__gnu_debug::_Safe_iterator<_Ite, _Seq, _Cat>& __first,
	       _Size __n, const _Tp& __value,
	       std::input_iterator_tag);

  template<typename _OutputIterator, typename _Size, typename _Tp>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _OutputIterator
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value,
	       std::output_iterator_tag)
    {
#if __cplusplus >= 201103L
      static_assert(is_integral<_Size>{}, "fill_n must pass integral size");
#endif
      return __fill_n_a1(__first, __n, __value);
    }

  template<typename _OutputIterator, typename _Size, typename _Tp>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _OutputIterator
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value,
	       std::input_iterator_tag)
    {
#if __cplusplus >= 201103L
      static_assert(is_integral<_Size>{}, "fill_n must pass integral size");
#endif
      return __fill_n_a1(__first, __n, __value);
    }

  template<typename _OutputIterator, typename _Size, typename _Tp>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _OutputIterator
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value,
	       std::random_access_iterator_tag)
    {
#if __cplusplus >= 201103L
      static_assert(is_integral<_Size>{}, "fill_n must pass integral size");
#endif
      if (__n <= 0)
	return __first;

      __glibcxx_requires_can_increment(__first, __n);

      std::__fill_a(__first, __first + __n, __value);
      return __first + __n;
    }

  /**
   *  @brief Fills the range [first,first+n) with copies of value.
   *  @ingroup mutating_algorithms
   *  @param  __first  An output iterator.
   *  @param  __n      The count of copies to perform.
   *  @param  __value  A reference-to-const of arbitrary type.
   *  @return   The iterator at first+n.
   *
   *  This function fills a range with copies of the same value.  For char
   *  types filling contiguous areas of memory, this becomes an inline call
   *  to @c memset or @c wmemset.
   *
   *  If @p __n is negative, the function does nothing.
  */
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 865. More algorithms that throw away information
  // DR 426. search_n(), fill_n(), and generate_n() with negative n
  template<typename _OI, typename _Size, typename _Tp>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline _OI
    fill_n(_OI __first, _Size __n, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_OutputIteratorConcept<_OI, const _Tp&>)

      return std::__fill_n_a(__first, std::__size_to_integer(__n), __value,
			       std::__iterator_category(__first));
    }

  template<bool _BoolType>
    struct __equal
    {
      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static bool
	equal(_II1 __first1, _II1 __last1, _II2 __first2)
	{
	  for (; __first1 != __last1; ++__first1, (void) ++__first2)
	    if (!(*__first1 == *__first2))
	      return false;
	  return true;
	}
    };

  template<>
    struct __equal<true>
    {
      template<typename _Tp>
	_GLIBCXX20_CONSTEXPR
	static bool
	equal(const _Tp* __first1, const _Tp* __last1, const _Tp* __first2)
	{
	  if (const size_t __len = (__last1 - __first1))
	    return !std::__memcmp(__first1, __first2, __len);
	  return true;
	}
    };

  template<typename _Tp, typename _Ref, typename _Ptr, typename _II>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value, bool>::__type
    __equal_aux1(_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		 _GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>,
		 _II);

  template<typename _Tp1, typename _Ref1, typename _Ptr1,
	   typename _Tp2, typename _Ref2, typename _Ptr2>
    bool
    __equal_aux1(_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
		 _GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
		 _GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>);

  template<typename _II, typename _Tp, typename _Ref, typename _Ptr>
    typename __gnu_cxx::__enable_if<
      __is_random_access_iter<_II>::__value, bool>::__type
    __equal_aux1(_II, _II,
		_GLIBCXX_STD_C::_Deque_iterator<_Tp, _Ref, _Ptr>);

  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal_aux1(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      const bool __simple = ((__is_integer<_ValueType1>::__value
#if _GLIBCXX_USE_BUILTIN_TRAIT(__is_pointer)
				|| __is_pointer(_ValueType1)
#endif
#if __glibcxx_byte && __glibcxx_type_trait_variable_templates
				// bits/cpp_type_traits.h declares std::byte
				|| is_same_v<_ValueType1, byte>
#endif
			     ) && __memcmpable<_II1, _II2>::__value);
      return std::__equal<__simple>::equal(__first1, __last1, __first2);
    }

  template<typename _II1, typename _II2>
    __attribute__((__always_inline__))
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal_aux(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      return std::__equal_aux1(std::__niter_base(__first1),
			       std::__niter_base(__last1),
			       std::__niter_base(__first2));
    }

  template<typename _II1, typename _Seq1, typename _Cat1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    bool
    __equal_aux(const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		_II2);

  template<typename _II1, typename _II2, typename _Seq2, typename _Cat2>
    _GLIBCXX20_CONSTEXPR
    bool
    __equal_aux(_II1, _II1,
		const ::__gnu_debug::_Safe_iterator<_II2, _Seq2, _Cat2>&);

  template<typename _II1, typename _Seq1, typename _Cat1,
	   typename _II2, typename _Seq2, typename _Cat2>
    _GLIBCXX20_CONSTEXPR
    bool
    __equal_aux(const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_II1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_II2, _Seq2, _Cat2>&);

  template<typename, typename>
    struct __lc_rai
    {
      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static _II1
	__newlast1(_II1, _II1 __last1, _II2, _II2)
	{ return __last1; }

      template<typename _II>
	_GLIBCXX20_CONSTEXPR
	static bool
	__cnd2(_II __first, _II __last)
	{ return __first != __last; }
    };

  template<>
    struct __lc_rai<random_access_iterator_tag, random_access_iterator_tag>
    {
      template<typename _RAI1, typename _RAI2>
	_GLIBCXX20_CONSTEXPR
	static _RAI1
	__newlast1(_RAI1 __first1, _RAI1 __last1,
		   _RAI2 __first2, _RAI2 __last2)
	{
	  const typename iterator_traits<_RAI1>::difference_type
	    __diff1 = __last1 - __first1;
	  const typename iterator_traits<_RAI2>::difference_type
	    __diff2 = __last2 - __first2;
	  return __diff2 < __diff1 ? __first1 + __diff2 : __last1;
	}

      template<typename _RAI>
	static _GLIBCXX20_CONSTEXPR bool
	__cnd2(_RAI, _RAI)
	{ return true; }
    };

  template<typename _II1, typename _II2, typename _Compare>
    _GLIBCXX20_CONSTEXPR
    bool
    __lexicographical_compare_impl(_II1 __first1, _II1 __last1,
				   _II2 __first2, _II2 __last2,
				   _Compare __comp)
    {
      typedef typename iterator_traits<_II1>::iterator_category _Category1;
      typedef typename iterator_traits<_II2>::iterator_category _Category2;
      typedef std::__lc_rai<_Category1, _Category2> __rai_type;

      __last1 = __rai_type::__newlast1(__first1, __last1, __first2, __last2);
      for (; __first1 != __last1 && __rai_type::__cnd2(__first2, __last2);
	   ++__first1, (void)++__first2)
	{
	  if (__comp(__first1, __first2))
	    return true;
	  if (__comp(__first2, __first1))
	    return false;
	}
      return __first1 == __last1 && __first2 != __last2;
    }

  template<bool _BoolType>
    struct __lexicographical_compare
    {
      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static bool
	__lc(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
	{
	  using __gnu_cxx::__ops::__iter_less_iter;
	  return std::__lexicographical_compare_impl(__first1, __last1,
						     __first2, __last2,
						     __iter_less_iter());
	}

      template<typename _II1, typename _II2>
	_GLIBCXX20_CONSTEXPR
	static int
	__3way(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
	{
	  while (__first1 != __last1)
	    {
	      if (__first2 == __last2)
		return +1;
	      if (*__first1 < *__first2)
		return -1;
	      if (*__first2 < *__first1)
		return +1;
	      ++__first1;
	      ++__first2;
	    }
	  return int(__first2 == __last2) - 1;
	}
    };

  template<>
    struct __lexicographical_compare<true>
    {
      template<typename _Tp, typename _Up>
	_GLIBCXX20_CONSTEXPR
	static bool
	__lc(const _Tp* __first1, const _Tp* __last1,
	     const _Up* __first2, const _Up* __last2)
	{ return __3way(__first1, __last1, __first2, __last2) < 0; }

      template<typename _Tp, typename _Up>
	_GLIBCXX20_CONSTEXPR
	static ptrdiff_t
	__3way(const _Tp* __first1, const _Tp* __last1,
	       const _Up* __first2, const _Up* __last2)
	{
	  const size_t __len1 = __last1 - __first1;
	  const size_t __len2 = __last2 - __first2;
	  if (const size_t __len = std::min(__len1, __len2))
	    if (int __result = std::__memcmp(__first1, __first2, __len))
	      return __result;
	  return ptrdiff_t(__len1 - __len2);
	}
    };

  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __lexicographical_compare_aux1(_II1 __first1, _II1 __last1,
				   _II2 __first2, _II2 __last2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      typedef typename iterator_traits<_II2>::value_type _ValueType2;
#if _GLIBCXX_USE_BUILTIN_TRAIT(__is_pointer)
      const bool __simple =
	(__is_memcmp_ordered_with<_ValueType1, _ValueType2>::__value
	 && __is_pointer(_II1) && __is_pointer(_II2)
#if __cplusplus > 201703L && __glibcxx_concepts
	 // For C++20 iterator_traits<volatile T*>::value_type is non-volatile
	 // so __is_byte<T> could be true, but we can't use memcmp with
	 // volatile data.
	 && !is_volatile_v<remove_reference_t<iter_reference_t<_II1>>>
	 && !is_volatile_v<remove_reference_t<iter_reference_t<_II2>>>
#endif
	 );
#else
      const bool __simple = false;
#endif

      return std::__lexicographical_compare<__simple>::__lc(__first1, __last1,
							    __first2, __last2);
    }

  template<typename _Tp1, typename _Ref1, typename _Ptr1,
	   typename _Tp2>
    bool
    __lexicographical_compare_aux1(
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_Tp2*, _Tp2*);

  template<typename _Tp1,
	   typename _Tp2, typename _Ref2, typename _Ptr2>
    bool
    __lexicographical_compare_aux1(_Tp1*, _Tp1*,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>);

  template<typename _Tp1, typename _Ref1, typename _Ptr1,
	   typename _Tp2, typename _Ref2, typename _Ptr2>
    bool
    __lexicographical_compare_aux1(
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp1, _Ref1, _Ptr1>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>,
	_GLIBCXX_STD_C::_Deque_iterator<_Tp2, _Ref2, _Ptr2>);

  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __lexicographical_compare_aux(_II1 __first1, _II1 __last1,
				  _II2 __first2, _II2 __last2)
    {
      return std::__lexicographical_compare_aux1(std::__niter_base(__first1),
						 std::__niter_base(__last1),
						 std::__niter_base(__first2),
						 std::__niter_base(__last2));
    }

  template<typename _Iter1, typename _Seq1, typename _Cat1,
	   typename _II2>
    _GLIBCXX20_CONSTEXPR
    bool
    __lexicographical_compare_aux(
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		_II2, _II2);

  template<typename _II1,
	   typename _Iter2, typename _Seq2, typename _Cat2>
    _GLIBCXX20_CONSTEXPR
    bool
    __lexicographical_compare_aux(
		_II1, _II1,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&);

  template<typename _Iter1, typename _Seq1, typename _Cat1,
	   typename _Iter2, typename _Seq2, typename _Cat2>
    _GLIBCXX20_CONSTEXPR
    bool
    __lexicographical_compare_aux(
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_Iter1, _Seq1, _Cat1>&,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&,
		const ::__gnu_debug::_Safe_iterator<_Iter2, _Seq2, _Cat2>&);

  template<typename _ForwardIterator, typename _Tp, typename _Compare>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator
    __lower_bound(_ForwardIterator __first, _ForwardIterator __last,
		  const _Tp& __val, _Compare __comp)
    {
      typedef typename iterator_traits<_ForwardIterator>::difference_type
	_DistanceType;

      _DistanceType __len = std::distance(__first, __last);

      while (__len > 0)
	{
	  _DistanceType __half = __len >> 1;
	  _ForwardIterator __middle = __first;
	  std::advance(__middle, __half);
	  if (__comp(__middle, __val))
	    {
	      __first = __middle;
	      ++__first;
	      __len = __len - __half - 1;
	    }
	  else
	    __len = __half;
	}
      return __first;
    }

  /**
   *  @brief Finds the first position in which @a val could be inserted
   *         without changing the ordering.
   *  @param  __first   An iterator.
   *  @param  __last    Another iterator.
   *  @param  __val     The search term.
   *  @return         An iterator pointing to the first element <em>not less
   *                  than</em> @a val, or end() if every element is less than
   *                  @a val.
   *  @ingroup binary_search_algorithms
  */
  template<typename _ForwardIterator, typename _Tp>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator
    lower_bound(_ForwardIterator __first, _ForwardIterator __last,
		const _Tp& __val)
    {
      // concept requirements
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
      __glibcxx_function_requires(_LessThanOpConcept<
	    typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
      __glibcxx_requires_partitioned_lower(__first, __last, __val);

      return std::__lower_bound(__first, __last, __val,
				__gnu_cxx::__ops::__iter_less_val());
    }

  /// This is a helper function for the sort routines and for random.tcc.
  //  Precondition: __n > 0.
  template<typename _Tp>
    inline _GLIBCXX_CONSTEXPR _Tp
    __lg(_Tp __n)
    {
#if __cplusplus >= 201402L
      return std::__bit_width(make_unsigned_t<_Tp>(__n)) - 1;
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wlong-long"
      // Use +__n so it promotes to at least int.
      return (sizeof(+__n) * __CHAR_BIT__ - 1)
	       - (sizeof(+__n) == sizeof(long long)
		    ? __builtin_clzll(+__n)
		    : (sizeof(+__n) == sizeof(long)
			 ? __builtin_clzl(+__n)
			 : __builtin_clz(+__n)));
#pragma GCC diagnostic pop
#endif
    }

_GLIBCXX_BEGIN_NAMESPACE_ALGO

  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using @c == and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _II1, typename _II2>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_II1>::value_type,
	    typename iterator_traits<_II2>::value_type>)
      __glibcxx_requires_can_increment_range(__first1, __last1, __first2);

      return std::__equal_aux(__first1, __last1, __first2);
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *                  functor@endlink.
   *  @return         A boolean true or false.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_IIter1 __first1, _IIter1 __last1,
	  _IIter2 __first2, _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_IIter1>)
      __glibcxx_function_requires(_InputIteratorConcept<_IIter2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for (; __first1 != __last1; ++__first1, (void)++__first2)
	if (!bool(__binary_pred(*__first1, *__first2)))
	  return false;
      return true;
    }

#if __cplusplus >= 201103L
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr

  // 4-iterator version of std::equal<It1, It2> for use in C++11.
  template<typename _II1, typename _II2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal4(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
    {
      using _RATag = random_access_iterator_tag;
      using _Cat1 = typename iterator_traits<_II1>::iterator_category;
      using _Cat2 = typename iterator_traits<_II2>::iterator_category;
      using _RAIters = __and_<is_same<_Cat1, _RATag>, is_same<_Cat2, _RATag>>;
      if constexpr (_RAIters::value)
	{
	  if ((__last1 - __first1) != (__last2 - __first2))
	    return false;
	  return _GLIBCXX_STD_A::equal(__first1, __last1, __first2);
	}
      else
	{
	  for (; __first1 != __last1 && __first2 != __last2;
	       ++__first1, (void)++__first2)
	    if (!(*__first1 == *__first2))
	      return false;
	  return __first1 == __last1 && __first2 == __last2;
	}
    }

  // 4-iterator version of std::equal<It1, It2, BinaryPred> for use in C++11.
  template<typename _II1, typename _II2, typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    inline bool
    __equal4(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2,
	     _BinaryPredicate __binary_pred)
    {
      using _RATag = random_access_iterator_tag;
      using _Cat1 = typename iterator_traits<_II1>::iterator_category;
      using _Cat2 = typename iterator_traits<_II2>::iterator_category;
      using _RAIters = __and_<is_same<_Cat1, _RATag>, is_same<_Cat2, _RATag>>;
      if constexpr (_RAIters::value)
	{
	  if ((__last1 - __first1) != (__last2 - __first2))
	    return false;
	  return _GLIBCXX_STD_A::equal(__first1, __last1, __first2,
				       __binary_pred);
	}
      else
	{
	  for (; __first1 != __last1 && __first2 != __last2;
	       ++__first1, (void)++__first2)
	    if (!bool(__binary_pred(*__first1, *__first2)))
	      return false;
	  return __first1 == __last1 && __first2 == __last2;
	}
    }
#pragma GCC diagnostic pop
#endif // C++11

#ifdef __glibcxx_robust_nonmodifying_seq_ops // C++ >= 14
  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using @c == and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _II1, typename _II2>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_II1>::value_type,
	    typename iterator_traits<_II2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__equal4(__first1, __last1, __first2, __last2);
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *                  functor@endlink.
   *  @return         A boolean true or false.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline bool
    equal(_IIter1 __first1, _IIter1 __last1,
	  _IIter2 __first2, _IIter2 __last2, _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_IIter1>)
      __glibcxx_function_requires(_InputIteratorConcept<_IIter2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__equal4(__first1, __last1, __first2, __last2,
				      __binary_pred);
    }
#endif // __glibcxx_robust_nonmodifying_seq_ops

  /**
   *  @brief Performs @b dictionary comparison on ranges.
   *  @ingroup sorting_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @return   A boolean true or false.
   *
   *  <em>Returns true if the sequence of elements defined by the range
   *  [first1,last1) is lexicographically less than the sequence of elements
   *  defined by the range [first2,last2).  Returns false otherwise.</em>
   *  (Quoted from [25.3.8]/1.)  If the iterators are all character pointers,
   *  then this is an inline call to @c memcmp.
  */
  template<typename _II1, typename _II2>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline bool
    lexicographical_compare(_II1 __first1, _II1 __last1,
			    _II2 __first2, _II2 __last2)
    {
#ifdef _GLIBCXX_CONCEPT_CHECKS
      // concept requirements
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      typedef typename iterator_traits<_II2>::value_type _ValueType2;
#endif
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_function_requires(_LessThanOpConcept<_ValueType1, _ValueType2>)
      __glibcxx_function_requires(_LessThanOpConcept<_ValueType2, _ValueType1>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return std::__lexicographical_compare_aux(__first1, __last1,
						__first2, __last2);
    }

  /**
   *  @brief Performs @b dictionary comparison on ranges.
   *  @ingroup sorting_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @param  __comp  A @link comparison_functors comparison functor@endlink.
   *  @return   A boolean true or false.
   *
   *  The same as the four-parameter @c lexicographical_compare, but uses the
   *  comp parameter instead of @c <.
  */
  template<typename _II1, typename _II2, typename _Compare>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline bool
    lexicographical_compare(_II1 __first1, _II1 __last1,
			    _II2 __first2, _II2 __last2, _Compare __comp)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return std::__lexicographical_compare_impl
	(__first1, __last1, __first2, __last2,
	 __gnu_cxx::__ops::__iter_comp_iter(__comp));
    }

#if __cpp_lib_three_way_comparison
  // Both iterators refer to contiguous ranges of unsigned narrow characters,
  // or std::byte, or big-endian unsigned integers, suitable for comparison
  // using memcmp.
  template<typename _Iter1, typename _Iter2>
    concept __memcmp_ordered_with
      = (__is_memcmp_ordered_with<iter_value_t<_Iter1>,
				  iter_value_t<_Iter2>>::__value)
	  && contiguous_iterator<_Iter1> && contiguous_iterator<_Iter2>;

  // Return a struct with two members, initialized to the smaller of x and y
  // (or x if they compare equal) and the result of the comparison x <=> y.
  template<typename _Tp>
    constexpr auto
    __min_cmp(_Tp __x, _Tp __y)
    {
      struct _Res {
	_Tp _M_min;
	decltype(__x <=> __y) _M_cmp;
      };
      auto __c = __x <=> __y;
      if (__c > 0)
	return _Res{__y, __c};
      return _Res{__x, __c};
    }

  /**
   *  @brief Performs dictionary comparison on ranges.
   *  @ingroup sorting_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @param  __comp  A @link comparison_functors comparison functor@endlink.
   *  @return   The comparison category that `__comp(*__first1, *__first2)`
   *		returns.
  */
  template<typename _InputIter1, typename _InputIter2, typename _Comp>
    [[nodiscard]] constexpr auto
    lexicographical_compare_three_way(_InputIter1 __first1,
				      _InputIter1 __last1,
				      _InputIter2 __first2,
				      _InputIter2 __last2,
				      _Comp __comp)
    -> decltype(__comp(*__first1, *__first2))
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIter1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIter2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      using _Cat = decltype(__comp(*__first1, *__first2));
      static_assert(same_as<common_comparison_category_t<_Cat>, _Cat>);

      if (!std::__is_constant_evaluated())
	if constexpr (same_as<_Comp, __detail::_Synth3way>
		      || same_as<_Comp, compare_three_way>)
	  if constexpr (__memcmp_ordered_with<_InputIter1, _InputIter2>)
	    {
	      const auto [__len, __lencmp] = _GLIBCXX_STD_A::
		__min_cmp(__last1 - __first1, __last2 - __first2);
	      if (__len)
		{
		  const auto __blen = __len * sizeof(*__first1);
		  const auto __c
		    = __builtin_memcmp(&*__first1, &*__first2, __blen) <=> 0;
		  if (__c != 0)
		    return __c;
		}
	      return __lencmp;
	    }

      while (__first1 != __last1)
	{
	  if (__first2 == __last2)
	    return strong_ordering::greater;
	  if (auto __cmp = __comp(*__first1, *__first2); __cmp != 0)
	    return __cmp;
	  ++__first1;
	  ++__first2;
	}
      return (__first2 == __last2) <=> true; // See PR 94006
    }

  template<typename _InputIter1, typename _InputIter2>
    constexpr auto
    lexicographical_compare_three_way(_InputIter1 __first1,
				      _InputIter1 __last1,
				      _InputIter2 __first2,
				      _InputIter2 __last2)
    {
      return _GLIBCXX_STD_A::
	lexicographical_compare_three_way(__first1, __last1, __first2, __last2,
					  compare_three_way{});
    }
#endif // three_way_comparison

  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    pair<_InputIterator1, _InputIterator2>
    __mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	       _InputIterator2 __first2, _BinaryPredicate __binary_pred)
    {
      while (__first1 != __last1 && __binary_pred(__first1, __first2))
	{
	  ++__first1;
	  ++__first2;
	}
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using @c == and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_InputIterator1>::value_type,
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2,
			     __gnu_cxx::__ops::__iter_equal_to_iter());
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *         functor@endlink.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2, _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2,
	__gnu_cxx::__ops::__iter_comp_iter(__binary_pred));
    }

#if __glibcxx_robust_nonmodifying_seq_ops // C++ >= 14
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    pair<_InputIterator1, _InputIterator2>
    __mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	       _InputIterator2 __first2, _InputIterator2 __last2,
	       _BinaryPredicate __binary_pred)
    {
      while (__first1 != __last1 && __first2 != __last2
	     && __binary_pred(__first1, __first2))
	{
	  ++__first1;
	  ++__first2;
	}
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using @c == and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2, _InputIterator2 __last2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_InputIterator1>::value_type,
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2, __last2,
			     __gnu_cxx::__ops::__iter_equal_to_iter());
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  An input iterator.
   *  @param  __last1   An input iterator.
   *  @param  __first2  An input iterator.
   *  @param  __last2   An input iterator.
   *  @param __binary_pred A binary predicate @link functors
   *         functor@endlink.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX_NODISCARD _GLIBCXX20_CONSTEXPR
    inline pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2, _InputIterator2 __last2,
	     _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return _GLIBCXX_STD_A::__mismatch(__first1, __last1, __first2, __last2,
			     __gnu_cxx::__ops::__iter_comp_iter(__binary_pred));
    }
#endif

_GLIBCXX_END_NAMESPACE_ALGO

  // Implementation of std::find_if, also used in std::remove_if and others.
  template<typename _Iterator, typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    inline _Iterator
    __find_if(_Iterator __first, _Iterator __last, _Predicate __pred)
    {
#pragma GCC unroll 4
      while (__first != __last && !__pred(__first))
	++__first;
      return __first;
    }

  template<typename _InputIterator, typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    typename iterator_traits<_InputIterator>::difference_type
    __count_if(_InputIterator __first, _InputIterator __last, _Predicate __pred)
    {
      typename iterator_traits<_InputIterator>::difference_type __n = 0;
      for (; __first != __last; ++__first)
	if (__pred(__first))
	  ++__n;
      return __n;
    }

  template<typename _ForwardIterator, typename _Predicate>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator
    __remove_if(_ForwardIterator __first, _ForwardIterator __last,
		_Predicate __pred)
    {
      __first = std::__find_if(__first, __last, __pred);
      if (__first == __last)
	return __first;
      _ForwardIterator __result = __first;
      ++__first;
      for (; __first != __last; ++__first)
	if (!__pred(__first))
	  {
	    *__result = _GLIBCXX_MOVE(*__first);
	    ++__result;
	  }
      return __result;
    }

  template<typename _ForwardIterator1, typename _ForwardIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    _ForwardIterator1
    __search(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
	     _ForwardIterator2 __first2, _ForwardIterator2 __last2,
	     _BinaryPredicate  __predicate)
    {
      // Test for empty ranges
      if (__first1 == __last1 || __first2 == __last2)
	return __first1;

      // Test for a pattern of length 1.
      _ForwardIterator2 __p1(__first2);
      if (++__p1 == __last2)
	return std::__find_if(__first1, __last1,
		__gnu_cxx::__ops::__iter_comp_iter(__predicate, __first2));

      // General case.
      _ForwardIterator1 __current = __first1;

      for (;;)
	{
	  __first1 =
	    std::__find_if(__first1, __last1,
		__gnu_cxx::__ops::__iter_comp_iter(__predicate, __first2));

	  if (__first1 == __last1)
	    return __last1;

	  _ForwardIterator2 __p = __p1;
	  __current = __first1;
	  if (++__current == __last1)
	    return __last1;

	  while (__predicate(__current, __p))
	    {
	      if (++__p == __last2)
		return __first1;
	      if (++__current == __last1)
		return __last1;
	    }
	  ++__first1;
	}
      return __first1;
    }

#if __cplusplus >= 201103L
  template<typename _ForwardIterator1, typename _ForwardIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    bool
    __is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
		     _ForwardIterator2 __first2, _BinaryPredicate __pred)
    {
      // Efficiently compare identical prefixes:  O(N) if sequences
      // have the same elements in the same order.
      for (; __first1 != __last1; ++__first1, (void)++__first2)
	if (!__pred(__first1, __first2))
	  break;

      if (__first1 == __last1)
	return true;

      // Establish __last2 assuming equal ranges by iterating over the
      // rest of the list.
      _ForwardIterator2 __last2 = __first2;
      std::advance(__last2, std::distance(__first1, __last1));
      for (_ForwardIterator1 __scan = __first1; __scan != __last1; ++__scan)
	{
	  if (__scan != std::__find_if(__first1, __scan,
			  __gnu_cxx::__ops::__iter_comp_iter(__pred, __scan)))
	    continue; // We've seen this one before.

	  auto __matches
	    = std::__count_if(__first2, __last2,
			__gnu_cxx::__ops::__iter_comp_iter(__pred, __scan));
	  if (0 == __matches ||
	      std::__count_if(__scan, __last1,
			__gnu_cxx::__ops::__iter_comp_iter(__pred, __scan))
	      != __matches)
	    return false;
	}
      return true;
    }

  /**
   *  @brief  Checks whether a permutation of the second sequence is equal
   *          to the first sequence.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1  Start of first range.
   *  @param  __last1   End of first range.
   *  @param  __first2  Start of second range.
   *  @return true if there exists a permutation of the elements in the range
   *          [__first2, __first2 + (__last1 - __first1)), beginning with
   *          ForwardIterator2 begin, such that equal(__first1, __last1, begin)
   *          returns true; otherwise, returns false.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _GLIBCXX20_CONSTEXPR
    inline bool
    is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
		   _ForwardIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
      __glibcxx_function_requires(_EqualOpConcept<
		typename iterator_traits<_ForwardIterator1>::value_type,
		typename iterator_traits<_ForwardIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      return std::__is_permutation(__first1, __last1, __first2,
				   __gnu_cxx::__ops::__iter_equal_to_iter());
    }
#endif // C++11

_GLIBCXX_BEGIN_NAMESPACE_ALGO

  /**
   *  @brief Search a sequence for a matching sub-sequence using a predicate.
   *  @ingroup non_mutating_algorithms
   *  @param  __first1     A forward iterator.
   *  @param  __last1      A forward iterator.
   *  @param  __first2     A forward iterator.
   *  @param  __last2      A forward iterator.
   *  @param  __predicate  A binary predicate.
   *  @return   The first iterator @c i in the range
   *  @p [__first1,__last1-(__last2-__first2)) such that
   *  @p __predicate(*(i+N),*(__first2+N)) is true for each @c N in the range
   *  @p [0,__last2-__first2), or @p __last1 if no such iterator exists.
   *
   *  Searches the range @p [__first1,__last1) for a sub-sequence that
   *  compares equal value-by-value with the sequence given by @p
   *  [__first2,__last2), using @p __predicate to determine equality,
   *  and returns an iterator to the first element of the
   *  sub-sequence, or @p __last1 if no such iterator exists.
   *
   *  @see search(_ForwardIter1, _ForwardIter1, _ForwardIter2, _ForwardIter2)
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2,
	   typename _BinaryPredicate>
    _GLIBCXX20_CONSTEXPR
    inline _ForwardIterator1
    search(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
	   _ForwardIterator2 __first2, _ForwardIterator2 __last2,
	   _BinaryPredicate  __predicate)
    {
      // concept requirements
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
      __glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
      __glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
	    typename iterator_traits<_ForwardIterator1>::value_type,
	    typename iterator_traits<_ForwardIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      return std::__search(__first1, __last1, __first2, __last2,
			   __gnu_cxx::__ops::__iter_comp_iter(__predicate));
    }

_GLIBCXX_END_NAMESPACE_ALGO
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

// NB: This file is included within many other C++ includes, as a way
// of getting the base algorithms. So, make sure that parallel bits
// come in too if requested.
#ifdef _GLIBCXX_PARALLEL
# include <parallel/algobase.h>
#endif

#endif
