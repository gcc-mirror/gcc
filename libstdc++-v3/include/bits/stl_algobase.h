// Bits and pieces used in algorithms -*- C++ -*-

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
// Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
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

/** @file stl_algobase.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _STL_ALGOBASE_H
#define _STL_ALGOBASE_H 1

#include <bits/c++config.h>
#include <cstddef>
#include <bits/functexcept.h>
#include <bits/stl_pair.h>
#include <bits/cpp_type_traits.h>
#include <ext/type_traits.h>
#include <ext/numeric_traits.h>
#include <bits/stl_iterator_base_types.h>
#include <bits/stl_iterator_base_funcs.h>
#include <bits/stl_iterator.h>
#include <bits/concept_check.h>
#include <debug/debug.h>

_GLIBCXX_BEGIN_NAMESPACE(std)

  /**
   *  @brief Swaps two values.
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @return   Nothing.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  any type which has a copy constructor and an assignment operator.
  */
  template<typename _Tp>
    inline void
    swap(_Tp& __a, _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_SGIAssignableConcept<_Tp>)

      _Tp __tmp = __a;
      __a = __b;
      __b = __tmp;
    }

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

  /**
   *  @brief Swaps the contents of two iterators.
   *  @param  a  An iterator.
   *  @param  b  Another iterator.
   *  @return   Nothing.
   *
   *  This function swaps the values pointed to by two iterators, not the
   *  iterators themselves.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    inline void
    iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
    {
      typedef typename iterator_traits<_ForwardIterator1>::value_type
	_ValueType1;
      typedef typename iterator_traits<_ForwardIterator2>::value_type
	_ValueType2;

      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator1>)
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator2>)
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
    }

  /**
   *  @brief Swap the elements of two sequences.
   *  @param  first1  A forward iterator.
   *  @param  last1   A forward iterator.
   *  @param  first2  A forward iterator.
   *  @return   An iterator equal to @p first2+(last1-first1).
   *
   *  Swaps each element in the range @p [first1,last1) with the
   *  corresponding element in the range @p [first2,(last1-first1)).
   *  The ranges must not overlap.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _ForwardIterator2
    swap_ranges(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
		_ForwardIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator1>)
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator2>)
      __glibcxx_function_requires(_ConvertibleConcept<
	    typename iterator_traits<_ForwardIterator1>::value_type,
	    typename iterator_traits<_ForwardIterator2>::value_type>)
      __glibcxx_function_requires(_ConvertibleConcept<
	    typename iterator_traits<_ForwardIterator2>::value_type,
	    typename iterator_traits<_ForwardIterator1>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for (; __first1 != __last1; ++__first1, ++__first2)
	std::iter_swap(__first1, __first2);
      return __first2;
    }

  /**
   *  @brief This does what you think it does.
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @return   The lesser of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
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
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @return   The greater of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
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
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @param  comp  A @link s20_3_3_comparisons comparison functor@endlink.
   *  @return   The lesser of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
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
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @param  comp  A @link s20_3_3_comparisons comparison functor@endlink.
   *  @return   The greater of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__a, __b) ? __b : __a;
      if (__comp(__a, __b))
	return __b;
      return __a;
    }


  // If _Iterator is a __normal_iterator return its base (a plain pointer,
  // normally) otherwise return it untouched.  See copy, fill, ... 
  template<typename _Iterator,
	   bool _BoolType = __is_normal_iterator<_Iterator>::__value>
    struct __niter_base
    {
      static const _Iterator&
      __b(const _Iterator& __it)
      { return __it; }
    };

  template<typename _Iterator>
    struct __niter_base<_Iterator, true>
    {
      static const typename _Iterator::_Iterator_type&
      __b(const _Iterator& __it)
      { return __it.base(); }
    };

  // All of these auxiliary structs serve two purposes.  (1) Replace
  // calls to copy with memmove whenever possible.  (Memmove, not memcpy,
  // because the input and output ranges are permitted to overlap.)
  // (2) If we're using random access iterators, then write the loop as
  // a for loop with an explicit count.

  template<bool, typename>
    struct __copy
    {
      template<typename _II, typename _OI>
        static _OI
        copy(_II __first, _II __last, _OI __result)
        {
	  for (; __first != __last; ++__result, ++__first)
	    *__result = *__first;
	  return __result;
	}
    };

  template<bool _BoolType>
    struct __copy<_BoolType, random_access_iterator_tag>
    {
      template<typename _II, typename _OI>
        static _OI
        copy(_II __first, _II __last, _OI __result)
        { 
	  typedef typename iterator_traits<_II>::difference_type _Distance;
	  for(_Distance __n = __last - __first; __n > 0; --__n)
	    {
	      *__result = *__first;
	      ++__first;
	      ++__result;
	    }
	  return __result;
	}
    };

  template<>
    struct __copy<true, random_access_iterator_tag>
    {
      template<typename _Tp>
        static _Tp*
        copy(const _Tp* __first, const _Tp* __last, _Tp* __result)
        { 
	  __builtin_memmove(__result, __first,
			    sizeof(_Tp) * (__last - __first));
	  return __result + (__last - __first);
	}
    };

  template<typename _II, typename _OI>
    inline _OI
    __copy_aux(_II __first, _II __last, _OI __result)
    {
      typedef typename iterator_traits<_II>::value_type _ValueTypeI;
      typedef typename iterator_traits<_OI>::value_type _ValueTypeO;
      typedef typename iterator_traits<_II>::iterator_category _Category;
      const bool __simple = (__is_pod(_ValueTypeI)
	                     && __is_pointer<_II>::__value
	                     && __is_pointer<_OI>::__value
			     && __are_same<_ValueTypeI, _ValueTypeO>::__value);

      return std::__copy<__simple, _Category>::copy(__first, __last, __result);
    }

  // Helpers for streambuf iterators (either istream or ostream).
  // NB: avoid including <iosfwd>, relatively large.
  template<typename _CharT>
    struct char_traits;

  template<typename _CharT, typename _Traits>
    class istreambuf_iterator;

  template<typename _CharT, typename _Traits>
    class ostreambuf_iterator;

  template<typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value, 
	     ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_aux(_CharT*, _CharT*,
	       ostreambuf_iterator<_CharT, char_traits<_CharT> >);

  template<typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value, 
	     ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_aux(const _CharT*, const _CharT*,
	       ostreambuf_iterator<_CharT, char_traits<_CharT> >);

  template<typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
				    _CharT*>::__type
    __copy_aux(istreambuf_iterator<_CharT, char_traits<_CharT> >,
	       istreambuf_iterator<_CharT, char_traits<_CharT> >, _CharT*);

  /**
   *  @brief Copies the range [first,last) into result.
   *  @param  first  An input iterator.
   *  @param  last   An input iterator.
   *  @param  result An output iterator.
   *  @return   result + (first - last)
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
    inline _OI
    copy(_II __first, _II __last, _OI __result)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II>)
      __glibcxx_function_requires(_OutputIteratorConcept<_OI,
	    typename iterator_traits<_II>::value_type>)
      __glibcxx_requires_valid_range(__first, __last);

      return _OI(std::__copy_aux(__niter_base<_II>::__b(__first),
				 __niter_base<_II>::__b(__last),
				 __niter_base<_OI>::__b(__result)));
    }


  template<bool, typename>
    struct __copy_backward
    {
      template<typename _BI1, typename _BI2>
        static _BI2
        __copy_b(_BI1 __first, _BI1 __last, _BI2 __result)
        { 
	  while (__first != __last)
	    *--__result = *--__last;
	  return __result;
	}
    };

  template<bool _BoolType>
    struct __copy_backward<_BoolType, random_access_iterator_tag>
    {
      template<typename _BI1, typename _BI2>
        static _BI2
        __copy_b(_BI1 __first, _BI1 __last, _BI2 __result)
        { 
	  typename iterator_traits<_BI1>::difference_type __n;
	  for (__n = __last - __first; __n > 0; --__n)
	    *--__result = *--__last;
	  return __result;
	}
    };

  template<>
    struct __copy_backward<true, random_access_iterator_tag>
    {
      template<typename _Tp>
        static _Tp*
        __copy_b(const _Tp* __first, const _Tp* __last, _Tp* __result)
        { 
	  const ptrdiff_t _Num = __last - __first;
	  __builtin_memmove(__result - _Num, __first, sizeof(_Tp) * _Num);
	  return __result - _Num;
	}
    };

  template<typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_aux(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      typedef typename iterator_traits<_BI1>::value_type _ValueType1;
      typedef typename iterator_traits<_BI2>::value_type _ValueType2;
      typedef typename iterator_traits<_BI1>::iterator_category _Category;
      const bool __simple = (__is_pod(_ValueType1)
	                     && __is_pointer<_BI1>::__value
	                     && __is_pointer<_BI2>::__value
			     && __are_same<_ValueType1, _ValueType2>::__value);

      return std::__copy_backward<__simple, _Category>::__copy_b(__first,
								 __last,
								 __result);
    }

  /**
   *  @brief Copies the range [first,last) into result.
   *  @param  first  A bidirectional iterator.
   *  @param  last   A bidirectional iterator.
   *  @param  result A bidirectional iterator.
   *  @return   result - (first - last)
   *
   *  The function has the same effect as copy, but starts at the end of the
   *  range and works its way to the start, returning the start of the result.
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).
   *
   *  Result may not be in the range [first,last).  Use copy instead.  Note
   *  that the start of the output range may overlap [first,last).
  */
  template <typename _BI1, typename _BI2>
    inline _BI2
    copy_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<_BI1>)
      __glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<_BI2>)
      __glibcxx_function_requires(_ConvertibleConcept<
	    typename iterator_traits<_BI1>::value_type,
	    typename iterator_traits<_BI2>::value_type>)
      __glibcxx_requires_valid_range(__first, __last);

      return _BI2(std::__copy_backward_aux(__niter_base<_BI1>::__b(__first),
					   __niter_base<_BI1>::__b(__last),
					   __niter_base<_BI2>::__b(__result)));
    }


  template<bool>
    struct __fill
    {
      template<typename _ForwardIterator, typename _Tp>
        static void
        fill(_ForwardIterator __first, _ForwardIterator __last,
	     const _Tp& __value)
        {
	  for (; __first != __last; ++__first)
	    *__first = __value;
	}
    };

  template<>
    struct __fill<true>
    {
      template<typename _ForwardIterator, typename _Tp>
        static void
        fill(_ForwardIterator __first, _ForwardIterator __last,
	     const _Tp& __value)
        {
	  const _Tp __tmp = __value;
	  for (; __first != __last; ++__first)
	    *__first = __tmp;
	}
    };

  template<typename _ForwardIterator, typename _Tp>
    inline void
    __fill_aux(_ForwardIterator __first, _ForwardIterator __last,
	       const _Tp& __value)
    {
      const bool __scalar = __is_scalar<_Tp>::__value;
      std::__fill<__scalar>::fill(__first, __last, __value);
    }

  // Specialization: for char types we can use memset.
  inline void
  __fill_aux(unsigned char* __first, unsigned char* __last, unsigned char __c)
  { __builtin_memset(__first, __c, __last - __first); }

  inline void
  __fill_aux(signed char* __first, signed char* __last, signed char __c)
  { __builtin_memset(__first, static_cast<unsigned char>(__c),
		     __last - __first); }

  inline void
  __fill_aux(char* __first, char* __last, char __c)
  { __builtin_memset(__first, static_cast<unsigned char>(__c),
		     __last - __first); }

  /**
   *  @brief Fills the range [first,last) with copies of value.
   *  @param  first  A forward iterator.
   *  @param  last   A forward iterator.
   *  @param  value  A reference-to-const of arbitrary type.
   *  @return   Nothing.
   *
   *  This function fills a range with copies of the same value.  For char
   *  types filling contiguous areas of memory, this becomes an inline call
   *  to @c memset or @c wmemset.
  */
  template<typename _ForwardIterator, typename _Tp>
    inline void
    fill(_ForwardIterator __first, _ForwardIterator __last, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator>)
      __glibcxx_requires_valid_range(__first, __last);

      std::__fill_aux(__niter_base<_ForwardIterator>::__b(__first),
		      __niter_base<_ForwardIterator>::__b(__last), __value);
    }


  template<bool>
    struct __fill_n
    {
      template<typename _OutputIterator, typename _Size, typename _Tp>
        static _OutputIterator
        fill_n(_OutputIterator __first, _Size __n, const _Tp& __value)
        {
	  for (; __n > 0; --__n, ++__first)
	    *__first = __value;
	  return __first;
	}
    };

  template<>
    struct __fill_n<true>
    {
      template<typename _OutputIterator, typename _Size, typename _Tp>
        static _OutputIterator
        fill_n(_OutputIterator __first, _Size __n, const _Tp& __value)
        {
	  const _Tp __tmp = __value;
	  for (; __n > 0; --__n, ++__first)
	    *__first = __tmp;
	  return __first;	  
	}
    };

  template<typename _OutputIterator, typename _Size, typename _Tp>
    inline _OutputIterator
    __fill_n_aux(_OutputIterator __first, _Size __n, const _Tp& __value)
    {
      const bool __scalar = __is_scalar<_Tp>::__value;
      return std::__fill_n<__scalar>::fill_n(__first, __n, __value);
    }

  template<typename _Size>
    inline unsigned char*
    __fill_n_aux(unsigned char* __first, _Size __n, unsigned char __c)
    {
      std::__fill_aux(__first, __first + __n, __c);
      return __first + __n;
    }

  template<typename _Size>
    inline signed char*
    __fill_n_aux(signed char* __first, _Size __n, signed char __c)
    {
      std::__fill_aux(__first, __first + __n, __c);
      return __first + __n;
    }

  template<typename _Size>
    inline char*
    __fill_n_aux(char* __first, _Size __n, char __c)
    {
      std::__fill_aux(__first, __first + __n, __c);
      return __first + __n;
    }

  /**
   *  @brief Fills the range [first,first+n) with copies of value.
   *  @param  first  An output iterator.
   *  @param  n      The count of copies to perform.
   *  @param  value  A reference-to-const of arbitrary type.
   *  @return   The iterator at first+n.
   *
   *  This function fills a range with copies of the same value.  For char
   *  types filling contiguous areas of memory, this becomes an inline call
   *  to @c memset or @ wmemset.
  */
  template<typename _OI, typename _Size, typename _Tp>
    inline _OI
    fill_n(_OI __first, _Size __n, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_OutputIteratorConcept<_OI, _Tp>)

      return _OI(std::__fill_n_aux(__niter_base<_OI>::__b(__first), __n,
				   __value));
    }


  template<bool _BoolType>
    struct __equal
    {
      template<typename _II1, typename _II2>
        static bool
        equal(_II1 __first1, _II1 __last1, _II2 __first2)
        {
	  for (; __first1 != __last1; ++__first1, ++__first2)
	    if (!(*__first1 == *__first2))
	      return false;
	  return true;
	}
    };

  template<>
    struct __equal<true>
    {
      template<typename _Tp>
        static bool
        equal(const _Tp* __first1, const _Tp* __last1, const _Tp* __first2)
        {
	  return !__builtin_memcmp(__first1, __first2, sizeof(_Tp)
				   * (__last1 - __first1));
	}
    };

  template<typename _II1, typename _II2>
    inline bool
    __equal_aux(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      typedef typename iterator_traits<_II2>::value_type _ValueType2;
      const bool __simple = (__is_integer<_ValueType1>::__value
	                     && __is_pointer<_II1>::__value
	                     && __is_pointer<_II2>::__value
			     && __are_same<_ValueType1, _ValueType2>::__value);

      return std::__equal<__simple>::equal(__first1, __last1, __first2);
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using @c == and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _II1, typename _II2>
    inline bool
    equal(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_II1>)
      __glibcxx_function_requires(_InputIteratorConcept<_II2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_II1>::value_type,
	    typename iterator_traits<_II2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      return std::__equal_aux(__niter_base<_II1>::__b(__first1),
			      __niter_base<_II1>::__b(__last1),
			      __niter_base<_II2>::__b(__first2));
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @param  binary_pred  A binary predicate @link s20_3_1_base functor@endlink.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    inline bool
    equal(_InputIterator1 __first1, _InputIterator1 __last1,
	  _InputIterator2 __first2,
	  _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for (; __first1 != __last1; ++__first1, ++__first2)
	if (!bool(__binary_pred(*__first1, *__first2)))
	  return false;
      return true;
    }

  /**
   *  @brief Performs "dictionary" comparison on ranges.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @param  last2   An input iterator.
   *  @return   A boolean true or false.
   *
   *  "Returns true if the sequence of elements defined by the range
   *  [first1,last1) is lexicographically less than the sequence of elements
   *  defined by the range [first2,last2).  Returns false otherwise."
   *  (Quoted from [25.3.8]/1.)  If the iterators are all character pointers,
   *  then this is an inline call to @c memcmp.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    bool
    lexicographical_compare(_InputIterator1 __first1, _InputIterator1 __last1,
			    _InputIterator2 __first2, _InputIterator2 __last2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_LessThanOpConcept<
	    typename iterator_traits<_InputIterator1>::value_type,
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_function_requires(_LessThanOpConcept<
	    typename iterator_traits<_InputIterator2>::value_type,
	    typename iterator_traits<_InputIterator1>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      for (; __first1 != __last1 && __first2 != __last2;
	   ++__first1, ++__first2)
	{
	  if (*__first1 < *__first2)
	    return true;
	  if (*__first2 < *__first1)
	    return false;
	}
      return __first1 == __last1 && __first2 != __last2;
    }

  /**
   *  @brief Performs "dictionary" comparison on ranges.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @param  last2   An input iterator.
   *  @param  comp  A @link s20_3_3_comparisons comparison functor@endlink.
   *  @return   A boolean true or false.
   *
   *  The same as the four-parameter @c lexigraphical_compare, but uses the
   *  comp parameter instead of @c <.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _Compare>
    bool
    lexicographical_compare(_InputIterator1 __first1, _InputIterator1 __last1,
			    _InputIterator2 __first2, _InputIterator2 __last2,
			    _Compare __comp)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      for (; __first1 != __last1 && __first2 != __last2;
	   ++__first1, ++__first2)
	{
	  if (__comp(*__first1, *__first2))
	    return true;
	  if (__comp(*__first2, *__first1))
	    return false;
	}
      return __first1 == __last1 && __first2 != __last2;
    }

  inline bool
  lexicographical_compare(const unsigned char* __first1,
			  const unsigned char* __last1,
			  const unsigned char* __first2,
			  const unsigned char* __last2)
  {
    __glibcxx_requires_valid_range(__first1, __last1);
    __glibcxx_requires_valid_range(__first2, __last2);

    const size_t __len1 = __last1 - __first1;
    const size_t __len2 = __last2 - __first2;
    const int __result = __builtin_memcmp(__first1, __first2,
					  std::min(__len1, __len2));
    return __result != 0 ? __result < 0 : __len1 < __len2;
  }

  inline bool
  lexicographical_compare(const char* __first1, const char* __last1,
			  const char* __first2, const char* __last2)
  {
    __glibcxx_requires_valid_range(__first1, __last1);
    __glibcxx_requires_valid_range(__first2, __last2);

    if (__gnu_cxx::__numeric_traits<char>::__is_signed)
      return std::lexicographical_compare((const signed char*) __first1,
					  (const signed char*) __last1,
					  (const signed char*) __first2,
					  (const signed char*) __last2);
    else
      return std::lexicographical_compare((const unsigned char*) __first1,
					  (const unsigned char*) __last1,
					  (const unsigned char*) __first2,
					  (const unsigned char*) __last2);
  }

_GLIBCXX_END_NAMESPACE

#endif
