// Bits and pieces used in algorithms -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
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

/* NOTE: This is an internal header file, included by other STL headers.
 *   You should not attempt to use it directly.
 */


#ifndef __SGI_STL_INTERNAL_ALGOBASE_H
#define __SGI_STL_INTERNAL_ALGOBASE_H

#include <bits/c++config.h>
#include <bits/stl_pair.h>
#include <bits/type_traits.h>
#include <bits/std_cstring.h>
#include <bits/std_climits.h>
#include <bits/std_cstdlib.h>
#include <bits/std_cstddef.h>
#include <new>

#include <bits/std_iosfwd.h>
#include <bits/stl_iterator_base_types.h>
#include <bits/stl_iterator_base_funcs.h>
#include <bits/stl_iterator.h>
#include <bits/concept_check.h>

namespace std
{

  // swap and iter_swap

  template<typename _ForwardIter1, typename _ForwardIter2>
    inline void
    iter_swap(_ForwardIter1 __a, _ForwardIter2 __b)
    {
      typedef typename iterator_traits<_ForwardIter1>::value_type _ValueType1;
      typedef typename iterator_traits<_ForwardIter2>::value_type _ValueType2;

      // concept requirements
      __glibcpp_function_requires(_Mutable_ForwardIteratorConcept<_ForwardIter1>);
      __glibcpp_function_requires(_Mutable_ForwardIteratorConcept<_ForwardIter2>);
      __glibcpp_function_requires(_ConvertibleConcept<_ValueType1, _ValueType2>);
      __glibcpp_function_requires(_ConvertibleConcept<_ValueType2, _ValueType1>);

      _ValueType1 __tmp = *__a;
      *__a = *__b;
      *__b = __tmp;
    }

  template<typename _Tp>
    inline void
    swap(_Tp& __a, _Tp& __b)
    {
      // concept requirements
      __glibcpp_function_requires(_SGIAssignableConcept<_Tp>);
      
      _Tp __tmp = __a;
      __a = __b;
      __b = __tmp;
    }

  //--------------------------------------------------
  // min and max

  #undef min
  #undef max

  template<typename _Tp>
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcpp_function_requires(_LessThanComparableConcept<_Tp>);
      //return __b < __a ? __b : __a;
      if (__b < __a) return __b; return __a;
    }

  template<typename _Tp>
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b) 
    {
      // concept requirements
      __glibcpp_function_requires(_LessThanComparableConcept<_Tp>);
      //return  __a < __b ? __b : __a;
      if (__a < __b) return __b; return __a;
    }

  template<typename _Tp, typename _Compare>
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__b, __a) ? __b : __a;
      if (__comp(__b, __a)) return __b; return __a;
    }

  template<typename _Tp, typename _Compare>
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__a, __b) ? __b : __a;
      if (__comp(__a, __b)) return __b; return __a;
    }

  //--------------------------------------------------
  // copy

  // All of these auxiliary functions serve two purposes.  (1) Replace
  // calls to copy with memmove whenever possible.  (Memmove, not memcpy,
  // because the input and output ranges are permitted to overlap.)
  // (2) If we're using random access iterators, then write the loop as
  // a for loop with an explicit count.

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    __copy(_InputIter __first, _InputIter __last,
	   _OutputIter __result,
	   input_iterator_tag)
    {
      for ( ; __first != __last; ++__result, ++__first)
	*__result = *__first;
      return __result;
    }

  template<typename _RandomAccessIter, typename _OutputIter>
    inline _OutputIter
    __copy(_RandomAccessIter __first, _RandomAccessIter __last,
	   _OutputIter __result,
	   random_access_iterator_tag)
    {
      typedef typename iterator_traits<_RandomAccessIter>::difference_type
          _Distance;
      for (_Distance __n = __last - __first; __n > 0; --__n) {
	*__result = *__first;
	++__first;
	++__result;
      }
      return __result;
    }

  template<typename _Tp>
    inline _Tp*
    __copy_trivial(const _Tp* __first, const _Tp* __last, _Tp* __result)
    {
      memmove(__result, __first, sizeof(_Tp) * (__last - __first));
      return __result + (__last - __first);
    }

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    __copy_aux2(_InputIter __first, _InputIter __last,
		_OutputIter __result, __false_type)
    { return __copy(__first, __last, __result, __iterator_category(__first)); }

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    __copy_aux2(_InputIter __first, _InputIter __last,
		_OutputIter __result, __true_type)
    { return __copy(__first, __last, __result, __iterator_category(__first)); }

  template<typename _Tp>
    inline _Tp*
    __copy_aux2(_Tp* __first, _Tp* __last,
		_Tp* __result, __true_type)
    { return __copy_trivial(__first, __last, __result); }

  template<typename _Tp>
    inline _Tp*
    __copy_aux2(const _Tp* __first, const _Tp* __last,
		_Tp* __result, __true_type)
    { return __copy_trivial(__first, __last, __result); }

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    __copy_ni2(_InputIter __first, _InputIter __last,
	       _OutputIter __result, __true_type)
    {
      typedef typename iterator_traits<_InputIter>::value_type
	  _ValueType;
      typedef typename __type_traits<_ValueType>::has_trivial_assignment_operator
	  _Trivial;
      return _OutputIter(__copy_aux2(__first, __last,
                                     __result.base(),
				     _Trivial()));
    }

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    __copy_ni2(_InputIter __first, _InputIter __last,
	       _OutputIter __result, __false_type)
    {
      typedef typename iterator_traits<_InputIter>::value_type
          _ValueType;
      typedef typename __type_traits<_ValueType>::has_trivial_assignment_operator
          _Trivial;
      return __copy_aux2(__first, __last,
                         __result,
			 _Trivial());
    }

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    __copy_ni1(_InputIter __first, _InputIter __last,
	       _OutputIter __result, __true_type)
    {
      typedef typename _Is_normal_iterator<_OutputIter>::_Normal __Normal;
      return __copy_ni2(__first.base(), __last.base(), __result, __Normal());
    }

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    __copy_ni1(_InputIter __first, _InputIter __last,
	       _OutputIter __result, __false_type)
    {
      typedef typename _Is_normal_iterator<_OutputIter>::_Normal __Normal;
      return __copy_ni2(__first, __last, __result, __Normal());
    }

  template<typename _InputIter, typename _OutputIter>
    inline _OutputIter
    copy(_InputIter __first, _InputIter __last, _OutputIter __result)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter>);
      __glibcpp_function_requires(_OutputIteratorConcept<_OutputIter,
	    typename iterator_traits<_InputIter>::value_type>);

       typedef typename _Is_normal_iterator<_InputIter>::_Normal __Normal;
       return __copy_ni1(__first, __last, __result, __Normal());
    }

  //--------------------------------------------------
  // copy_backward

  template<typename _BidirectionalIter1, typename _BidirectionalIter2>
    inline _BidirectionalIter2
    __copy_backward(_BidirectionalIter1 __first, _BidirectionalIter1 __last, 
		    _BidirectionalIter2 __result,
		    bidirectional_iterator_tag)
    {
      while (__first != __last)
        *--__result = *--__last;
      return __result;
    }

  template<typename _RandomAccessIter, typename _BidirectionalIter>
    inline _BidirectionalIter
    __copy_backward(_RandomAccessIter __first, _RandomAccessIter __last, 
		    _BidirectionalIter __result,
		    random_access_iterator_tag)
    {
      typename iterator_traits<_RandomAccessIter>::difference_type __n;
      for (__n = __last - __first; __n > 0; --__n)
        *--__result = *--__last;
      return __result;
    }


  // This dispatch class is a workaround for compilers that do not 
  // have partial ordering of function templates.  All we're doing is
  // creating a specialization so that we can turn a call to copy_backward
  // into a memmove whenever possible.

  template<typename _BidirectionalIter1, typename _BidirectionalIter2,
           typename _BoolType>
    struct __copy_backward_dispatch
    {
      static _BidirectionalIter2
      copy(_BidirectionalIter1 __first, _BidirectionalIter1 __last, 
	   _BidirectionalIter2 __result)
      {
        return __copy_backward(__first, __last,
	                       __result,
			       __iterator_category(__first));
      }
    };

  template<typename _Tp>
    struct __copy_backward_dispatch<_Tp*, _Tp*, __true_type>
    {
      static _Tp*
      copy(const _Tp* __first, const _Tp* __last, _Tp* __result)
      {
	const ptrdiff_t _Num = __last - __first;
	memmove(__result - _Num, __first, sizeof(_Tp) * _Num);
	return __result - _Num;
      }
    };

  template<typename _Tp>
    struct __copy_backward_dispatch<const _Tp*, _Tp*, __true_type>
    {
      static _Tp*
      copy(const _Tp* __first, const _Tp* __last, _Tp* __result)
      {
	return  __copy_backward_dispatch<_Tp*, _Tp*, __true_type>
	  ::copy(__first, __last, __result);
      }
    };

  template<typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_aux(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      typedef typename __type_traits<typename iterator_traits<_BI2>::value_type>
			    ::has_trivial_assignment_operator _Trivial;
      return __copy_backward_dispatch<_BI1, _BI2, _Trivial>
		  ::copy(__first, __last, __result);
    }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_output_normal_iterator(_BI1 __first, _BI1 __last,
					   _BI2 __result, __true_type)
    { return _BI2(__copy_backward_aux(__first, __last, __result.base())); }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_output_normal_iterator(_BI1 __first, _BI1 __last,
					   _BI2 __result, __false_type)
    { return __copy_backward_aux(__first, __last, __result); }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_input_normal_iterator(_BI1 __first, _BI1 __last,
					  _BI2 __result, __true_type)
    {
      typedef typename _Is_normal_iterator<_BI2>::_Normal __Normal;
      return __copy_backward_output_normal_iterator(__first.base(), __last.base(),
						    __result, __Normal());
    }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_input_normal_iterator(_BI1 __first, _BI1 __last,
					  _BI2 __result, __false_type)
    {
      typedef typename _Is_normal_iterator<_BI2>::_Normal __Normal;
      return __copy_backward_output_normal_iterator(__first, __last, __result,
						    __Normal());
    }

  template <typename _BI1, typename _BI2>
    inline _BI2
    copy_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      // concept requirements
      __glibcpp_function_requires(_BidirectionalIteratorConcept<_BI1>);
      __glibcpp_function_requires(_Mutable_BidirectionalIteratorConcept<_BI2>);
      __glibcpp_function_requires(_ConvertibleConcept<
	    typename iterator_traits<_BI1>::value_type,
	    typename iterator_traits<_BI2>::value_type>);

      typedef typename _Is_normal_iterator<_BI1>::_Normal __Normal;
      return __copy_backward_input_normal_iterator(__first, __last, __result,
						   __Normal());
    }

  //--------------------------------------------------
  // copy_n (not part of the C++ standard)

  template<typename _InputIter, typename _Size, typename _OutputIter>
    pair<_InputIter, _OutputIter>
    __copy_n(_InputIter __first, _Size __count,
	     _OutputIter __result,
	     input_iterator_tag)
    {
      for ( ; __count > 0; --__count) {
	*__result = *__first;
	++__first;
	++__result;
      }
      return pair<_InputIter, _OutputIter>(__first, __result);
    }

  template<typename _RAIter, typename _Size, typename _OutputIter>
    inline pair<_RAIter, _OutputIter>
    __copy_n(_RAIter __first, _Size __count,
	     _OutputIter __result,
	     random_access_iterator_tag)
    {
      _RAIter __last = __first + __count;
      return pair<_RAIter, _OutputIter>(__last, copy(__first, __last, __result));
    }

  template<typename _InputIter, typename _Size, typename _OutputIter>
    inline pair<_InputIter, _OutputIter>
    copy_n(_InputIter __first, _Size __count, _OutputIter __result)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter>);
      __glibcpp_function_requires(_OutputIteratorConcept<_OutputIter,
	    typename iterator_traits<_InputIter>::value_type>);

      return __copy_n(__first, __count, __result, __iterator_category(__first));
    }

  //--------------------------------------------------
  // fill and fill_n


  template<typename _ForwardIter, typename _Tp>
    void
    fill(_ForwardIter __first, _ForwardIter __last, const _Tp& __value)
    {
      // concept requirements
      __glibcpp_function_requires(_Mutable_ForwardIteratorConcept<_ForwardIter>);

      for ( ; __first != __last; ++__first)
	*__first = __value;
    }

  template<typename _OutputIter, typename _Size, typename _Tp>
    _OutputIter
    fill_n(_OutputIter __first, _Size __n, const _Tp& __value)
    {
      // concept requirements
      __glibcpp_function_requires(_OutputIteratorConcept<_OutputIter,_Tp>);

      for ( ; __n > 0; --__n, ++__first)
	*__first = __value;
      return __first;
    }

  // Specialization: for one-byte types we can use memset.

  inline void
  fill(unsigned char* __first, unsigned char* __last, const unsigned char& __c)
  {
    unsigned char __tmp = __c;
    memset(__first, __tmp, __last - __first);
  }

  inline void
  fill(signed char* __first, signed char* __last, const signed char& __c)
  {
    signed char __tmp = __c;
    memset(__first, static_cast<unsigned char>(__tmp), __last - __first);
  }

  inline void
  fill(char* __first, char* __last, const char& __c)
  {
    char __tmp = __c;
    memset(__first, static_cast<unsigned char>(__tmp), __last - __first);
  }

  template<typename _Size>
    inline unsigned char*
    fill_n(unsigned char* __first, _Size __n, const unsigned char& __c)
    {
      fill(__first, __first + __n, __c);
      return __first + __n;
    }

  template<typename _Size>
    inline signed char*
    fill_n(char* __first, _Size __n, const signed char& __c)
    {
      fill(__first, __first + __n, __c);
      return __first + __n;
    }

  template<typename _Size>
    inline char*
    fill_n(char* __first, _Size __n, const char& __c)
    {
      fill(__first, __first + __n, __c);
      return __first + __n;
    }


  //--------------------------------------------------
  // equal and mismatch

  template<typename _InputIter1, typename _InputIter2>
    pair<_InputIter1, _InputIter2>
    mismatch(_InputIter1 __first1, _InputIter1 __last1,
	     _InputIter2 __first2)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter1>);
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter2>);
      __glibcpp_function_requires(_EqualityComparableConcept<
	    typename iterator_traits<_InputIter1>::value_type>);
      __glibcpp_function_requires(_EqualityComparableConcept<
	    typename iterator_traits<_InputIter2>::value_type>);

      while (__first1 != __last1 && *__first1 == *__first2) {
	++__first1;
	++__first2;
      }
      return pair<_InputIter1, _InputIter2>(__first1, __first2);
    }

  template<typename _InputIter1, typename _InputIter2, typename _BinaryPredicate>
    pair<_InputIter1, _InputIter2>
    mismatch(_InputIter1 __first1, _InputIter1 __last1,
	     _InputIter2 __first2,
	     _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter1>);
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter2>);

      while (__first1 != __last1 && __binary_pred(*__first1, *__first2)) {
	++__first1;
	++__first2;
      }
      return pair<_InputIter1, _InputIter2>(__first1, __first2);
    }

  template<typename _InputIter1, typename _InputIter2>
    inline bool
    equal(_InputIter1 __first1, _InputIter1 __last1,
	  _InputIter2 __first2)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter1>);
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter2>);
      __glibcpp_function_requires(_EqualOpConcept<
	    typename iterator_traits<_InputIter1>::value_type,
	    typename iterator_traits<_InputIter2>::value_type>);

      for ( ; __first1 != __last1; ++__first1, ++__first2)
	if (!(*__first1 == *__first2))
	  return false;
      return true;
    }

  template<typename _InputIter1, typename _InputIter2, typename _BinaryPredicate>
    inline bool
    equal(_InputIter1 __first1, _InputIter1 __last1,
	  _InputIter2 __first2,
	  _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter1>);
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter2>);

      for ( ; __first1 != __last1; ++__first1, ++__first2)
	if (!__binary_pred(*__first1, *__first2))
	  return false;
      return true;
    }

  //--------------------------------------------------
  // lexicographical_compare and lexicographical_compare_3way.
  // (the latter is not part of the C++ standard.)

  template<typename _InputIter1, typename _InputIter2>
    bool
    lexicographical_compare(_InputIter1 __first1, _InputIter1 __last1,
			    _InputIter2 __first2, _InputIter2 __last2)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter1>);
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter2>);
      __glibcpp_function_requires(_LessThanComparableConcept<
	    typename iterator_traits<_InputIter1>::value_type>);
      __glibcpp_function_requires(_LessThanComparableConcept<
	    typename iterator_traits<_InputIter2>::value_type>);

      for ( ; __first1 != __last1 && __first2 != __last2
	    ; ++__first1, ++__first2) {
	if (*__first1 < *__first2)
	  return true;
	if (*__first2 < *__first1)
	  return false;
      }
      return __first1 == __last1 && __first2 != __last2;
    }

  template<typename _InputIter1, typename _InputIter2, typename _Compare>
    bool
    lexicographical_compare(_InputIter1 __first1, _InputIter1 __last1,
			    _InputIter2 __first2, _InputIter2 __last2,
			    _Compare __comp)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter1>);
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter2>);

      for ( ; __first1 != __last1 && __first2 != __last2
	    ; ++__first1, ++__first2) {
	if (__comp(*__first1, *__first2))
	  return true;
	if (__comp(*__first2, *__first1))
	  return false;
      }
      return __first1 == __last1 && __first2 != __last2;
    }

  inline bool 
  lexicographical_compare(const unsigned char* __first1, const unsigned char* __last1,
			  const unsigned char* __first2, const unsigned char* __last2)
  {
    const size_t __len1 = __last1 - __first1;
    const size_t __len2 = __last2 - __first2;
    const int __result = memcmp(__first1, __first2, min(__len1, __len2));
    return __result != 0 ? __result < 0 : __len1 < __len2;
  }

  inline bool
  lexicographical_compare(const char* __first1, const char* __last1,
			  const char* __first2, const char* __last2)
  {
#if CHAR_MAX == SCHAR_MAX
    return lexicographical_compare((const signed char*) __first1,
				   (const signed char*) __last1,
				   (const signed char*) __first2,
				   (const signed char*) __last2);
#else /* CHAR_MAX == SCHAR_MAX */
    return lexicographical_compare((const unsigned char*) __first1,
				   (const unsigned char*) __last1,
				   (const unsigned char*) __first2,
				   (const unsigned char*) __last2);
#endif /* CHAR_MAX == SCHAR_MAX */
  }

  template<typename _InputIter1, typename _InputIter2>
    int
    __lexicographical_compare_3way(_InputIter1 __first1, _InputIter1 __last1,
				   _InputIter2 __first2, _InputIter2 __last2)
    {
      while (__first1 != __last1 && __first2 != __last2) {
	if (*__first1 < *__first2)
	  return -1;
	if (*__first2 < *__first1)
	  return 1;
	++__first1;
	++__first2;
      }
      if (__first2 == __last2) {
	return !(__first1 == __last1);
      }
      else {
	return -1;
      }
    }

  inline int
  __lexicographical_compare_3way(const unsigned char* __first1,
				 const unsigned char* __last1,
				 const unsigned char* __first2,
				 const unsigned char* __last2)
  {
    const ptrdiff_t __len1 = __last1 - __first1;
    const ptrdiff_t __len2 = __last2 - __first2;
    const int __result = memcmp(__first1, __first2, min(__len1, __len2));
    return __result != 0 ? __result 
			 : (__len1 == __len2 ? 0 : (__len1 < __len2 ? -1 : 1));
  }

  inline int 
  __lexicographical_compare_3way(const char* __first1, const char* __last1,
				 const char* __first2, const char* __last2)
  {
#if CHAR_MAX == SCHAR_MAX
    return __lexicographical_compare_3way(
				  (const signed char*) __first1,
				  (const signed char*) __last1,
				  (const signed char*) __first2,
				  (const signed char*) __last2);
#else
    return __lexicographical_compare_3way((const unsigned char*) __first1,
					  (const unsigned char*) __last1,
					  (const unsigned char*) __first2,
					  (const unsigned char*) __last2);
#endif
  }

  template<typename _InputIter1, typename _InputIter2>
    int
    lexicographical_compare_3way(_InputIter1 __first1, _InputIter1 __last1,
				 _InputIter2 __first2, _InputIter2 __last2)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter1>);
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter2>);
      __glibcpp_function_requires(_LessThanComparableConcept<
	    typename iterator_traits<_InputIter1>::value_type>);
      __glibcpp_function_requires(_LessThanComparableConcept<
	    typename iterator_traits<_InputIter2>::value_type>);

      return __lexicographical_compare_3way(__first1, __last1, __first2, __last2);
    }

} // namespace std

#endif /* __SGI_STL_INTERNAL_ALGOBASE_H */

// Local Variables:
// mode:C++
// End:
