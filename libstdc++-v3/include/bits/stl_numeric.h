// Numeric functions implementation -*- C++ -*-

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

/* NOTE: This is an internal header file, included by other STL headers.
 *   You should not attempt to use it directly.
 */


#ifndef _CPP_BITS_STL_NUMERIC_H
#define _CPP_BITS_STL_NUMERIC_H 1

namespace std
{

template <class _InputIterator, class _Tp>
_Tp accumulate(_InputIterator __first, _InputIterator __last, _Tp __init)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);

  for ( ; __first != __last; ++__first)
    __init = __init + *__first;
  return __init;
}

template <class _InputIterator, class _Tp, class _BinaryOperation>
_Tp accumulate(_InputIterator __first, _InputIterator __last, _Tp __init,
              _BinaryOperation __binary_op)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);

  for ( ; __first != __last; ++__first)
    __init = __binary_op(__init, *__first);
  return __init;
}

template <class _InputIterator1, class _InputIterator2, class _Tp>
_Tp inner_product(_InputIterator1 __first1, _InputIterator1 __last1,
                 _InputIterator2 __first2, _Tp __init)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator1>);
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator2>);

  for ( ; __first1 != __last1; ++__first1, ++__first2)
    __init = __init + (*__first1 * *__first2);
  return __init;
}

template <class _InputIterator1, class _InputIterator2, class _Tp,
          class _BinaryOperation1, class _BinaryOperation2>
_Tp inner_product(_InputIterator1 __first1, _InputIterator1 __last1,
                 _InputIterator2 __first2, _Tp __init, 
                 _BinaryOperation1 __binary_op1,
                 _BinaryOperation2 __binary_op2)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator1>);
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator2>);

  for ( ; __first1 != __last1; ++__first1, ++__first2)
    __init = __binary_op1(__init, __binary_op2(*__first1, *__first2));
  return __init;
}

template <class _InputIterator, class _OutputIterator, class _Tp>
_OutputIterator 
__partial_sum(_InputIterator __first, _InputIterator __last,
              _OutputIterator __result, _Tp*)
{
  _Tp __value = *__first;
  while (++__first != __last) {
    __value = __value + *__first;
    *++__result = __value;
  }
  return ++__result;
}

template <class _InputIterator, class _OutputIterator>
_OutputIterator 
partial_sum(_InputIterator __first, _InputIterator __last,
            _OutputIterator __result)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);
  __glibcpp_function_requires(_OutputIteratorConcept<_OutputIterator,
        typename iterator_traits<_InputIterator>::value_type>);

  if (__first == __last) return __result;
  *__result = *__first;
  return __partial_sum(__first, __last, __result, __value_type(__first));
}

template <class _InputIterator, class _OutputIterator, class _Tp,
          class _BinaryOperation>
_OutputIterator 
__partial_sum(_InputIterator __first, _InputIterator __last, 
              _OutputIterator __result, _Tp*, _BinaryOperation __binary_op)
{
  _Tp __value = *__first;
  while (++__first != __last) {
    __value = __binary_op(__value, *__first);
    *++__result = __value;
  }
  return ++__result;
}

template <class _InputIterator, class _OutputIterator, class _BinaryOperation>
_OutputIterator 
partial_sum(_InputIterator __first, _InputIterator __last,
            _OutputIterator __result, _BinaryOperation __binary_op)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);
  __glibcpp_function_requires(_OutputIteratorConcept<_OutputIterator,
        typename iterator_traits<_InputIterator>::value_type>);

  if (__first == __last) return __result;
  *__result = *__first;
  return __partial_sum(__first, __last, __result, __value_type(__first), 
                       __binary_op);
}

template <class _InputIterator, class _OutputIterator, class _Tp>
_OutputIterator 
__adjacent_difference(_InputIterator __first, _InputIterator __last,
                      _OutputIterator __result, _Tp*)
{
  _Tp __value = *__first;
  while (++__first != __last) {
    _Tp __tmp = *__first;
    *++__result = __tmp - __value;
    __value = __tmp;
  }
  return ++__result;
}

template <class _InputIterator, class _OutputIterator>
_OutputIterator
adjacent_difference(_InputIterator __first,
                    _InputIterator __last, _OutputIterator __result)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);
  __glibcpp_function_requires(_OutputIteratorConcept<_OutputIterator,
        typename iterator_traits<_InputIterator>::value_type>);

  if (__first == __last) return __result;
  *__result = *__first;
  return __adjacent_difference(__first, __last, __result,
                               __value_type(__first));
}

template <class _InputIterator, class _OutputIterator, class _Tp, 
          class _BinaryOperation>
_OutputIterator
__adjacent_difference(_InputIterator __first, _InputIterator __last, 
                      _OutputIterator __result, _Tp*,
                      _BinaryOperation __binary_op) {
  _Tp __value = *__first;
  while (++__first != __last) {
    _Tp __tmp = *__first;
    *++__result = __binary_op(__tmp, __value);
    __value = __tmp;
  }
  return ++__result;
}

template <class _InputIterator, class _OutputIterator, class _BinaryOperation>
_OutputIterator 
adjacent_difference(_InputIterator __first, _InputIterator __last,
                    _OutputIterator __result, _BinaryOperation __binary_op)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);
  __glibcpp_function_requires(_OutputIteratorConcept<_OutputIterator,
        typename iterator_traits<_InputIterator>::value_type>);

  if (__first == __last) return __result;
  *__result = *__first;
  return __adjacent_difference(__first, __last, __result,
                               __value_type(__first),
                               __binary_op);
}

// Returns __x ** __n, where __n >= 0.  _Note that "multiplication"
// is required to be associative, but not necessarily commutative.

 
template <class _Tp, class _Integer, class _MonoidOperation>
_Tp __power(_Tp __x, _Integer __n, _MonoidOperation __monoid_op)
{
  if (__n == 0)
    return identity_element(__monoid_op);
  else {
    while ((__n & 1) == 0) {
      __n >>= 1;
      __x = __monoid_op(__x, __x);
    }

    _Tp __result = __x;
    __n >>= 1;
    while (__n != 0) {
      __x = __monoid_op(__x, __x);
      if ((__n & 1) != 0)
        __result = __monoid_op(__result, __x);
      __n >>= 1;
    }
    return __result;
  }
}

template <class _Tp, class _Integer>
inline _Tp __power(_Tp __x, _Integer __n)
{
  return __power(__x, __n, multiplies<_Tp>());
}

// Alias for the internal name __power.  Note that power is an extension,
// not part of the C++ standard.

template <class _Tp, class _Integer, class _MonoidOperation>
inline _Tp power(_Tp __x, _Integer __n, _MonoidOperation __monoid_op)
{
  return __power(__x, __n, __monoid_op);
}

template <class _Tp, class _Integer>
inline _Tp power(_Tp __x, _Integer __n)
{
  return __power(__x, __n);
}

// iota is not part of the C++ standard.  It is an extension.

template <class _ForwardIter, class _Tp>
void 
iota(_ForwardIter __first, _ForwardIter __last, _Tp __value)
{
  // concept requirements
  __glibcpp_function_requires(_Mutable_ForwardIteratorConcept<_ForwardIter>);
  __glibcpp_function_requires(_ConvertibleConcept<_Tp,
        typename iterator_traits<_ForwardIter>::value_type>);

  while (__first != __last)
    *__first++ = __value++;
}

} // namespace std

#endif /* _CPP_BITS_STL_NUMERIC_H */

// Local Variables:
// mode:C++
// End:
