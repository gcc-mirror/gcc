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

#ifndef __SGI_STL_INTERNAL_ITERATOR_BASE_FUNCS_H
#define __SGI_STL_INTERNAL_ITERATOR_BASE_FUNCS_H

// This file contains all of the general iterator-related utility
// functions, such as distance() and advance().
// The internal file stl_iterator.h contains predefined iterators, 
// such as front_insert_iterator and istream_iterator.

#pragma GCC system_header
#include <bits/concept_check.h>

namespace std
{

// There are two signatures for distance.  In addition to the one taking
// two iterators and returning a result, there is another taking two
// iterators and a reference-to-result variable, and returning nothing.
// The latter seems to be an SGI extension.   -- pedwards
template <class _InputIterator, class _Distance>
inline void __distance(_InputIterator __first, _InputIterator __last,
                       _Distance& __n, input_iterator_tag)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);
  while (__first != __last) { ++__first; ++__n; }
}

template <class _RandomAccessIterator, class _Distance>
inline void __distance(_RandomAccessIterator __first, 
                       _RandomAccessIterator __last, 
                       _Distance& __n, random_access_iterator_tag)
{
  // concept requirements
  __glibcpp_function_requires(_RandomAccessIteratorConcept<_RandomAccessIterator>);
  __n += __last - __first;
}

template <class _InputIterator, class _Distance>
inline void distance(_InputIterator __first, 
                     _InputIterator __last, _Distance& __n)
{
  // concept requirements -- taken care of in __distance
  __distance(__first, __last, __n, iterator_category(__first));
}

template <class _InputIterator>
inline typename iterator_traits<_InputIterator>::difference_type
__distance(_InputIterator __first, _InputIterator __last, input_iterator_tag)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);
  typename iterator_traits<_InputIterator>::difference_type __n = 0;
  while (__first != __last) {
    ++__first; ++__n;
  }
  return __n;
}

template <class _RandomAccessIterator>
inline typename iterator_traits<_RandomAccessIterator>::difference_type
__distance(_RandomAccessIterator __first, _RandomAccessIterator __last,
           random_access_iterator_tag)
{
  // concept requirements
  __glibcpp_function_requires(_RandomAccessIteratorConcept<_RandomAccessIterator>);
  return __last - __first;
}

template <class _InputIterator>
inline typename iterator_traits<_InputIterator>::difference_type
distance(_InputIterator __first, _InputIterator __last)
{
  // concept requirements -- taken care of in __distance
  typedef typename iterator_traits<_InputIterator>::iterator_category 
    _Category;
  return __distance(__first, __last, _Category());
}

template <class _InputIter, class _Distance>
inline void __advance(_InputIter& __i, _Distance __n, input_iterator_tag)
{
  // concept requirements
  __glibcpp_function_requires(_InputIteratorConcept<_InputIter>);
  while (__n--) ++__i;
}

template <class _BidirectionalIterator, class _Distance>
inline void __advance(_BidirectionalIterator& __i, _Distance __n, 
                      bidirectional_iterator_tag)
{
  // concept requirements
__glibcpp_function_requires(_BidirectionalIteratorConcept<_BidirectionalIterator>);
  if (__n > 0)
    while (__n--) ++__i;
  else
    while (__n++) --__i;
}

template <class _RandomAccessIterator, class _Distance>
inline void __advance(_RandomAccessIterator& __i, _Distance __n, 
                      random_access_iterator_tag)
{
  // concept requirements
  __glibcpp_function_requires(_RandomAccessIteratorConcept<_RandomAccessIterator>);
  __i += __n;
}

template <class _InputIterator, class _Distance>
inline void advance(_InputIterator& __i, _Distance __n)
{
  // concept requirements -- taken care of in __advance
  __advance(__i, __n, iterator_category(__i));
}

} // namespace std

#endif /* __SGI_STL_INTERNAL_ITERATOR_BASE_FUNCS_H */


// Local Variables:
// mode:C++
// End:
