// Functions used by iterators -*- C++ -*-

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
  template<typename _InputIterator, typename _Distance>
    inline void
    __distance(_InputIterator __first, _InputIterator __last,
	       _Distance& __n, input_iterator_tag)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIterator>);
      while (__first != __last) { ++__first; ++__n; }
    }

  template<typename _RandomAccessIterator, typename _Distance>
    inline void
    __distance(_RandomAccessIterator __first, _RandomAccessIterator __last, 
	       _Distance& __n, random_access_iterator_tag)
    {
      // concept requirements
      __glibcpp_function_requires(_RandomAccessIteratorConcept<_RandomAccessIterator>);
      __n += __last - __first;
    }

  template<typename _InputIterator, typename _Distance>
    inline void
    distance(_InputIterator __first, _InputIterator __last,
             _Distance& __n)
    {
      // concept requirements -- taken care of in __distance
      __distance(__first, __last, __n, __iterator_category(__first));
    }

  template<typename _InputIterator>
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

  template<typename _RandomAccessIterator>
    inline typename iterator_traits<_RandomAccessIterator>::difference_type
    __distance(_RandomAccessIterator __first, _RandomAccessIterator __last,
			   random_access_iterator_tag)
    {
      // concept requirements
      __glibcpp_function_requires(_RandomAccessIteratorConcept<_RandomAccessIterator>);
      return __last - __first;
    }

  template<typename _InputIterator>
    inline typename iterator_traits<_InputIterator>::difference_type
    distance(_InputIterator __first, _InputIterator __last)
    {
      // concept requirements -- taken care of in __distance
      return __distance(__first, __last, __iterator_category(__first));
    }

  template<typename _InputIter, typename _Distance>
    inline void
    __advance(_InputIter& __i, _Distance __n, input_iterator_tag)
    {
      // concept requirements
      __glibcpp_function_requires(_InputIteratorConcept<_InputIter>);
      while (__n--) ++__i;
    }

  template<typename _BidirectionalIterator, typename _Distance>
    inline void
    __advance(_BidirectionalIterator& __i, _Distance __n, bidirectional_iterator_tag)
    {
      // concept requirements
      __glibcpp_function_requires(_BidirectionalIteratorConcept<_BidirectionalIterator>);
      if (__n > 0)
	while (__n--) ++__i;
      else
	while (__n++) --__i;
    }

  template<typename _RandomAccessIterator, typename _Distance>
    inline void
    __advance(_RandomAccessIterator& __i, _Distance __n, random_access_iterator_tag)
    {
      // concept requirements
      __glibcpp_function_requires(_RandomAccessIteratorConcept<_RandomAccessIterator>);
      __i += __n;
    }

  template<typename _InputIterator, typename _Distance>
    inline void
    advance(_InputIterator& __i, _Distance __n)
    {
      // concept requirements -- taken care of in __advance
      __advance(__i, __n, __iterator_category(__i));
    }

} // namespace std

#endif /* __SGI_STL_INTERNAL_ITERATOR_BASE_FUNCS_H */


// Local Variables:
// mode:C++
// End:
