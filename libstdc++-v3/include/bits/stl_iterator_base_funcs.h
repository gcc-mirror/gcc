// Functions used by iterators -*- C++ -*-

// Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

/** @file bits/stl_iterator_base_funcs.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iterator}
 *
 *  This file contains all of the general iterator-related utility
 *  functions, such as distance() and advance().
 */

#ifndef _STL_ITERATOR_BASE_FUNCS_H
#define _STL_ITERATOR_BASE_FUNCS_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/concept_check.h>
#include <debug/assertions.h>
#include <bits/stl_iterator_base_types.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

_GLIBCXX_BEGIN_NAMESPACE_CONTAINER
  // Forward declaration for the overloads of __distance.
  template <typename> struct _List_iterator;
  template <typename> struct _List_const_iterator;
_GLIBCXX_END_NAMESPACE_CONTAINER

  template<typename _InputIterator>
    inline _GLIBCXX14_CONSTEXPR
    typename iterator_traits<_InputIterator>::difference_type
    __distance(_InputIterator __first, _InputIterator __last,
               input_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)

      typename iterator_traits<_InputIterator>::difference_type __n = 0;
      while (__first != __last)
	{
	  ++__first;
	  ++__n;
	}
      return __n;
    }

  template<typename _RandomAccessIterator>
    __attribute__((__always_inline__))
    inline _GLIBCXX14_CONSTEXPR
    typename iterator_traits<_RandomAccessIterator>::difference_type
    __distance(_RandomAccessIterator __first, _RandomAccessIterator __last,
               random_access_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_RandomAccessIteratorConcept<
				  _RandomAccessIterator>)
      return __last - __first;
    }

#if _GLIBCXX_USE_CXX11_ABI
  // Forward declaration because of the qualified call in distance.
  template<typename _Tp>
    ptrdiff_t
    __distance(_GLIBCXX_STD_C::_List_iterator<_Tp>,
	       _GLIBCXX_STD_C::_List_iterator<_Tp>,
	       input_iterator_tag);

  template<typename _Tp>
    ptrdiff_t
    __distance(_GLIBCXX_STD_C::_List_const_iterator<_Tp>,
	       _GLIBCXX_STD_C::_List_const_iterator<_Tp>,
	       input_iterator_tag);
#endif

#if __cplusplus >= 201103L
  // Give better error if std::distance called with a non-Cpp17InputIterator.
  template<typename _OutputIterator>
    void
    __distance(_OutputIterator, _OutputIterator, output_iterator_tag) = delete;
#endif

#ifdef __glibcxx_concepts
namespace __detail
{
  // Satisfied if ITER_TRAITS(Iter)::iterator_category is valid and is
  // at least as strong as ITER_TRAITS(Iter)::iterator_concept.
  template<typename _Iter>
    concept __iter_category_converts_to_concept
      = convertible_to<typename __iter_traits<_Iter>::iterator_category,
		       typename __iter_traits<_Iter>::iterator_concept>;

  // Satisfied if the type is a C++20 iterator that defines iterator_concept,
  // and its iterator_concept is stronger than its iterator_category (if any).
  // Used by std::distance and std::advance to detect iterators which should
  // dispatch based on their C++20 concept not their C++17 category.
  template<typename _Iter>
    concept __promotable_iterator
      = input_iterator<_Iter>
	  && requires { typename __iter_traits<_Iter>::iterator_concept; }
	  && ! __iter_category_converts_to_concept<_Iter>;
} // namespace __detail
#endif

  /**
   *  @brief A generalization of pointer arithmetic.
   *  @param  __first  An input iterator.
   *  @param  __last  An input iterator.
   *  @return  The distance between them.
   *
   *  Returns @c n such that __first + n == __last.  This requires
   *  that @p __last must be reachable from @p __first.  Note that @c
   *  n may be negative.
   *
   *  For random access iterators, this uses their @c + and @c - operations
   *  and are constant time.  For other %iterator classes they are linear time.
  */
  template<typename _InputIterator>
    _GLIBCXX_NODISCARD __attribute__((__always_inline__))
    inline _GLIBCXX17_CONSTEXPR
    typename iterator_traits<_InputIterator>::difference_type
    distance(_InputIterator __first, _InputIterator __last)
    {
#ifdef __glibcxx_concepts
      // A type which satisfies the C++20 random_access_iterator concept might
      // have input_iterator_tag as its iterator_category type, which would
      // mean we select the O(n) __distance. Or a C++20 std::input_iterator
      // that is not a Cpp17InputIterator might have output_iterator_tag as
      // its iterator_category type and then calling __distance with
      // std::__iterator_category(__first) would be ill-formed.
      // So for C++20 iterator types we can just choose to do the right thing.
      if constexpr (__detail::__promotable_iterator<_InputIterator>)
	{
	  if constexpr (random_access_iterator<_InputIterator>)
	    return __last - __first;
	  else
	    return std::__distance(std::move(__first), std::move(__last),
				   input_iterator_tag());
	}
      else // assume it meets the Cpp17InputIterator requirements:
#endif
      // concept requirements -- taken care of in __distance
      return std::__distance(__first, __last,
			     std::__iterator_category(__first));
    }

  template<typename _InputIterator, typename _Distance>
    inline _GLIBCXX14_CONSTEXPR void
    __advance(_InputIterator& __i, _Distance __n, input_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
      __glibcxx_assert(__n >= 0);
      while (__n-- > 0)
	++__i;
    }

  template<typename _BidirectionalIterator, typename _Distance>
    inline _GLIBCXX14_CONSTEXPR void
    __advance(_BidirectionalIterator& __i, _Distance __n,
	      bidirectional_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<
				  _BidirectionalIterator>)
      if (__n > 0)
        while (__n--)
	  ++__i;
      else
        while (__n++)
	  --__i;
    }

  template<typename _RandomAccessIterator, typename _Distance>
    inline _GLIBCXX14_CONSTEXPR void
    __advance(_RandomAccessIterator& __i, _Distance __n,
              random_access_iterator_tag)
    {
      // concept requirements
      __glibcxx_function_requires(_RandomAccessIteratorConcept<
				  _RandomAccessIterator>)
      if (__builtin_constant_p(__n) && __n == 1)
	++__i;
      else if (__builtin_constant_p(__n) && __n == -1)
	--__i;
      else
	__i += __n;
    }

#if __cplusplus >= 201103L
  // Give better error if std::advance called with a non-Cpp17InputIterator.
  template<typename _OutputIterator, typename _Distance>
    void
    __advance(_OutputIterator&, _Distance, output_iterator_tag) = delete;
#endif

  /**
   *  @brief A generalization of pointer arithmetic.
   *  @param  __i  An input iterator.
   *  @param  __n  The @a delta by which to change @p __i.
   *  @return  Nothing.
   *
   *  This increments @p i by @p n.  For bidirectional and random access
   *  iterators, @p __n may be negative, in which case @p __i is decremented.
   *
   *  For random access iterators, this uses their @c + and @c - operations
   *  and are constant time.  For other %iterator classes they are linear time.
  */
  template<typename _InputIterator, typename _Distance>
    __attribute__((__always_inline__))
    inline _GLIBCXX17_CONSTEXPR void
    advance(_InputIterator& __i, _Distance __n)
    {
#ifdef __glibcxx_concepts
      // A type which satisfies the C++20 bidirectional_iterator concept might
      // have input_iterator_tag as its iterator_category type, which would
      // mean we select the __advance overload which cannot move backwards.
      // For a C++20 random_access_iterator we might select the O(n) __advance
      // if it doesn't meet the Cpp17RandomAccessIterator requirements.
      // So for C++20 iterator types we can just choose to do the right thing.
      if constexpr (__detail::__promotable_iterator<_InputIterator>
		      && ranges::__detail::__is_integer_like<_Distance>)
	{
	  auto __d = static_cast<iter_difference_t<_InputIterator>>(__n);
	  if constexpr (random_access_iterator<_InputIterator>)
	    std::__advance(__i, __d, random_access_iterator_tag());
	  else if constexpr (bidirectional_iterator<_InputIterator>)
	    std::__advance(__i, __d, bidirectional_iterator_tag());
	  else
	    std::__advance(__i, __d, input_iterator_tag());
	}
      else // assume it meets the Cpp17InputIterator requirements:
#endif
	{
	  // concept requirements -- taken care of in __advance
	  typename iterator_traits<_InputIterator>::difference_type __d = __n;
	  std::__advance(__i, __d, std::__iterator_category(__i));
	}
    }

#if __cplusplus >= 201103L

  template<typename _InputIterator>
    _GLIBCXX_NODISCARD [[__gnu__::__always_inline__]]
    inline _GLIBCXX17_CONSTEXPR _InputIterator
    next(_InputIterator __x, typename
	 iterator_traits<_InputIterator>::difference_type __n = 1)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
      std::advance(__x, __n);
      return __x;
    }

  template<typename _BidirectionalIterator>
    _GLIBCXX_NODISCARD [[__gnu__::__always_inline__]]
    inline _GLIBCXX17_CONSTEXPR _BidirectionalIterator
    prev(_BidirectionalIterator __x, typename
	 iterator_traits<_BidirectionalIterator>::difference_type __n = 1)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<
				  _BidirectionalIterator>)
      std::advance(__x, -__n);
      return __x;
    }

#endif // C++11

#if __glibcxx_algorithm_iterator_requirements // C++ >= 20
  template<typename _Iter>
    consteval auto
    __iter_concept_or_category()
    {
      if constexpr (__detail::__promotable_iterator<_Iter>)
	{
	  using __type = __detail::__iter_traits<_Iter>::iterator_concept;
	  if constexpr (derived_from<__type, random_access_iterator_tag>)
	    return random_access_iterator_tag{};
	  else
	    return __type{};
	}
      else
	return typename iterator_traits<_Iter>::iterator_category{};
    }

  template<typename _Iter>
    __attribute__((__always_inline__))
    constexpr auto
    __iter_concept_or_category(const _Iter&)
    { return std::__iter_concept_or_category<_Iter>(); }
#else
  template<typename _Iter>
    __attribute__((__always_inline__))
    inline _GLIBCXX_CONSTEXPR
    typename iterator_traits<_Iter>::iterator_category
    __iter_concept_or_category()
    { return typename iterator_traits<_Iter>::iterator_category(); }

  template<typename _Iter>
    __attribute__((__always_inline__))
    inline _GLIBCXX_CONSTEXPR
    typename iterator_traits<_Iter>::iterator_category
    __iter_concept_or_category(const _Iter&)
    { return typename iterator_traits<_Iter>::iterator_category(); }
#endif

  // Like __is_random_access_iter, but based off of __iter_concept_or_category
  // instead of iterator_traits::iterator_category.
  template<typename _Iter,
	   typename _Cat = __decltype(__iter_concept_or_category<_Iter>())>
    struct __is_any_random_access_iter
#if __cplusplus >= 201103L
      : is_base_of<random_access_iterator_tag, _Cat>
#endif
    { enum { __value = __is_base_of(random_access_iterator_tag, _Cat) }; };

// A wrapper around ranges::iter_move that also converts to the iterator's
// value type.
#if __cplusplus >= 202002L
#define _GLIBCXX_ITER_MOVE(__it) \
  std::iter_value_t<decltype(__it)>(std::ranges::iter_move(__it))
#else
#define _GLIBCXX_ITER_MOVE(__it) _GLIBCXX_MOVE(*__it)
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif /* _STL_ITERATOR_BASE_FUNCS_H */
