// -*- C++ -*-

// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file parallel/iterator.h
 * @brief Helper iterator classes for the std::transform() functions.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_ITERATOR_H
#define _GLIBCXX_PARALLEL_ITERATOR_H 1

#include <parallel/basic_iterator.h>
#include <bits/stl_pair.h>

namespace __gnu_parallel
{
  /** @brief A pair of iterators. The usual iterator operations are
   *  applied to both child iterators.
   */
  template<typename Iterator1, typename Iterator2, typename IteratorCategory>
    class iterator_pair : public std::pair<Iterator1, Iterator2>
    {
    private:
      typedef iterator_pair<Iterator1, Iterator2, IteratorCategory> type;
      typedef std::pair<Iterator1, Iterator2> base_type;

    public:
      typedef IteratorCategory iterator_category;
      typedef void value_type;

      typedef std::iterator_traits<Iterator1> traits_type;
      typedef typename traits_type::difference_type difference_type;
      typedef type* pointer;
      typedef type& reference;

      iterator_pair() { }

      iterator_pair(const Iterator1& first, const Iterator2& second) 
      : base_type(first, second) { }

      // Pre-increment operator.
      type&
      operator++()
      {
	++base_type::first;
	++base_type::second;
	return *this;
      }

      // Post-increment operator.
      const type
      operator++(int)
      { return type(base_type::first++, base_type::second++); }

      // Pre-decrement operator.
      type&
      operator--()
      {
	--base_type::first;
	--base_type::second;
	return *this;
      }

      // Post-decrement operator.
      const type
      operator--(int)
      { return type(base_type::first--, base_type::second--); }

      // Type conversion.
      operator Iterator2() const
      { return base_type::second; }

      type&
      operator=(const type& other)
      {
	base_type::first = other.first;
	base_type::second = other.second;
	return *this;
      }

      type
      operator+(difference_type delta) const
      { return type(base_type::first + delta, base_type::second + delta); }

      difference_type
      operator-(const type& other) const
      { return base_type::first - other.first; }
  };


  /** @brief A triple of iterators. The usual iterator operations are
      applied to all three child iterators.
   */
  template<typename Iterator1, typename Iterator2, typename Iterator3,
	   typename IteratorCategory>
    class iterator_triple
    {
    private:
      typedef iterator_triple<Iterator1, Iterator2, Iterator3,
			      IteratorCategory> type;

    public:
      typedef IteratorCategory iterator_category;
      typedef void value_type;
      typedef typename std::iterator_traits<Iterator1>::difference_type
                                                            difference_type;
      typedef type* pointer;
      typedef type& reference;

      Iterator1 first;
      Iterator2 second;
      Iterator3 third;

      iterator_triple() { }

      iterator_triple(const Iterator1& _first, const Iterator2& _second,
		      const Iterator3& _third)
      {
	first = _first;
	second = _second;
	third = _third;
      }

      // Pre-increment operator.
      type&
      operator++()
      {
	++first;
	++second;
	++third;
	return *this;
      }

      // Post-increment operator.
      const type
      operator++(int)
      { return type(first++, second++, third++); }

      // Pre-decrement operator.
      type&
      operator--()
      {
	--first;
	--second;
	--third;
	return *this;
      }

      // Post-decrement operator.
      const type
      operator--(int)
      { return type(first--, second--, third--); }

      // Type conversion.
      operator Iterator3() const
      { return third; }

      type&
      operator=(const type& other)
      {
	first = other.first;
	second = other.second;
	third = other.third;
	return *this;
      }

      type
      operator+(difference_type delta) const
      { return type(first + delta, second + delta, third + delta); }

      difference_type
      operator-(const type& other) const
      { return first - other.first; }
  };
}

#endif /* _GLIBCXX_PARALLEL_ITERATOR_H */
