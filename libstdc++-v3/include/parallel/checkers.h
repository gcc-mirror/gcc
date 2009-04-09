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

/** @file parallel/checkers.h
 *  @brief Routines for checking the correctness of algorithm results.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_CHECKERS_H
#define _GLIBCXX_PARALLEL_CHECKERS_H 1

#include <functional>
#include <cstdio>
#include <bits/stl_algobase.h>

namespace __gnu_parallel
{
  /**
   * @brief Check whether @c [begin, @c end) is sorted according to @c comp.
   * @param begin Begin iterator of sequence.
   * @param end End iterator of sequence.
   * @param comp Comparator.
   * @return @c true if sorted, @c false otherwise.
   */
  // XXX Comparator default template argument
  template<typename InputIterator, typename Comparator>
    bool
    is_sorted(InputIterator begin, InputIterator end,
	      Comparator comp
	      = std::less<typename std::iterator_traits<InputIterator>::
	      value_type>())
    {
      if (begin == end)
	return true;

      InputIterator current(begin), recent(begin);

      unsigned long long position = 1;
      for (current++; current != end; current++)
	{
	  if (comp(*current, *recent))
	    {
	      printf("is_sorted: check failed before position %i.\n",
		     position);
	      return false;
	    }
	  recent = current;
	  position++;
	}

      return true;
    }

  /**
   * @brief Check whether @c [begin, @c end) is sorted according to @c comp.
   * Prints the position in case an unordered pair is found.
   * @param begin Begin iterator of sequence.
   * @param end End iterator of sequence.
   * @param first_failure The first failure is returned in this variable.
   * @param comp Comparator.
   * @return @c true if sorted, @c false otherwise.
   */
  // XXX Comparator default template argument
  template<typename InputIterator, typename Comparator>
    bool
    is_sorted_failure(InputIterator begin, InputIterator end,
		      InputIterator& first_failure,
		      Comparator comp
		      = std::less<typename std::iterator_traits<InputIterator>::
		      value_type>())
    {
      if (begin == end)
	return true;

      InputIterator current(begin), recent(begin);

      unsigned long long position = 1;
      for (current++; current != end; current++)
	{
	  if (comp(*current, *recent))
	    {
	      first_failure = current;
	      printf("is_sorted: check failed before position %lld.\n",
		     position);
	      return false;
	    }
	  recent = current;
	  position++;
	}

      first_failure = end;
      return true;
    }

  /**
   * @brief Check whether @c [begin, @c end) is sorted according to @c comp.
   * Prints all unordered pair, including the surrounding two elements.
   * @param begin Begin iterator of sequence.
   * @param end End iterator of sequence.
   * @param comp Comparator.
   * @return @c true if sorted, @c false otherwise.
   */
  template<typename InputIterator, typename Comparator>
    bool
    // XXX Comparator default template argument
    is_sorted_print_failures(InputIterator begin, InputIterator end,
			     Comparator comp
			     = std::less<typename std::iterator_traits
			     <InputIterator>::value_type>())
    {
      if (begin == end)
	return true;

      InputIterator recent(begin);
      bool ok = true;

      for (InputIterator pos(begin + 1); pos != end; pos++)
	{
	  if (comp(*pos, *recent))
	    {
	      printf("%ld: %d %d %d %d\n", pos - begin, *(pos - 2),
		     *(pos- 1), *pos, *(pos + 1));
	      ok = false;
	    }
	  recent = pos;
	}
      return ok;
    }
}

#endif /* _GLIBCXX_PARALLEL_CHECKERS_H */
