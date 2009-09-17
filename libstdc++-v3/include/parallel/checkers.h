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
   * @brief Check whether @__c [__begin, @__c __end) is sorted according
   * to @__c __comp.
   * @param __begin Begin iterator of sequence.
   * @param __end End iterator of sequence.
   * @param __comp Comparator.
   * @return @__c true if sorted, @__c false otherwise.
   */
  // XXX Compare default template argument
  template<typename _IIter, typename _Compare>
    bool
    __is_sorted(_IIter __begin, _IIter __end,
              _Compare __comp
              = std::less<typename std::iterator_traits<_IIter>::
              _ValueType>())
    {
      if (__begin == __end)
        return true;

      _IIter __current(__begin), __recent(__begin);

      unsigned long long __position = 1;
      for (__current++; __current != __end; __current++)
        {
          if (__comp(*__current, *__recent))
            {
              printf("__is_sorted: check failed before position %__i.\n",
                     __position);
              return false;
            }
          __recent = __current;
          __position++;
        }

      return true;
    }

  /**
   * @brief Check whether @__c [__begin, @__c __end) is sorted according to
   * @__c __comp.
   * Prints the position in case an unordered pair is found.
   * @param __begin Begin iterator of sequence.
   * @param __end End iterator of sequence.
   * @param __first_failure The first failure is returned in this variable.
   * @param __comp Comparator.
   * @return @__c true if sorted, @__c false otherwise.
   */
  // XXX Compare default template argument
  template<typename _IIter, typename _Compare>
    bool
    is_sorted_failure(_IIter __begin, _IIter __end,
                      _IIter& __first_failure,
                      _Compare __comp
                      = std::less<typename std::iterator_traits<_IIter>::
                      _ValueType>())
    {
      if (__begin == __end)
        return true;

      _IIter __current(__begin), __recent(__begin);

      unsigned long long __position = 1;
      for (__current++; __current != __end; __current++)
        {
          if (__comp(*__current, *__recent))
            {
              __first_failure = __current;
              printf("__is_sorted: check failed before position %lld.\n",
                     __position);
              return false;
            }
          __recent = __current;
          __position++;
        }

      __first_failure = __end;
      return true;
    }

  /**
   * @brief Check whether @__c [__begin, @__c __end) is sorted according to
   * @__c __comp.
   * Prints all unordered pair, including the surrounding two elements.
   * @param __begin Begin iterator of sequence.
   * @param __end End iterator of sequence.
   * @param __comp Comparator.
   * @return @__c true if sorted, @__c false otherwise.
   */
  template<typename _IIter, typename _Compare>
    bool
    // XXX Compare default template argument
    is_sorted_print_failures(_IIter __begin, _IIter __end,
                             _Compare __comp
                             = std::less<typename std::iterator_traits
                             <_IIter>::value_type>())
    {
      if (__begin == __end)
        return true;

      _IIter __recent(__begin);
      bool __ok = true;

      for (_IIter __pos(__begin + 1); __pos != __end; __pos++)
        {
          if (__comp(*__pos, *__recent))
            {
              printf("%ld: %d %d %d %d\n", __pos - __begin, *(__pos - 2),
                     *(__pos- 1), *__pos, *(__pos + 1));
              __ok = false;
            }
          __recent = __pos;
        }
      return __ok;
    }
}

#endif /* _GLIBCXX_PARALLEL_CHECKERS_H */
