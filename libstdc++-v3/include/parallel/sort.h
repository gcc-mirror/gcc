// -*- C++ -*-

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

/** @file parallel/sort.h
 *  @brief Parallel sorting algorithm switch.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_SORT_H
#define _GLIBCXX_PARALLEL_SORT_H 1

#include <parallel/basic_iterator.h>
#include <parallel/features.h>
#include <parallel/parallel.h>

#if _GLIBCXX_ASSERTIONS
#include <parallel/checkers.h>
#endif

#if _GLIBCXX_MERGESORT
#include <parallel/multiway_mergesort.h>
#endif

#if _GLIBCXX_QUICKSORT
#include <parallel/quicksort.h>
#endif

#if _GLIBCXX_BAL_QUICKSORT
#include <parallel/balanced_quicksort.h>
#endif

namespace __gnu_parallel
{
  /** 
   *  @brief Choose a parallel sorting algorithm.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @param stable Sort stable.
   *  @callgraph 
   */
  template<typename RandomAccessIterator, typename Comparator>
    inline void
    parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
                  Comparator comp, bool stable)
    {
      _GLIBCXX_CALL(end - begin)
      typedef std::iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;

      if (begin != end)
      {
        difference_type n = end - begin;

        if (false) ;
#if _GLIBCXX_MERGESORT
        else if (stable)
          {
            if(_Settings::get().sort_splitting == EXACT)
              parallel_sort_mwms<true, true>
                (begin, end, comp, get_max_threads());
            else
              parallel_sort_mwms<true, false>
                (begin, end, comp, get_max_threads());
          }
        else if (_Settings::get().sort_algorithm == MWMS)
          {
            if(_Settings::get().sort_splitting == EXACT)
              parallel_sort_mwms<false, true>
                (begin, end, comp, get_max_threads());
            else
              parallel_sort_mwms<false, false>
                (begin, end, comp, get_max_threads());
          }
#endif
#if _GLIBCXX_QUICKSORT
        else if (!stable && _Settings::get().sort_algorithm == QS)
          parallel_sort_qs(begin, end, comp, n, get_max_threads());
#endif
#if _GLIBCXX_BAL_QUICKSORT
        else if (!stable && _Settings::get().sort_algorithm == QS_BALANCED)
          parallel_sort_qsb(begin, end, comp, n, get_max_threads());
#endif
        else if(stable)
          __gnu_sequential::stable_sort(begin, end, comp);
        else
          __gnu_sequential::sort(begin, end, comp);
      }
    }
} // end namespace __gnu_parallel

#endif
