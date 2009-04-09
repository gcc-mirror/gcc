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
	//prototype
  template<bool stable, typename RandomAccessIterator,
           typename Comparator, typename Parallelism>
  void
  parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
  Comparator comp, Parallelism parallelism);
	
  /** 
   *  @brief Choose multiway mergesort, splitting variant at run-time,
   *  for parallel sorting.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @callgraph 
   */
  template<bool stable, typename RandomAccessIterator, typename Comparator>
  inline void
  parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
    Comparator comp, multiway_mergesort_tag parallelism)
  {
    _GLIBCXX_CALL(end - begin)

    if(_Settings::get().sort_splitting == EXACT)
      parallel_sort_mwms<stable, true>
        (begin, end, comp, parallelism.get_num_threads());
    else
      parallel_sort_mwms<stable, false>
        (begin, end, comp, parallelism.get_num_threads());
  }

  /** 
   *  @brief Choose multiway mergesort with exact splitting,
   *  for parallel sorting.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @callgraph 
   */
  template<bool stable, typename RandomAccessIterator, typename Comparator>
  inline void
  parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
    Comparator comp, multiway_mergesort_exact_tag parallelism)
  {
    _GLIBCXX_CALL(end - begin)

      parallel_sort_mwms<stable, true>
        (begin, end, comp, parallelism.get_num_threads());
  }

  /** 
   *  @brief Choose multiway mergesort with splitting by sampling,
   *  for parallel sorting.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @callgraph 
   */
  template<bool stable, typename RandomAccessIterator, typename Comparator>
  inline void
  parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
    Comparator comp, multiway_mergesort_sampling_tag parallelism)
  {
    _GLIBCXX_CALL(end - begin)

    parallel_sort_mwms<stable, false>
      (begin, end, comp, parallelism.get_num_threads());
  }

  /**
   *  @brief Choose quicksort for parallel sorting.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @callgraph 
   */
  template<bool stable, typename RandomAccessIterator, typename Comparator>
  inline void
  parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
    Comparator comp, quicksort_tag parallelism)
  {
    _GLIBCXX_CALL(end - begin)

    _GLIBCXX_PARALLEL_ASSERT(stable == false);

    parallel_sort_qs(begin, end, comp, parallelism.get_num_threads());
  }

  /**
   *  @brief Choose balanced quicksort for parallel sorting.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @param stable Sort stable.
   *  @callgraph 
   */
  template<bool stable, typename RandomAccessIterator, typename Comparator>
  inline void
  parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
    Comparator comp, balanced_quicksort_tag parallelism)
  {
    _GLIBCXX_CALL(end - begin)

    _GLIBCXX_PARALLEL_ASSERT(stable == false);

    parallel_sort_qsb(begin, end, comp, parallelism.get_num_threads());
  }


  /** 
   *  @brief Choose multiway mergesort with exact splitting,
   *  for parallel sorting.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @callgraph 
   */
  template<bool stable, typename RandomAccessIterator, typename Comparator>
  inline void
  parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
    Comparator comp, default_parallel_tag parallelism)
  {
    _GLIBCXX_CALL(end - begin)

    parallel_sort<stable>
      (begin, end, comp,
        multiway_mergesort_exact_tag(parallelism.get_num_threads()));
  }


  /**
   *  @brief Choose a parallel sorting algorithm.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param comp Comparator.
   *  @param stable Sort stable.
   *  @callgraph 
   */
  template<bool stable, typename RandomAccessIterator, typename Comparator>
    inline void
    parallel_sort(RandomAccessIterator begin, RandomAccessIterator end,
                  Comparator comp, parallel_tag parallelism)
    {
      _GLIBCXX_CALL(end - begin)
      typedef std::iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;

      if (false) ;
#if _GLIBCXX_MERGESORT
      else if (stable || _Settings::get().sort_algorithm == MWMS)
        {
          if(_Settings::get().sort_splitting == EXACT)
            parallel_sort_mwms<stable, true>
              (begin, end, comp, parallelism.get_num_threads());
          else
            parallel_sort_mwms<false, false>
              (begin, end, comp, parallelism.get_num_threads());
        }
#endif
#if _GLIBCXX_QUICKSORT
      else if (_Settings::get().sort_algorithm == QS)
        parallel_sort_qs(begin, end, comp, parallelism.get_num_threads());
#endif
#if _GLIBCXX_BAL_QUICKSORT
      else if (_Settings::get().sort_algorithm == QS_BALANCED)
        parallel_sort_qsb(begin, end, comp, parallelism.get_num_threads());
#endif
      else
        __gnu_sequential::sort(begin, end, comp);
    }
} // end namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_SORT_H */
