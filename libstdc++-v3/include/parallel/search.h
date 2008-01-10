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

/** @file parallel/search.h
 *  @brief Parallel implementation base for std::search() and
 *  std::search_n().
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze.

#ifndef _GLIBCXX_PARALLEL_SEARCH_H
#define _GLIBCXX_PARALLEL_SEARCH_H 1

#include <bits/stl_algobase.h>

#include <parallel/parallel.h>
#include <parallel/equally_split.h>


namespace __gnu_parallel
{
  /**
   *  @brief Precalculate advances for Knuth-Morris-Pratt algorithm.
   *  @param elements Begin iterator of sequence to search for.
   *  @param length Length of sequence to search for.
   *  @param advances Returned offsets. 
   */
template<typename RandomAccessIterator, typename _DifferenceTp>
  void
  calc_borders(RandomAccessIterator elements, _DifferenceTp length, 
              _DifferenceTp* off)
  {
    typedef _DifferenceTp difference_type;

    off[0] = -1;
    if (length > 1)
      off[1] = 0;
    difference_type k = 0;
    for (difference_type j = 2; j <= length; j++)
      {
        while ((k >= 0) && !(elements[k] == elements[j-1]))
          k = off[k];
        off[j] = ++k;
      }
  }

  // Generic parallel find algorithm (requires random access iterator).

  /** @brief Parallel std::search.
   *  @param begin1 Begin iterator of first sequence.
   *  @param end1 End iterator of first sequence.
   *  @param begin2 Begin iterator of second sequence.
   *  @param end2 End iterator of second sequence.
   *  @param pred Find predicate.
   *  @return Place of finding in first sequences. */
template<typename _RandomAccessIterator1,
	 typename _RandomAccessIterator2,
	 typename Pred>
  _RandomAccessIterator1
  search_template(_RandomAccessIterator1 begin1, _RandomAccessIterator1 end1,
                  _RandomAccessIterator2 begin2, _RandomAccessIterator2 end2,
                  Pred pred)
  {
    typedef std::iterator_traits<_RandomAccessIterator1> traits_type;
    typedef typename traits_type::difference_type difference_type;

    _GLIBCXX_CALL((end1 - begin1) + (end2 - begin2));

    difference_type pattern_length = end2 - begin2;

    // Pattern too short.
    if(pattern_length <= 0)
      return end1;

    // Last point to start search.
    difference_type input_length = (end1 - begin1) - pattern_length;

    // Where is first occurrence of pattern? defaults to end.
    difference_type result = (end1 - begin1);
    difference_type *splitters;

    // Pattern too long.
    if (input_length < 0)
      return end1;

    omp_lock_t result_lock;
    omp_init_lock(&result_lock);

    thread_index_t num_threads =
        std::max<difference_type>(1,
            std::min<difference_type>(input_length, get_max_threads()));

    difference_type advances[pattern_length];
    calc_borders(begin2, pattern_length, advances);

#   pragma omp parallel num_threads(num_threads)
      {
#       pragma omp single
          {
            num_threads = omp_get_num_threads();
            splitters = new difference_type[num_threads + 1];
            equally_split(input_length, num_threads, splitters);
          }

        thread_index_t iam = omp_get_thread_num();

        difference_type start = splitters[iam], stop = splitters[iam + 1];

        difference_type pos_in_pattern = 0;
        bool found_pattern = false;

        while (start <= stop && !found_pattern)
          {
            // Get new value of result.
            #pragma omp flush(result)
            // No chance for this thread to find first occurrence.
            if (result < start)
              break;
            while (pred(begin1[start + pos_in_pattern],
                         begin2[pos_in_pattern]))
              {
                ++pos_in_pattern;
                if (pos_in_pattern == pattern_length)
                  {
                    // Found new candidate for result.
                            omp_set_lock(&result_lock);
                    result = std::min(result, start);
                            omp_unset_lock(&result_lock);

                    found_pattern = true;
                    break;
                  }
              }
            // Make safe jump.
            start += (pos_in_pattern - advances[pos_in_pattern]);
            pos_in_pattern =
                (advances[pos_in_pattern] < 0) ? 0 : advances[pos_in_pattern];
          }
      } //parallel

    omp_destroy_lock(&result_lock);

    delete[] splitters;

    // Return iterator on found element.
    return (begin1 + result);
  }
} // end namespace

#endif
