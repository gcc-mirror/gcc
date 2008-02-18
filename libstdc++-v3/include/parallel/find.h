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

/** @file parallel/find.h
 *  @brief Parallel implementation base for std::find(), std::equal()
 *  and related functions.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze and Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_FIND_H
#define _GLIBCXX_PARALLEL_FIND_H 1

#include <bits/stl_algobase.h>

#include <parallel/features.h>
#include <parallel/parallel.h>
#include <parallel/compatibility.h>
#include <parallel/equally_split.h>

namespace __gnu_parallel
{
/**
 *  @brief Parallel std::find, switch for different algorithms.
 *  @param begin1 Begin iterator of first sequence.
 *  @param end1 End iterator of first sequence.
 *  @param begin2 Begin iterator of second sequence. Must have same
 *  length as first sequence.
 *  @param pred Find predicate.
 *  @param selector Functionality (e. g. std::find_if (), std::equal(),...)
 *  @return Place of finding in both sequences.
 */
template<typename RandomAccessIterator1,
	 typename RandomAccessIterator2,
	 typename Pred,
	 typename Selector>
  inline std::pair<RandomAccessIterator1, RandomAccessIterator2>
  find_template(RandomAccessIterator1 begin1, RandomAccessIterator1 end1,
                RandomAccessIterator2 begin2, Pred pred, Selector selector)
  {
    switch (_Settings::get().find_algorithm)
      {
      case GROWING_BLOCKS:
        return find_template(begin1, end1, begin2, pred, selector,
			     growing_blocks_tag());
      case CONSTANT_SIZE_BLOCKS:
        return find_template(begin1, end1, begin2, pred, selector,
			     constant_size_blocks_tag());
      case EQUAL_SPLIT:
        return find_template(begin1, end1, begin2, pred, selector,
			     equal_split_tag());
      default:
        _GLIBCXX_PARALLEL_ASSERT(false);
        return std::make_pair(begin1, begin2);
      }
  }

#if _GLIBCXX_FIND_EQUAL_SPLIT

/**
 *  @brief Parallel std::find, equal splitting variant.
 *  @param begin1 Begin iterator of first sequence.
 *  @param end1 End iterator of first sequence.
 *  @param begin2 Begin iterator of second sequence. Second sequence
 *  must have same length as first sequence.
 *  @param pred Find predicate.
 *  @param selector Functionality (e. g. std::find_if (), std::equal(),...)
 *  @return Place of finding in both sequences.
 */
template<typename RandomAccessIterator1,
	 typename RandomAccessIterator2,
	 typename Pred,
	 typename Selector>
  std::pair<RandomAccessIterator1, RandomAccessIterator2>
  find_template(RandomAccessIterator1 begin1,
                RandomAccessIterator1 end1,
                RandomAccessIterator2 begin2,
                Pred pred,
                Selector selector,
                equal_split_tag)
  {
    _GLIBCXX_CALL(end1 - begin1)

    typedef std::iterator_traits<RandomAccessIterator1> traits_type;
    typedef typename traits_type::difference_type difference_type;
    typedef typename traits_type::value_type value_type;

    difference_type length = end1 - begin1;
    difference_type result = length;
    difference_type* borders;

    omp_lock_t result_lock;
    omp_init_lock(&result_lock);

    thread_index_t num_threads = get_max_threads();
#   pragma omp parallel num_threads(num_threads)
      {
#       pragma omp single
          {
            num_threads = omp_get_num_threads();
            borders = new difference_type[num_threads + 1];
            equally_split(length, num_threads, borders);
          } //single

        thread_index_t iam = omp_get_thread_num();
        difference_type start = borders[iam], stop = borders[iam + 1];

        RandomAccessIterator1 i1 = begin1 + start;
        RandomAccessIterator2 i2 = begin2 + start;
        for (difference_type pos = start; pos < stop; ++pos)
          {
            #pragma omp flush(result)
            // Result has been set to something lower.
            if (result < pos)
              break;

            if (selector(i1, i2, pred))
              {
                omp_set_lock(&result_lock);
                if (pos < result)
                  result = pos;
                omp_unset_lock(&result_lock);
                break;
              }
            ++i1;
            ++i2;
          }
      } //parallel

    omp_destroy_lock(&result_lock);
    delete[] borders;

    return
      std::pair<RandomAccessIterator1, RandomAccessIterator2>(begin1 + result,
							      begin2 + result);
  }

#endif

#if _GLIBCXX_FIND_GROWING_BLOCKS

/**
 *  @brief Parallel std::find, growing block size variant.
 *  @param begin1 Begin iterator of first sequence.
 *  @param end1 End iterator of first sequence.
 *  @param begin2 Begin iterator of second sequence. Second sequence
 *  must have same length as first sequence.
 *  @param pred Find predicate.
 *  @param selector Functionality (e. g. std::find_if (), std::equal(),...)
 *  @return Place of finding in both sequences.
 *  @see __gnu_parallel::_Settings::find_sequential_search_size
 *  @see __gnu_parallel::_Settings::find_initial_block_size
 *  @see __gnu_parallel::_Settings::find_maximum_block_size
 *  @see __gnu_parallel::_Settings::find_increasing_factor
 *
 *  There are two main differences between the growing blocks and
 *  the constant-size blocks variants.
 *  1. For GB, the block size grows; for CSB, the block size is fixed.

 *  2. For GB, the blocks are allocated dynamically;
 *     for CSB, the blocks are allocated in a predetermined manner,
 *     namely spacial round-robin.
 */
template<typename RandomAccessIterator1,
	 typename RandomAccessIterator2,
	 typename Pred,
	 typename Selector>
  std::pair<RandomAccessIterator1, RandomAccessIterator2>
  find_template(RandomAccessIterator1 begin1, RandomAccessIterator1 end1,
                RandomAccessIterator2 begin2, Pred pred, Selector selector,
                growing_blocks_tag)
  {
    _GLIBCXX_CALL(end1 - begin1)

    typedef std::iterator_traits<RandomAccessIterator1> traits_type;
    typedef typename traits_type::difference_type difference_type;
    typedef typename traits_type::value_type value_type;

    const _Settings& __s = _Settings::get();

    difference_type length = end1 - begin1;

    difference_type sequential_search_size =
      std::min<difference_type>(length, __s.find_sequential_search_size);

    // Try it sequentially first.
    std::pair<RandomAccessIterator1, RandomAccessIterator2> find_seq_result =
      selector.sequential_algorithm(
          begin1, begin1 + sequential_search_size, begin2, pred);

    if (find_seq_result.first != (begin1 + sequential_search_size))
      return find_seq_result;

    // Index of beginning of next free block (after sequential find).
    difference_type next_block_start = sequential_search_size;
    difference_type result = length;

    omp_lock_t result_lock;
    omp_init_lock(&result_lock);

    thread_index_t num_threads = get_max_threads();
#   pragma omp parallel shared(result) num_threads(num_threads)
      {
#       pragma omp single
          num_threads = omp_get_num_threads();

        // Not within first k elements -> start parallel.
        thread_index_t iam = omp_get_thread_num();

        difference_type block_size = __s.find_initial_block_size;
        difference_type start =
            fetch_and_add<difference_type>(&next_block_start, block_size);

        // Get new block, update pointer to next block.
        difference_type stop =
            std::min<difference_type>(length, start + block_size);

        std::pair<RandomAccessIterator1, RandomAccessIterator2> local_result;

        while (start < length)
          {
#           pragma omp flush(result)
            // Get new value of result.
            if (result < start)
              {
                // No chance to find first element.
                break;
              }

            local_result = selector.sequential_algorithm(
                begin1 + start, begin1 + stop, begin2 + start, pred);
            if (local_result.first != (begin1 + stop))
              {
                omp_set_lock(&result_lock);
                if ((local_result.first - begin1) < result)
                  {
                    result = local_result.first - begin1;

                    // Result cannot be in future blocks, stop algorithm.
                    fetch_and_add<difference_type>(&next_block_start, length);
                  }
                  omp_unset_lock(&result_lock);
              }

            block_size =
	      std::min<difference_type>(block_size * __s.find_increasing_factor,
					__s.find_maximum_block_size);

            // Get new block, update pointer to next block.
            start =
	      fetch_and_add<difference_type>(&next_block_start, block_size);
            stop = ((length < (start + block_size))
		    ? length : (start + block_size));
          }
      } //parallel

    omp_destroy_lock(&result_lock);

    // Return iterator on found element.
    return
      std::pair<RandomAccessIterator1, RandomAccessIterator2>(begin1 + result,
							      begin2 + result);
  }

#endif

#if _GLIBCXX_FIND_CONSTANT_SIZE_BLOCKS

/**
 *   @brief Parallel std::find, constant block size variant.
 *  @param begin1 Begin iterator of first sequence.
 *  @param end1 End iterator of first sequence.
 *  @param begin2 Begin iterator of second sequence. Second sequence
 *  must have same length as first sequence.
 *  @param pred Find predicate.
 *  @param selector Functionality (e. g. std::find_if (), std::equal(),...)
 *  @return Place of finding in both sequences.
 *  @see __gnu_parallel::_Settings::find_sequential_search_size
 *  @see __gnu_parallel::_Settings::find_block_size
 *  There are two main differences between the growing blocks and the
 *  constant-size blocks variants.
 *  1. For GB, the block size grows; for CSB, the block size is fixed.
 *  2. For GB, the blocks are allocated dynamically; for CSB, the
 *  blocks are allocated in a predetermined manner, namely spacial
 *  round-robin.
 */
template<typename RandomAccessIterator1,
	 typename RandomAccessIterator2,
	 typename Pred,
	 typename Selector>
  std::pair<RandomAccessIterator1, RandomAccessIterator2>
  find_template(RandomAccessIterator1 begin1, RandomAccessIterator1 end1,
                RandomAccessIterator2 begin2, Pred pred, Selector selector,
                constant_size_blocks_tag)
  {
    _GLIBCXX_CALL(end1 - begin1)
    typedef std::iterator_traits<RandomAccessIterator1> traits_type;
    typedef typename traits_type::difference_type difference_type;
    typedef typename traits_type::value_type value_type;

    const _Settings& __s = _Settings::get();

    difference_type length = end1 - begin1;

    difference_type sequential_search_size = std::min<difference_type>(
        length, __s.find_sequential_search_size);

    // Try it sequentially first.
    std::pair<RandomAccessIterator1, RandomAccessIterator2> find_seq_result =
      selector.sequential_algorithm(begin1, begin1 + sequential_search_size,
                                    begin2, pred);

    if (find_seq_result.first != (begin1 + sequential_search_size))
      return find_seq_result;

    difference_type result = length;
    omp_lock_t result_lock;
    omp_init_lock(&result_lock);

    // Not within first sequential_search_size elements -> start parallel.

    thread_index_t num_threads = get_max_threads();
#   pragma omp parallel shared(result) num_threads(num_threads)
      {
#       pragma omp single
          num_threads = omp_get_num_threads();

        thread_index_t iam = omp_get_thread_num();
        difference_type block_size = __s.find_initial_block_size;

        // First element of thread's current iteration.
        difference_type iteration_start = sequential_search_size;

        // Where to work (initialization).
        difference_type start = iteration_start + iam * block_size;
        difference_type stop =
            std::min<difference_type>(length, start + block_size);

        std::pair<RandomAccessIterator1, RandomAccessIterator2> local_result;

        while (start < length)
          {
            // Get new value of result.
#           pragma omp flush(result)
            // No chance to find first element.
            if (result < start)
              break;
            local_result = selector.sequential_algorithm(
                begin1 + start, begin1 + stop,
                begin2 + start, pred);
            if (local_result.first != (begin1 + stop))
              {
                omp_set_lock(&result_lock);
                if ((local_result.first - begin1) < result)
                  result = local_result.first - begin1;
                omp_unset_lock(&result_lock);
                // Will not find better value in its interval.
                break;
              }

            iteration_start += num_threads * block_size;

            // Where to work.
            start = iteration_start + iam * block_size;
            stop = std::min<difference_type>(length, start + block_size);
          }
      } //parallel

    omp_destroy_lock(&result_lock);

    // Return iterator on found element.
    return
      std::pair<RandomAccessIterator1, RandomAccessIterator2>(begin1 + result,
							      begin2 + result);
  }
#endif
} // end namespace

#endif
