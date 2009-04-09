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

/** @file parallel/partition.h
 *  @brief Parallel implementation of std::partition(),
 *  std::nth_element(), and std::partial_sort().
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler and Felix Putze.

#ifndef _GLIBCXX_PARALLEL_PARTITION_H
#define _GLIBCXX_PARALLEL_PARTITION_H 1

#include <parallel/basic_iterator.h>
#include <parallel/sort.h>
#include <parallel/random_number.h>
#include <bits/stl_algo.h>
#include <parallel/parallel.h>

/** @brief Decide whether to declare certain variables volatile. */
#define _GLIBCXX_VOLATILE volatile

namespace __gnu_parallel
{
/** @brief Parallel implementation of std::partition.
  *  @param begin Begin iterator of input sequence to split.
  *  @param end End iterator of input sequence to split.
  *  @param pred Partition predicate, possibly including some kind of pivot.
  *  @param num_threads Maximum number of threads to use for this task.
  *  @return Number of elements not fulfilling the predicate. */
template<typename RandomAccessIterator, typename Predicate>
  typename std::iterator_traits<RandomAccessIterator>::difference_type
  parallel_partition(RandomAccessIterator begin, RandomAccessIterator end,
                     Predicate pred, thread_index_t num_threads)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    difference_type n = end - begin;

    _GLIBCXX_CALL(n)

    const _Settings& __s = _Settings::get();

    // Shared.
    _GLIBCXX_VOLATILE difference_type left = 0, right = n - 1;
    _GLIBCXX_VOLATILE difference_type leftover_left, leftover_right;
    _GLIBCXX_VOLATILE difference_type leftnew, rightnew;

    bool* reserved_left = NULL, * reserved_right = NULL;

    difference_type chunk_size;

    omp_lock_t result_lock;
    omp_init_lock(&result_lock);

    //at least two chunks per thread
    if(right - left + 1 >= 2 * num_threads * chunk_size)
#   pragma omp parallel num_threads(num_threads)
      {
#       pragma omp single
          {
            num_threads = omp_get_num_threads();
            reserved_left = new bool[num_threads];
            reserved_right = new bool[num_threads];

            if (__s.partition_chunk_share > 0.0)
              chunk_size = std::max<difference_type>(__s.partition_chunk_size,
				    (double)n * __s.partition_chunk_share
						     / (double)num_threads);
            else
              chunk_size = __s.partition_chunk_size;
          }

        while (right - left + 1 >= 2 * num_threads * chunk_size)
          {
#           pragma omp single
              {
                difference_type num_chunks = (right - left + 1) / chunk_size;

                for (int r = 0; r < num_threads; ++r)
                  {
                    reserved_left[r] = false;
                    reserved_right[r] = false;
                  }
                leftover_left = 0;
                leftover_right = 0;
              } //implicit barrier

            // Private.
            difference_type thread_left, thread_left_border,
                            thread_right, thread_right_border;
            thread_left = left + 1;

            // Just to satisfy the condition below.
            thread_left_border = thread_left - 1;
            thread_right = n - 1;
            thread_right_border = thread_right + 1;

            bool iam_finished = false;
            while (!iam_finished)
              {
                if (thread_left > thread_left_border)
                  {
                    omp_set_lock(&result_lock);
                    if (left + (chunk_size - 1) > right)
                      iam_finished = true;
                    else
                      {
                        thread_left = left;
                        thread_left_border = left + (chunk_size - 1);
                        left += chunk_size;
                      }
                    omp_unset_lock(&result_lock);
                  }

                if (thread_right < thread_right_border)
                  {
                    omp_set_lock(&result_lock);
                    if (left > right - (chunk_size - 1))
                      iam_finished = true;
                    else
                      {
                        thread_right = right;
                        thread_right_border = right - (chunk_size - 1);
                        right -= chunk_size;
                      }
                    omp_unset_lock(&result_lock);
                  }

                if (iam_finished)
                  break;

                // Swap as usual.
                while (thread_left < thread_right)
                  {
                    while (pred(begin[thread_left])
                            && thread_left <= thread_left_border)
                      ++thread_left;
                    while (!pred(begin[thread_right])
                            && thread_right >= thread_right_border)
                      --thread_right;

                    if (thread_left > thread_left_border
                        || thread_right < thread_right_border)
                      // Fetch new chunk(s).
                      break;

                    std::swap(begin[thread_left], begin[thread_right]);
                    ++thread_left;
                    --thread_right;
                  }
              }

            // Now swap the leftover chunks to the right places.
            if (thread_left <= thread_left_border)
#             pragma omp atomic
              ++leftover_left;
            if (thread_right >= thread_right_border)
#             pragma omp atomic
              ++leftover_right;

#           pragma omp barrier

#           pragma omp single
              {
                leftnew = left - leftover_left * chunk_size;
                rightnew = right + leftover_right * chunk_size;
              }

#           pragma omp barrier

            // <=> thread_left_border + (chunk_size - 1) >= leftnew
            if (thread_left <= thread_left_border
                && thread_left_border >= leftnew)
              {
                // Chunk already in place, reserve spot.
                reserved_left[(left - (thread_left_border + 1)) / chunk_size]
                    = true;
              }

            // <=> thread_right_border - (chunk_size - 1) <= rightnew
            if (thread_right >= thread_right_border
                && thread_right_border <= rightnew)
              {
                // Chunk already in place, reserve spot.
                reserved_right[((thread_right_border - 1) - right)
			       / chunk_size] = true;
              }

#           pragma omp barrier

            if (thread_left <= thread_left_border
                && thread_left_border < leftnew)
              {
                // Find spot and swap.
                difference_type swapstart = -1;
                omp_set_lock(&result_lock);
                for (int r = 0; r < leftover_left; ++r)
                  if (!reserved_left[r])
                    {
                      reserved_left[r] = true;
                      swapstart = left - (r + 1) * chunk_size;
                      break;
                    }
                omp_unset_lock(&result_lock);

#if _GLIBCXX_ASSERTIONS
                _GLIBCXX_PARALLEL_ASSERT(swapstart != -1);
#endif

                std::swap_ranges(begin + thread_left_border
				 - (chunk_size - 1),
				 begin + thread_left_border + 1,
				 begin + swapstart);
              }

            if (thread_right >= thread_right_border
                && thread_right_border > rightnew)
              {
                // Find spot and swap
                difference_type swapstart = -1;
                omp_set_lock(&result_lock);
                for (int r = 0; r < leftover_right; ++r)
                  if (!reserved_right[r])
                    {
                      reserved_right[r] = true;
                      swapstart = right + r * chunk_size + 1;
                      break;
                    }
                omp_unset_lock(&result_lock);

#if _GLIBCXX_ASSERTIONS
                _GLIBCXX_PARALLEL_ASSERT(swapstart != -1);
#endif

                std::swap_ranges(begin + thread_right_border,
				 begin + thread_right_border + chunk_size,
				 begin + swapstart);
              }
#if _GLIBCXX_ASSERTIONS
#             pragma omp barrier

#             pragma omp single
                {
                  for (int r = 0; r < leftover_left; ++r)
                    _GLIBCXX_PARALLEL_ASSERT(reserved_left[r]);
                  for (int r = 0; r < leftover_right; ++r)
                    _GLIBCXX_PARALLEL_ASSERT(reserved_right[r]);
                }

#             pragma omp barrier
#endif

#             pragma omp barrier

              left = leftnew;
              right = rightnew;
          }
#         pragma omp flush(left, right)
      } // end "recursion" //parallel

    difference_type final_left = left, final_right = right;

    while (final_left < final_right)
      {
        // Go right until key is geq than pivot.
        while (pred(begin[final_left]) && final_left < final_right)
          ++final_left;

        // Go left until key is less than pivot.
        while (!pred(begin[final_right]) && final_left < final_right)
          --final_right;

        if (final_left == final_right)
          break;
        std::swap(begin[final_left], begin[final_right]);
        ++final_left;
        --final_right;
      }

    // All elements on the left side are < piv, all elements on the
    // right are >= piv
    delete[] reserved_left;
    delete[] reserved_right;

    omp_destroy_lock(&result_lock);

    // Element "between" final_left and final_right might not have
    // been regarded yet
    if (final_left < n && !pred(begin[final_left]))
      // Really swapped.
      return final_left;
    else
      return final_left + 1;
  }

/**
  *  @brief Parallel implementation of std::nth_element().
  *  @param begin Begin iterator of input sequence.
  *  @param nth Iterator of element that must be in position afterwards.
  *  @param end End iterator of input sequence.
  *  @param comp Comparator.
  */
template<typename RandomAccessIterator, typename Comparator>
  void 
  parallel_nth_element(RandomAccessIterator begin, RandomAccessIterator nth, 
		       RandomAccessIterator end, Comparator comp)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    _GLIBCXX_CALL(end - begin)

    RandomAccessIterator split;
    random_number rng;

    difference_type minimum_length =
      std::max<difference_type>(2, _Settings::get().partition_minimal_n);

    // Break if input range to small.
    while (static_cast<sequence_index_t>(end - begin) >= minimum_length)
      {
        difference_type n = end - begin;

        RandomAccessIterator pivot_pos = begin +  rng(n);

        // Swap pivot_pos value to end.
        if (pivot_pos != (end - 1))
          std::swap(*pivot_pos, *(end - 1));
        pivot_pos = end - 1;

        // XXX Comparator must have first_value_type, second_value_type,
	// result_type
        // Comparator == __gnu_parallel::lexicographic<S, int,
	// __gnu_parallel::less<S, S> >
        // pivot_pos == std::pair<S, int>*
        // XXX binder2nd only for RandomAccessIterators??
        __gnu_parallel::binder2nd<Comparator, value_type, value_type, bool>
	  pred(comp, *pivot_pos);

        // Divide, leave pivot unchanged in last place.
        RandomAccessIterator split_pos1, split_pos2;
        split_pos1 = begin + parallel_partition(begin, end - 1, pred,
						get_max_threads());

        // Left side: < pivot_pos; right side: >= pivot_pos

        // Swap pivot back to middle.
        if (split_pos1 != pivot_pos)
          std::swap(*split_pos1, *pivot_pos);
        pivot_pos = split_pos1;

        // In case all elements are equal, split_pos1 == 0
        if ((split_pos1 + 1 - begin) < (n >> 7)
	    || (end - split_pos1) < (n >> 7))
          {
            // Very unequal split, one part smaller than one 128th
            // elements not strictly larger than the pivot.
            __gnu_parallel::unary_negate<__gnu_parallel::
	      binder1st<Comparator, value_type, value_type, bool>, value_type>
	      pred(__gnu_parallel::binder1st<Comparator, value_type,
		   value_type, bool>(comp, *pivot_pos));

            // Find other end of pivot-equal range.
            split_pos2 = __gnu_sequential::partition(split_pos1 + 1,
						     end, pred);
          }
        else
          // Only skip the pivot.
          split_pos2 = split_pos1 + 1;

        // Compare iterators.
        if (split_pos2 <= nth)
          begin = split_pos2;
        else if (nth < split_pos1)
          end = split_pos1;
        else
          break;
      }

    // Only at most _Settings::partition_minimal_n elements left.
    __gnu_sequential::sort(begin, end, comp);
  }

/** @brief Parallel implementation of std::partial_sort().
*  @param begin Begin iterator of input sequence.
*  @param middle Sort until this position.
*  @param end End iterator of input sequence.
*  @param comp Comparator. */
template<typename RandomAccessIterator, typename Comparator>
  void
  parallel_partial_sort(RandomAccessIterator begin,
			RandomAccessIterator middle,
			RandomAccessIterator end, Comparator comp)
  {
    parallel_nth_element(begin, middle, end, comp);
    std::sort(begin, middle, comp);
  }

} //namespace __gnu_parallel

#undef _GLIBCXX_VOLATILE

#endif /* _GLIBCXX_PARALLEL_PARTITION_H */
