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

/** @file parallel/quicksort.h
 *  @brief Implementation of a unbalanced parallel quicksort (in-place).
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_QUICKSORT_H
#define _GLIBCXX_PARALLEL_QUICKSORT_H 1

#include <parallel/parallel.h>
#include <parallel/partition.h>

namespace __gnu_parallel
{
  /** @brief Unbalanced quicksort divide step.
   *  @param begin Begin iterator of subsequence.
   *  @param end End iterator of subsequence.
   *  @param comp Comparator.
   *  @param pivot_rank Desired rank of the pivot.
   *  @param num_samples Choose pivot from that many samples.
   *  @param num_threads Number of threads that are allowed to work on
   *  this part.
   */
  template<typename RandomAccessIterator, typename Comparator>
    typename std::iterator_traits<RandomAccessIterator>::difference_type
    parallel_sort_qs_divide(RandomAccessIterator begin,
			    RandomAccessIterator end,
			    Comparator comp, typename std::iterator_traits
			    <RandomAccessIterator>::difference_type pivot_rank,
			    typename std::iterator_traits
			    <RandomAccessIterator>::difference_type
			    num_samples, thread_index_t num_threads)
    {
      typedef std::iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;

      difference_type n = end - begin;
      num_samples = std::min(num_samples, n);

      // Allocate uninitialized, to avoid default constructor.
      value_type* samples =
	static_cast<value_type*>(::operator new(num_samples
						* sizeof(value_type)));

      for (difference_type s = 0; s < num_samples; ++s)
	{
	  const unsigned long long index = static_cast<unsigned long long>(s)
	    * n / num_samples;
	  ::new(&(samples[s])) value_type(begin[index]);
	}

      __gnu_sequential::sort(samples, samples + num_samples, comp);

      value_type& pivot = samples[pivot_rank * num_samples / n];

      __gnu_parallel::binder2nd<Comparator, value_type, value_type, bool>
        pred(comp, pivot);
      difference_type split =
          parallel_partition(begin, end, pred, num_threads);

      ::operator delete(samples);

      return split;
    }

  /** @brief Unbalanced quicksort conquer step.
   *  @param begin Begin iterator of subsequence.
   *  @param end End iterator of subsequence.
   *  @param comp Comparator.
   *  @param num_threads Number of threads that are allowed to work on
   *  this part.
   */
  template<typename RandomAccessIterator, typename Comparator>
    void
    parallel_sort_qs_conquer(RandomAccessIterator begin,
			     RandomAccessIterator end,
			     Comparator comp,
			     thread_index_t num_threads)
    {
      typedef std::iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;

      if (num_threads <= 1)
	{
	  __gnu_sequential::sort(begin, end, comp);
	  return;
	}

      difference_type n = end - begin, pivot_rank;

      if (n <= 1)
	return;

      thread_index_t num_threads_left;

      if ((num_threads % 2) == 1)
	num_threads_left = num_threads / 2 + 1;
      else
	num_threads_left = num_threads / 2;

      pivot_rank = n * num_threads_left / num_threads;

      difference_type split =
	parallel_sort_qs_divide(begin, end, comp, pivot_rank,
				_Settings::get().sort_qs_num_samples_preset,
				num_threads);

#pragma omp parallel sections num_threads(2)
      {
#pragma omp section
	parallel_sort_qs_conquer(begin, begin + split,
				 comp, num_threads_left);
#pragma omp section
	parallel_sort_qs_conquer(begin + split, end,
				 comp, num_threads - num_threads_left);
      }
    }



  /** @brief Unbalanced quicksort main call.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator input sequence, ignored.
   *  @param comp Comparator.
   *  @param num_threads Number of threads that are allowed to work on
   *  this part.
   */
  template<typename RandomAccessIterator, typename Comparator>
    void
    parallel_sort_qs(RandomAccessIterator begin,
		     RandomAccessIterator end,
		     Comparator comp,
		     thread_index_t num_threads)
    {
      _GLIBCXX_CALL(n)

      typedef std::iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;

      difference_type n = end - begin;

      // At least one element per processor.
      if (num_threads > n)
        num_threads = static_cast<thread_index_t>(n);

      parallel_sort_qs_conquer(begin, begin + n, comp, num_threads);
    }

} //namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_QUICKSORT_H */
