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

/** @file parallel/partial_sum.h
 *  @brief Parallel implementation of std::partial_sum(), i. e. prefix
 *  sums.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_PARTIAL_SUM_H
#define _GLIBCXX_PARALLEL_PARTIAL_SUM_H 1

#include <omp.h>
#include <new>
#include <bits/stl_algobase.h>
#include <parallel/parallel.h>
#include <parallel/numericfwd.h>

namespace __gnu_parallel
{
  // Problem: there is no 0-element given.

/** @brief Base case prefix sum routine.
  *  @param begin Begin iterator of input sequence.
  *  @param end End iterator of input sequence.
  *  @param result Begin iterator of output sequence.
  *  @param bin_op Associative binary function.
  *  @param value Start value. Must be passed since the neutral
  *  element is unknown in general.
  *  @return End iterator of output sequence. */
template<typename InputIterator,
	 typename OutputIterator,
	 typename BinaryOperation>
  OutputIterator
  parallel_partial_sum_basecase(InputIterator begin, InputIterator end,
				OutputIterator result, BinaryOperation bin_op,
				typename std::iterator_traits
				<InputIterator>::value_type value)
  {
    if (begin == end)
      return result;

    while (begin != end)
      {
        value = bin_op(value, *begin);
        *result = value;
        ++result;
        ++begin;
      }
    return result;
  }

/** @brief Parallel partial sum implementation, two-phase approach,
    no recursion.
    *  @param begin Begin iterator of input sequence.
    *  @param end End iterator of input sequence.
    *  @param result Begin iterator of output sequence.
    *  @param bin_op Associative binary function.
    *  @param n Length of sequence.
    *  @param num_threads Number of threads to use.
    *  @return End iterator of output sequence.
    */
template<typename InputIterator,
	 typename OutputIterator,
	 typename BinaryOperation>
  OutputIterator
  parallel_partial_sum_linear(InputIterator begin, InputIterator end,
			      OutputIterator result, BinaryOperation bin_op,
			      typename std::iterator_traits
			      <InputIterator>::difference_type n)
  {
    typedef std::iterator_traits<InputIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    if (begin == end)
      return result;

    thread_index_t num_threads =
        std::min<difference_type>(get_max_threads(), n - 1);

    if (num_threads < 2)
      {
        *result = *begin;
        return parallel_partial_sum_basecase(
            begin + 1, end, result + 1, bin_op, *begin);
      }

    difference_type* borders;
    value_type* sums;

    const _Settings& __s = _Settings::get();

#   pragma omp parallel num_threads(num_threads)
      {
#       pragma omp single
          {
            num_threads = omp_get_num_threads();

            borders = new difference_type[num_threads + 2];

            if (__s.partial_sum_dilation == 1.0f)
              equally_split(n, num_threads + 1, borders);
            else
              {
                difference_type chunk_length =
                    ((double)n
		     / ((double)num_threads + __s.partial_sum_dilation)),
		  borderstart = n - num_threads * chunk_length;
                borders[0] = 0;
                for (int i = 1; i < (num_threads + 1); ++i)
                  {
                    borders[i] = borderstart;
                    borderstart += chunk_length;
                  }
                borders[num_threads + 1] = n;
              }

            sums = static_cast<value_type*>(::operator new(sizeof(value_type)
							   * num_threads));
            OutputIterator target_end;
          } //single

        thread_index_t iam = omp_get_thread_num();
        if (iam == 0)
          {
            *result = *begin;
            parallel_partial_sum_basecase(begin + 1, begin + borders[1],
					  result + 1, bin_op, *begin);
            ::new(&(sums[iam])) value_type(*(result + borders[1] - 1));
          }
        else
          {
            ::new(&(sums[iam]))
	      value_type(std::accumulate(begin + borders[iam] + 1,
					 begin + borders[iam + 1],
					 *(begin + borders[iam]),
					 bin_op,
					 __gnu_parallel::sequential_tag()));
          }

#       pragma omp barrier

#       pragma omp single
          parallel_partial_sum_basecase(
              sums + 1, sums + num_threads, sums + 1, bin_op, sums[0]);

#       pragma omp barrier

        // Still same team.
        parallel_partial_sum_basecase(begin + borders[iam + 1],
				      begin + borders[iam + 2],
				      result + borders[iam + 1], bin_op,
				      sums[iam]);
      } //parallel

    ::operator delete(sums);
    delete[] borders;

    return result + n;
  }

/** @brief Parallel partial sum front-end.
  *  @param begin Begin iterator of input sequence.
  *  @param end End iterator of input sequence.
  *  @param result Begin iterator of output sequence.
  *  @param bin_op Associative binary function.
  *  @return End iterator of output sequence. */
template<typename InputIterator,
	 typename OutputIterator,
	 typename BinaryOperation>
  OutputIterator
  parallel_partial_sum(InputIterator begin, InputIterator end,
                       OutputIterator result, BinaryOperation bin_op)
  {
    _GLIBCXX_CALL(begin - end)

    typedef std::iterator_traits<InputIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    difference_type n = end - begin;

    switch (_Settings::get().partial_sum_algorithm)
      {
      case LINEAR:
        // Need an initial offset.
        return parallel_partial_sum_linear(begin, end, result, bin_op, n);
      default:
    // Partial_sum algorithm not implemented.
        _GLIBCXX_PARALLEL_ASSERT(0);
        return result + n;
      }
  }
}

#endif /* _GLIBCXX_PARALLEL_PARTIAL_SUM_H */
