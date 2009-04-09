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

/** @file parallel/omp_loop.h
 *  @brief Parallelization of embarrassingly parallel execution by
 *  means of an OpenMP for loop.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze.

#ifndef _GLIBCXX_PARALLEL_OMP_LOOP_H
#define _GLIBCXX_PARALLEL_OMP_LOOP_H 1

#include <omp.h>

#include <parallel/settings.h>
#include <parallel/basic_iterator.h>
#include <parallel/base.h>

namespace __gnu_parallel
{
/** @brief Embarrassingly parallel algorithm for random access
  * iterators, using an OpenMP for loop.
  *
  *  @param begin Begin iterator of element sequence.
  *  @param end End iterator of element sequence.
  *  @param o User-supplied functor (comparator, predicate, adding
  *  functor, etc.).
  *  @param f Functor to "process" an element with op (depends on
  *  desired functionality, e. g. for std::for_each(), ...).
  *  @param r Functor to "add" a single result to the already
  *  processed elements (depends on functionality).
  *  @param base Base value for reduction.
  *  @param output Pointer to position where final result is written to
  *  @param bound Maximum number of elements processed (e. g. for
  *  std::count_n()).
  *  @return User-supplied functor (that may contain a part of the result).
  */
template<typename RandomAccessIterator,
	 typename Op,
	 typename Fu,
	 typename Red,
	 typename Result>
  Op
  for_each_template_random_access_omp_loop(RandomAccessIterator begin,
					   RandomAccessIterator end,
					   Op o, Fu& f, Red r, Result base,
					   Result& output,
					   typename std::iterator_traits
					   <RandomAccessIterator>::
					   difference_type bound)
  {
    typedef typename
        std::iterator_traits<RandomAccessIterator>::difference_type
        difference_type;

    difference_type length = end - begin;
    thread_index_t num_threads =
      __gnu_parallel::min<difference_type>(get_max_threads(), length);

    Result *thread_results;

#   pragma omp parallel num_threads(num_threads)
      {
#       pragma omp single
          {
            num_threads = omp_get_num_threads();
            thread_results = new Result[num_threads];

            for (thread_index_t i = 0; i < num_threads; ++i)
              thread_results[i] = Result();
          }

        thread_index_t iam = omp_get_thread_num();

#      pragma omp for schedule(dynamic, _Settings::get().workstealing_chunk_size)
        for (difference_type pos = 0; pos < length; ++pos)
          thread_results[iam] =
              r(thread_results[iam], f(o, begin+pos));
      } //parallel

    for (thread_index_t i = 0; i < num_threads; ++i)
        output = r(output, thread_results[i]);

    delete [] thread_results;

    // Points to last element processed (needed as return value for
    // some algorithms like transform).
    f.finish_iterator = begin + length;

    return o;
  }

} // end namespace

#endif /* _GLIBCXX_PARALLEL_OMP_LOOP_H */
