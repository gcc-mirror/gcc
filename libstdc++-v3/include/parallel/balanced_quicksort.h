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

/** @file parallel/balanced_quicksort.h
 *  @brief Implementation of a dynamically load-balanced parallel quicksort.
 *
 *  It works in-place and needs only logarithmic extra memory.
 *  The algorithm is similar to the one proposed in
 *
 *  P. Tsigas and Y. Zhang.
 *  A simple, fast parallel implementation of quicksort and
 *  its performance evaluation on SUN enterprise 10000.
 *  In 11th Euromicro Conference on Parallel, Distributed and
 *  Network-Based Processing, page 372, 2003.
 *
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_BALANCED_QUICKSORT_H
#define _GLIBCXX_PARALLEL_BALANCED_QUICKSORT_H 1

#include <parallel/basic_iterator.h>
#include <bits/stl_algo.h>

#include <parallel/settings.h>
#include <parallel/partition.h>
#include <parallel/random_number.h>
#include <parallel/queue.h>
#include <functional>

#if _GLIBCXX_ASSERTIONS
#include <parallel/checkers.h>
#endif

namespace __gnu_parallel
{
/** @brief Information local to one thread in the parallel quicksort run. */
template<typename RandomAccessIterator>
  struct QSBThreadLocal
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::difference_type difference_type;

    /** @brief Continuous part of the sequence, described by an
    iterator pair. */
    typedef std::pair<RandomAccessIterator, RandomAccessIterator> Piece;

    /** @brief Initial piece to work on. */
    Piece initial;

    /** @brief Work-stealing queue. */
    RestrictedBoundedConcurrentQueue<Piece> leftover_parts;

    /** @brief Number of threads involved in this algorithm. */
    thread_index_t num_threads;

    /** @brief Pointer to a counter of elements left over to sort. */
    volatile difference_type* elements_leftover;

    /** @brief The complete sequence to sort. */
    Piece global;

    /** @brief Constructor.
     *  @param queue_size Size of the work-stealing queue. */
    QSBThreadLocal(int queue_size) : leftover_parts(queue_size) { }
  };

/** @brief Balanced quicksort divide step.
  *  @param begin Begin iterator of subsequence.
  *  @param end End iterator of subsequence.
  *  @param comp Comparator.
  *  @param num_threads Number of threads that are allowed to work on
  *  this part.
  *  @pre @c (end-begin)>=1 */
template<typename RandomAccessIterator, typename Comparator>
  typename std::iterator_traits<RandomAccessIterator>::difference_type
  qsb_divide(RandomAccessIterator begin, RandomAccessIterator end,
             Comparator comp, thread_index_t num_threads)
  {
    _GLIBCXX_PARALLEL_ASSERT(num_threads > 0);

    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    RandomAccessIterator pivot_pos =
      median_of_three_iterators(begin, begin + (end - begin) / 2,
				end  - 1, comp);

#if defined(_GLIBCXX_ASSERTIONS)
    // Must be in between somewhere.
    difference_type n = end - begin;

    _GLIBCXX_PARALLEL_ASSERT(
           (!comp(*pivot_pos, *begin) && !comp(*(begin + n / 2), *pivot_pos))
        || (!comp(*pivot_pos, *begin) && !comp(*(end - 1), *pivot_pos))
        || (!comp(*pivot_pos, *(begin + n / 2)) && !comp(*begin, *pivot_pos))
        || (!comp(*pivot_pos, *(begin + n / 2)) && !comp(*(end - 1), *pivot_pos))
        || (!comp(*pivot_pos, *(end - 1)) && !comp(*begin, *pivot_pos))
        || (!comp(*pivot_pos, *(end - 1)) && !comp(*(begin + n / 2), *pivot_pos)));
#endif

    // Swap pivot value to end.
    if (pivot_pos != (end - 1))
      std::swap(*pivot_pos, *(end - 1));
    pivot_pos = end - 1;

    __gnu_parallel::binder2nd<Comparator, value_type, value_type, bool>
        pred(comp, *pivot_pos);

    // Divide, returning end - begin - 1 in the worst case.
    difference_type split_pos = parallel_partition(
        begin, end - 1, pred, num_threads);

    // Swap back pivot to middle.
    std::swap(*(begin + split_pos), *pivot_pos);
    pivot_pos = begin + split_pos;

#if _GLIBCXX_ASSERTIONS
    RandomAccessIterator r;
    for (r = begin; r != pivot_pos; ++r)
      _GLIBCXX_PARALLEL_ASSERT(comp(*r, *pivot_pos));
    for (; r != end; ++r)
      _GLIBCXX_PARALLEL_ASSERT(!comp(*r, *pivot_pos));
#endif

    return split_pos;
  }

/** @brief Quicksort conquer step.
  *  @param tls Array of thread-local storages.
  *  @param begin Begin iterator of subsequence.
  *  @param end End iterator of subsequence.
  *  @param comp Comparator.
  *  @param iam Number of the thread processing this function.
  *  @param num_threads
  *          Number of threads that are allowed to work on this part. */
template<typename RandomAccessIterator, typename Comparator>
  void
  qsb_conquer(QSBThreadLocal<RandomAccessIterator>** tls,
              RandomAccessIterator begin, RandomAccessIterator end,
              Comparator comp,
              thread_index_t iam, thread_index_t num_threads,
              bool parent_wait)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    difference_type n = end - begin;

    if (num_threads <= 1 || n <= 1)
      {
        tls[iam]->initial.first  = begin;
        tls[iam]->initial.second = end;

        qsb_local_sort_with_helping(tls, comp, iam, parent_wait);

        return;
      }

    // Divide step.
    difference_type split_pos = qsb_divide(begin, end, comp, num_threads);

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(0 <= split_pos && split_pos < (end - begin));
#endif

    thread_index_t num_threads_leftside =
        std::max<thread_index_t>(1, std::min<thread_index_t>(
                          num_threads - 1, split_pos * num_threads / n));

#   pragma omp atomic
    *tls[iam]->elements_leftover -= (difference_type)1;

    // Conquer step.
#   pragma omp parallel num_threads(2)
    {
      bool wait;
      if(omp_get_num_threads() < 2)
        wait = false;
      else
        wait = parent_wait;

#     pragma omp sections
        {
#         pragma omp section
            {
              qsb_conquer(tls, begin, begin + split_pos, comp,
                          iam,
                          num_threads_leftside,
                          wait);
              wait = parent_wait;
            }
          // The pivot_pos is left in place, to ensure termination.
#         pragma omp section
            {
              qsb_conquer(tls, begin + split_pos + 1, end, comp,
                          iam + num_threads_leftside,
                          num_threads - num_threads_leftside,
                          wait);
              wait = parent_wait;
            }
        }
    }
  }

/**
  *  @brief Quicksort step doing load-balanced local sort.
  *  @param tls Array of thread-local storages.
  *  @param comp Comparator.
  *  @param iam Number of the thread processing this function.
  */
template<typename RandomAccessIterator, typename Comparator>
  void
  qsb_local_sort_with_helping(QSBThreadLocal<RandomAccessIterator>** tls,
                              Comparator& comp, int iam, bool wait)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;
    typedef std::pair<RandomAccessIterator, RandomAccessIterator> Piece;

    QSBThreadLocal<RandomAccessIterator>& tl = *tls[iam];

    difference_type base_case_n =
        _Settings::get().sort_qsb_base_case_maximal_n;
    if (base_case_n < 2)
      base_case_n = 2;
    thread_index_t num_threads = tl.num_threads;

    // Every thread has its own random number generator.
    random_number rng(iam + 1);

    Piece current = tl.initial;

    difference_type elements_done = 0;
#if _GLIBCXX_ASSERTIONS
    difference_type total_elements_done = 0;
#endif

    for (;;)
      {
        // Invariant: current must be a valid (maybe empty) range.
        RandomAccessIterator begin = current.first, end = current.second;
        difference_type n = end - begin;

        if (n > base_case_n)
          {
            // Divide.
            RandomAccessIterator pivot_pos = begin +  rng(n);

            // Swap pivot_pos value to end.
            if (pivot_pos != (end - 1))
              std::swap(*pivot_pos, *(end - 1));
            pivot_pos = end - 1;

            __gnu_parallel::binder2nd
                <Comparator, value_type, value_type, bool>
                pred(comp, *pivot_pos);

            // Divide, leave pivot unchanged in last place.
            RandomAccessIterator split_pos1, split_pos2;
            split_pos1 = __gnu_sequential::partition(begin, end - 1, pred);

            // Left side: < pivot_pos; right side: >= pivot_pos.
#if _GLIBCXX_ASSERTIONS
            _GLIBCXX_PARALLEL_ASSERT(begin <= split_pos1 && split_pos1 < end);
#endif
            // Swap pivot back to middle.
            if (split_pos1 != pivot_pos)
              std::swap(*split_pos1, *pivot_pos);
            pivot_pos = split_pos1;

            // In case all elements are equal, split_pos1 == 0.
            if ((split_pos1 + 1 - begin) < (n >> 7)
            || (end - split_pos1) < (n >> 7))
              {
                // Very unequal split, one part smaller than one 128th
                // elements not strictly larger than the pivot.
                __gnu_parallel::unary_negate<__gnu_parallel::binder1st
		  <Comparator, value_type, value_type, bool>, value_type>
		  pred(__gnu_parallel::binder1st
		       <Comparator, value_type, value_type, bool>(comp,
								  *pivot_pos));

                // Find other end of pivot-equal range.
                split_pos2 = __gnu_sequential::partition(split_pos1 + 1,
							 end, pred);
              }
            else
              // Only skip the pivot.
              split_pos2 = split_pos1 + 1;

            // Elements equal to pivot are done.
            elements_done += (split_pos2 - split_pos1);
#if _GLIBCXX_ASSERTIONS
            total_elements_done += (split_pos2 - split_pos1);
#endif
            // Always push larger part onto stack.
            if (((split_pos1 + 1) - begin) < (end - (split_pos2)))
              {
                // Right side larger.
                if ((split_pos2) != end)
                  tl.leftover_parts.push_front(std::make_pair(split_pos2,
							      end));

                //current.first = begin;	//already set anyway
                current.second = split_pos1;
                continue;
              }
            else
              {
                // Left side larger.
                if (begin != split_pos1)
                  tl.leftover_parts.push_front(std::make_pair(begin,
							      split_pos1));

                current.first = split_pos2;
                //current.second = end;	//already set anyway
                continue;
              }
          }
        else
          {
            __gnu_sequential::sort(begin, end, comp);
            elements_done += n;
#if _GLIBCXX_ASSERTIONS
            total_elements_done += n;
#endif

            // Prefer own stack, small pieces.
            if (tl.leftover_parts.pop_front(current))
              continue;

#           pragma omp atomic
            *tl.elements_leftover -= elements_done;

            elements_done = 0;

#if _GLIBCXX_ASSERTIONS
            double search_start = omp_get_wtime();
#endif

            // Look for new work.
            bool successfully_stolen = false;
            while (wait && *tl.elements_leftover > 0 && !successfully_stolen
#if _GLIBCXX_ASSERTIONS
              // Possible dead-lock.
              && (omp_get_wtime() < (search_start + 1.0))
#endif
              )
              {
                thread_index_t victim;
                victim = rng(num_threads);

                // Large pieces.
                successfully_stolen = (victim != iam)
                    && tls[victim]->leftover_parts.pop_back(current);
                if (!successfully_stolen)
                  yield();
#if !defined(__ICC) && !defined(__ECC)
#               pragma omp flush
#endif
              }

#if _GLIBCXX_ASSERTIONS
            if (omp_get_wtime() >= (search_start + 1.0))
              {
                sleep(1);
                _GLIBCXX_PARALLEL_ASSERT(omp_get_wtime()
					 < (search_start + 1.0));
              }
#endif
            if (!successfully_stolen)
              {
#if _GLIBCXX_ASSERTIONS
                _GLIBCXX_PARALLEL_ASSERT(*tl.elements_leftover == 0);
#endif
                return;
              }
          }
      }
  }

/** @brief Top-level quicksort routine.
  *  @param begin Begin iterator of sequence.
  *  @param end End iterator of sequence.
  *  @param comp Comparator.
  *  @param num_threads Number of threads that are allowed to work on
  *  this part.
  */
template<typename RandomAccessIterator, typename Comparator>
  void
  parallel_sort_qsb(RandomAccessIterator begin, RandomAccessIterator end,
                    Comparator comp,
                    thread_index_t num_threads)
  {
    _GLIBCXX_CALL(end - begin)

    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;
    typedef std::pair<RandomAccessIterator, RandomAccessIterator> Piece;

    typedef QSBThreadLocal<RandomAccessIterator> tls_type;

    difference_type n = end - begin;

    if (n <= 1)
      return;

    // At least one element per processor.
    if (num_threads > n)
      num_threads = static_cast<thread_index_t>(n);

    // Initialize thread local storage
    tls_type** tls = new tls_type*[num_threads];
    difference_type queue_size = num_threads * (thread_index_t)(log2(n) + 1);
    for (thread_index_t t = 0; t < num_threads; ++t)
      tls[t] = new QSBThreadLocal<RandomAccessIterator>(queue_size);

    // There can never be more than ceil(log2(n)) ranges on the stack, because
    // 1. Only one processor pushes onto the stack
    // 2. The largest range has at most length n
    // 3. Each range is larger than half of the range remaining
    volatile difference_type elements_leftover = n;
    for (int i = 0; i < num_threads; ++i)
      {
        tls[i]->elements_leftover = &elements_leftover;
        tls[i]->num_threads = num_threads;
        tls[i]->global = std::make_pair(begin, end);

        // Just in case nothing is left to assign.
        tls[i]->initial = std::make_pair(end, end);
      }

    // Main recursion call.
    qsb_conquer(tls, begin, begin + n, comp, 0, num_threads, true);

#if _GLIBCXX_ASSERTIONS
    // All stack must be empty.
    Piece dummy;
    for (int i = 1; i < num_threads; ++i)
      _GLIBCXX_PARALLEL_ASSERT(!tls[i]->leftover_parts.pop_back(dummy));
#endif

    for (int i = 0; i < num_threads; ++i)
      delete tls[i];
    delete[] tls;
  }
} // namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_BALANCED_QUICKSORT_H */
