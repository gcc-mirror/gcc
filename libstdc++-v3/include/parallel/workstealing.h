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

/** @file parallel/workstealing.h
 *  @brief Parallelization of embarrassingly parallel execution by
 *  means of work-stealing.
 *
 *  Work stealing is described in
 *
 *  R. D. Blumofe and C. E. Leiserson.
 *  Scheduling multithreaded computations by work stealing.
 *  Journal of the ACM, 46(5):720–748, 1999.
 *
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze.

#ifndef _GLIBCXX_PARALLEL_WORKSTEALING_H
#define _GLIBCXX_PARALLEL_WORKSTEALING_H 1

#include <parallel/parallel.h>
#include <parallel/random_number.h>
#include <parallel/compatibility.h>

namespace __gnu_parallel
{

#define _GLIBCXX_JOB_VOLATILE volatile

/** @brief One job for a certain thread. */
template<typename _DifferenceTp>
  struct Job
  {
    typedef _DifferenceTp difference_type;

    /** @brief First element.
     *
     *  Changed by owning and stealing thread. By stealing thread,
     *  always incremented. */
    _GLIBCXX_JOB_VOLATILE difference_type first;

    /** @brief Last element.
     *
     *  Changed by owning thread only. */
    _GLIBCXX_JOB_VOLATILE difference_type last;

    /** @brief Number of elements, i. e. @c last-first+1.
     *
     *  Changed by owning thread only. */
    _GLIBCXX_JOB_VOLATILE difference_type load;
  };

/** @brief Work stealing algorithm for random access iterators.
  *
  *  Uses O(1) additional memory. Synchronization at job lists is
  *  done with atomic operations.
  *  @param begin Begin iterator of element sequence.
  *  @param end End iterator of element sequence.
  *  @param op User-supplied functor (comparator, predicate, adding
  *  functor, ...).
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
  for_each_template_random_access_workstealing(RandomAccessIterator begin,
					       RandomAccessIterator end,
					       Op op, Fu& f, Red r,
					       Result base, Result& output,
					       typename std::iterator_traits
					       <RandomAccessIterator>::
					       difference_type bound)
  {
    _GLIBCXX_CALL(end - begin)

    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::difference_type difference_type;
    
    const _Settings& __s = _Settings::get();

    difference_type chunk_size = static_cast<difference_type>(__s.workstealing_chunk_size);

    // How many jobs?
    difference_type length = (bound < 0) ? (end - begin) : bound;

    // To avoid false sharing in a cache line.
    const int stride = __s.cache_line_size * 10 / sizeof(Job<difference_type>) + 1;

    // Total number of threads currently working.
    thread_index_t busy = 0;

    Job<difference_type> *job;

    omp_lock_t output_lock;
    omp_init_lock(&output_lock);

    // Write base value to output.
    output = base;

    // No more threads than jobs, at least one thread.
    thread_index_t num_threads =
        __gnu_parallel::max<thread_index_t>(1,
            __gnu_parallel::min<difference_type>(length, get_max_threads()));

#   pragma omp parallel shared(busy) num_threads(num_threads)
      {

#       pragma omp single
          {
            num_threads = omp_get_num_threads();

            // Create job description array.
            job = new Job<difference_type>[num_threads * stride];
          }

        // Initialization phase.

        // Flags for every thread if it is doing productive work.
        bool iam_working = false;

        // Thread id.
        thread_index_t iam = omp_get_thread_num();

        // This job.
        Job<difference_type>& my_job = job[iam * stride];

        // Random number (for work stealing).
        thread_index_t victim;

        // Local value for reduction.
        Result result = Result();

        // Number of elements to steal in one attempt.
        difference_type steal;

        // Every thread has its own random number generator
        // (modulo num_threads).
        random_number rand_gen(iam, num_threads);

        // This thread is currently working.
#       pragma omp atomic
          ++busy;

        iam_working = true;

        // How many jobs per thread? last thread gets the rest.
        my_job.first =
            static_cast<difference_type>(iam * (length / num_threads));

        my_job.last = (iam == (num_threads - 1)) ?
            (length - 1) : ((iam + 1) * (length / num_threads) - 1);
        my_job.load = my_job.last - my_job.first + 1;

        // Init result with first value (to have a base value for reduction).
        if (my_job.first <= my_job.last)
          {
            // Cannot use volatile variable directly.
            difference_type my_first = my_job.first;
            result = f(op, begin + my_first);
            ++my_job.first;
            --my_job.load;
          }

        RandomAccessIterator current;

#       pragma omp barrier

        // Actual work phase
        // Work on own or stolen start
        while (busy > 0)
          {
            // Work until no productive thread left.
#           pragma omp flush(busy)

            // Thread has own work to do
            while (my_job.first <= my_job.last)
              {
                // fetch-and-add call
                // Reserve current job block (size chunk_size) in my queue.
                difference_type current_job =
                  fetch_and_add<difference_type>(&(my_job.first), chunk_size);

                // Update load, to make the three values consistent,
                // first might have been changed in the meantime
                my_job.load = my_job.last - my_job.first + 1;
                for (difference_type job_counter = 0;
                     job_counter < chunk_size && current_job <= my_job.last;
                     ++job_counter)
                  {
                    // Yes: process it!
                    current = begin + current_job;
                    ++current_job;

                    // Do actual work.
                    result = r(result, f(op, current));
                  }

#               pragma omp flush(busy)
              }

            // After reaching this point, a thread's job list is empty.
            if (iam_working)
              {
                // This thread no longer has work.
#               pragma omp atomic
                --busy;

                iam_working = false;
              }

            difference_type supposed_first, supposed_last, supposed_load;
            do
              {
                // Find random nonempty deque (not own), do consistency check.
                yield();
#               pragma omp flush(busy)
                victim = rand_gen();
                supposed_first = job[victim * stride].first;
                supposed_last = job[victim * stride].last;
                supposed_load = job[victim * stride].load;
              }
            while (busy > 0
              && ((supposed_load <= 0)
                || ((supposed_first + supposed_load - 1) != supposed_last)));

            if (busy == 0)
              break;

            if (supposed_load > 0)
              {
                // Has work and work to do.
                // Number of elements to steal (at least one).
                steal = (supposed_load < 2) ? 1 : supposed_load / 2;

                // Push victim's start forward.
                difference_type stolen_first =
                    fetch_and_add<difference_type>(
                        &(job[victim * stride].first), steal);
                difference_type stolen_try =
                    stolen_first + steal - difference_type(1);

                my_job.first = stolen_first;
                my_job.last = __gnu_parallel::min(stolen_try, supposed_last);
                my_job.load = my_job.last - my_job.first + 1;

                // Has potential work again.
#               pragma omp atomic
                  ++busy;
                iam_working = true;

#               pragma omp flush(busy)
              }
#           pragma omp flush(busy)
          } // end while busy > 0
            // Add accumulated result to output.
        omp_set_lock(&output_lock);
        output = r(output, result);
        omp_unset_lock(&output_lock);
      }

    delete[] job;

    // Points to last element processed (needed as return value for
    // some algorithms like transform)
    f.finish_iterator = begin + length;

    omp_destroy_lock(&output_lock);

    return op;
  }
} // end namespace

#endif
