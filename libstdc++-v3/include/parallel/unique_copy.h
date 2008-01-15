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

/** @file parallel/unique_copy.h
 *  @brief Parallel implementations of std::unique_copy().
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Robert Geisberger and Robin Dapp.

#ifndef _GLIBCXX_PARALLEL_UNIQUE_H
#define _GLIBCXX_PARALLEL_UNIQUE_H 1

#include <parallel/parallel.h>
#include <parallel/multiseq_selection.h>

namespace __gnu_parallel
{

/** @brief Parallel std::unique_copy(), w/o explicit equality predicate.
  *  @param first Begin iterator of input sequence.
  *  @param last End iterator of input sequence.
  *  @param result Begin iterator of result sequence.
  *  @param binary_pred Equality predicate.
  *  @return End iterator of result sequence. */
template<typename InputIterator,
	 class OutputIterator,
	 class BinaryPredicate>
  OutputIterator
  parallel_unique_copy(InputIterator first, InputIterator last,
                       OutputIterator result, BinaryPredicate binary_pred)
  {
    _GLIBCXX_CALL(last - first)

    typedef std::iterator_traits<InputIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    difference_type size = last - first;

    if (size == 0)
      return result;

    // Let the first thread process two parts.
    difference_type *counter;
    difference_type *borders;

    thread_index_t num_threads = get_max_threads();
    // First part contains at least one element.
#   pragma omp parallel num_threads(num_threads)
      {
#       pragma omp single
          {
	    num_threads = omp_get_num_threads();
	    borders = new difference_type[num_threads + 2];
	    equally_split(size, num_threads + 1, borders);
	    counter = new difference_type[num_threads + 1];
          }

        thread_index_t iam = omp_get_thread_num();

        difference_type begin, end;

        // Check for length without duplicates
        // Needed for position in output
        difference_type i = 0;
        OutputIterator out = result;

        if (iam == 0)
        {
          begin = borders[0] + 1;	// == 1
          end = borders[iam + 1];

          ++i;
          *out++ = *first;

          for (InputIterator iter = first + begin; iter < first + end; ++iter)
            {
              if (!binary_pred(*iter, *(iter-1)))
                {
                  ++i;
                  *out++ = *iter;
                }
            }
        }
      else
        {
          begin = borders[iam]; //one part
          end = borders[iam + 1];

          for (InputIterator iter = first + begin; iter < first + end; ++iter)
            {
              if (!binary_pred(*iter, *(iter - 1)))
		++i;
	    }
        }
      counter[iam] = i;

      // Last part still untouched.
      difference_type begin_output;

#     pragma omp barrier

      // Store result in output on calculated positions.
      begin_output = 0;

      if (iam == 0)
        {
          for (int t = 0; t < num_threads; ++t)
            begin_output += counter[t];

          i = 0;

          OutputIterator iter_out = result + begin_output;

          begin = borders[num_threads];
          end = size;

          for (InputIterator iter = first + begin; iter < first + end; ++iter)
            {
              if (iter == first || !binary_pred(*iter, *(iter - 1)))
                {
                  ++i;
                  *iter_out++ = *iter;
                }
            }

          counter[num_threads] = i;
        }
      else
        {
          for (int t = 0; t < iam; t++)
            begin_output += counter[t];

          OutputIterator iter_out = result + begin_output;
          for (InputIterator iter = first + begin; iter < first + end; ++iter)
            {
              if (!binary_pred(*iter, *(iter-1)))
		*iter_out++ = *iter;
	    }
        }
    }

    difference_type end_output = 0;
    for (int t = 0; t < num_threads + 1; t++)
      end_output += counter[t];

    delete[] borders;

    return result + end_output;
  }

/** @brief Parallel std::unique_copy(), without explicit equality predicate
  *  @param first Begin iterator of input sequence.
  *  @param last End iterator of input sequence.
  *  @param result Begin iterator of result sequence.
  *  @return End iterator of result sequence. */
template<typename InputIterator, class OutputIterator>
  inline OutputIterator
  parallel_unique_copy(InputIterator first, InputIterator last,
                       OutputIterator result)
  {
    typedef typename std::iterator_traits<InputIterator>::value_type
      value_type;
    return parallel_unique_copy(first, last, result,
				std::equal_to<value_type>());
  }

}//namespace __gnu_parallel

#endif
