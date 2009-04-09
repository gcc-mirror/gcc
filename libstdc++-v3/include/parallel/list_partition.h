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

/** @file parallel/list_partition.h
 *  @brief Functionality to split sequence referenced by only input
 *  iterators.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Leonor Frias Moya and Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_LIST_PARTITION_H
#define _GLIBCXX_PARALLEL_LIST_PARTITION_H 1

#include <parallel/parallel.h>
#include <vector>

namespace __gnu_parallel
{
  /** @brief Shrinks and doubles the ranges.
   *  @param os_starts Start positions worked on (oversampled).
   *  @param count_to_two Counts up to 2.
   *  @param range_length Current length of a chunk.
   *  @param make_twice Whether the @c os_starts is allowed to be
   *  grown or not
   */
  template<typename InputIterator>
    void
    shrink_and_double(std::vector<InputIterator>& os_starts,
		      size_t& count_to_two, size_t& range_length,
		      const bool make_twice)
    {
      ++count_to_two;
      if (not make_twice or count_to_two < 2)
	shrink(os_starts, count_to_two, range_length);
      else
	{
	  os_starts.resize((os_starts.size() - 1) * 2 + 1);
	  count_to_two = 0;
	}
    }

  /** @brief Combines two ranges into one and thus halves the number of ranges.
   *  @param os_starts Start positions worked on (oversampled).
   *  @param count_to_two Counts up to 2.
   *  @param range_length Current length of a chunk. */
  template<typename InputIterator>
    void
    shrink(std::vector<InputIterator>& os_starts, size_t& count_to_two,
	   size_t& range_length)
    {
      for (typename std::vector<InputIterator>::size_type i = 0;
	   i <= (os_starts.size() / 2); ++i)
	os_starts[i] = os_starts[i * 2];
      range_length *= 2;
    }

  /** @brief Splits a sequence given by input iterators into parts of
   * almost equal size
   *
   *  The function needs only one pass over the sequence.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param starts Start iterators for the resulting parts, dimension
   *  @c num_parts+1. For convenience, @c starts @c [num_parts]
   *  contains the end iterator of the sequence.
   *  @param lengths Length of the resulting parts.
   *  @param num_parts Number of parts to split the sequence into.
   *  @param f Functor to be applied to each element by traversing it
   *  @param oversampling Oversampling factor. If 0, then the
   *  partitions will differ in at most @f$ \sqrt{\mathrm{end} -
   *  \mathrm{begin}} @f$ elements. Otherwise, the ratio between the
   *  longest and the shortest part is bounded by @f$
   *  1/(\mathrm{oversampling} \cdot \mathrm{num\_parts}) @f$.
   *  @return Length of the whole sequence.
   */
  template<typename InputIterator, typename FunctorType>
    size_t
    list_partition(const InputIterator begin, const InputIterator end,
		   InputIterator* starts, size_t* lengths, const int num_parts,
		   FunctorType& f, int oversampling = 0)
    {
      bool make_twice = false;

      // The resizing algorithm is chosen according to the oversampling factor.
      if (oversampling == 0)
	{
	  make_twice = true;
	  oversampling = 1;
	}

      std::vector<InputIterator> os_starts(2 * oversampling * num_parts + 1);

      os_starts[0]= begin;
      InputIterator prev = begin, it = begin;
      size_t dist_limit = 0, dist = 0;
      size_t cur = 1, next = 1;
      size_t range_length = 1;
      size_t count_to_two = 0;
      while (it != end)
	{
	  cur = next;
	  for (; cur < os_starts.size() and it != end; ++cur)
	    {
	      for (dist_limit += range_length;
		   dist < dist_limit and it != end; ++dist)
		{
		  f(it);
		  ++it;
		}
	      os_starts[cur] = it;
	    }

	  // Must compare for end and not cur < os_starts.size() , because
	  // cur could be == os_starts.size() as well
	  if (it == end)
	    break;

	  shrink_and_double(os_starts, count_to_two, range_length, make_twice);
	  next = os_starts.size() / 2 + 1;
	}

      // Calculation of the parts (one must be extracted from current
      // because the partition beginning at end, consists only of
      // itself).
      size_t size_part = (cur - 1) / num_parts;
      int size_greater = static_cast<int>((cur - 1) % num_parts);
      starts[0] = os_starts[0];

      size_t index = 0;

      // Smallest partitions.
      for (int i = 1; i < (num_parts + 1 - size_greater); ++i)
	{
	  lengths[i - 1] =  size_part * range_length;
	  index += size_part;
	  starts[i] = os_starts[index];
	}

      // Biggest partitions.
      for (int i = num_parts + 1 - size_greater; i <= num_parts; ++i)
	{
	  lengths[i - 1] =  (size_part+1) * range_length;
	  index += (size_part+1);
	  starts[i] = os_starts[index];
	}

      // Correction of the end size (the end iteration has not finished).
      lengths[num_parts - 1] -= (dist_limit - dist);

      return dist;
    }
}

#endif /* _GLIBCXX_PARALLEL_LIST_PARTITION_H */
