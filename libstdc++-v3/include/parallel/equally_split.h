// -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
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

/** @file parallel/equally_split.h
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_EQUALLY_SPLIT_H
#define _GLIBCXX_PARALLEL_EQUALLY_SPLIT_H 1

namespace __gnu_parallel
{
/** @brief Function to split a sequence into parts of almost equal size.
 *
 *  The resulting sequence s of length num_threads+1 contains the splitting
 *  positions when splitting the range [0,n) into parts of almost
 *  equal size (plus minus 1).  The first entry is 0, the last one
 *  n. There may result empty parts.
 *  @param n Number of elements
 *  @param num_threads Number of parts
 *  @param s Splitters
 *  @returns End of splitter sequence, i. e. @c s+num_threads+1 */
template<typename difference_type, typename OutputIterator>
  OutputIterator
  equally_split(difference_type n, thread_index_t num_threads, OutputIterator s)
  {
    difference_type chunk_length = n / num_threads;
    difference_type num_longer_chunks = n % num_threads;
    difference_type pos = 0;
    for (thread_index_t i = 0; i < num_threads; ++i)
      {
        *s++ = pos;
        pos += (i < num_longer_chunks) ? (chunk_length + 1) : chunk_length;
      }
    *s++ = n;
    return s;
  }


/** @brief Function to split a sequence into parts of almost equal size.
 *
 *  Returns the position of the splitting point between
 *  thread number thread_no (included) and
 *  thread number thread_no+1 (excluded).
 *  @param n Number of elements
 *  @param num_threads Number of parts
 *  @returns _SplittingAlgorithm point */
template<typename difference_type>
  difference_type
  equally_split_point(difference_type n,
                      thread_index_t num_threads,
                      thread_index_t thread_no)
  {
    difference_type chunk_length = n / num_threads;
    difference_type num_longer_chunks = n % num_threads;
    if (thread_no < num_longer_chunks)
      return thread_no * (chunk_length + 1);
    else
      return num_longer_chunks * (chunk_length + 1)
          + (thread_no - num_longer_chunks) * chunk_length;
  }
}

#endif
