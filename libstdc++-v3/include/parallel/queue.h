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

/** @file parallel/queue.h
 *  @brief Lock-free double-ended queue.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_QUEUE_H
#define _GLIBCXX_PARALLEL_QUEUE_H 1

#include <parallel/types.h>
#include <parallel/base.h>
#include <parallel/compatibility.h>

/** @brief Decide whether to declare certain variable volatile in this file. */
#define _GLIBCXX_VOLATILE volatile

namespace __gnu_parallel
{
  /**@brief Double-ended queue of bounded size, allowing lock-free
   *  atomic access.  push_front() and pop_front() must not be called
   *  concurrently to each other, while pop_back() can be called
   *  concurrently at all times.
   *  @c empty(), @c size(), and @c top() are intentionally not provided.
   *  Calling them would not make sense in a concurrent setting.
   *  @param T Contained element type. */
  template<typename T>
    class RestrictedBoundedConcurrentQueue
    {
    private:
      /** @brief Array of elements, seen as cyclic buffer. */
      T* base;

      /** @brief Maximal number of elements contained at the same time. */
      sequence_index_t max_size;

      /** @brief Cyclic begin and end pointers contained in one
	  atomically changeable value. */
      _GLIBCXX_VOLATILE lcas_t borders;

    public:
      /** @brief Constructor. Not to be called concurrent, of course.
       *  @param max_size Maximal number of elements to be contained. */
      RestrictedBoundedConcurrentQueue(sequence_index_t max_size)
      {
	this->max_size = max_size;
	base = new T[max_size];
	borders = encode2(0, 0);
#pragma omp flush
      }

      /** @brief Destructor. Not to be called concurrent, of course. */
      ~RestrictedBoundedConcurrentQueue()
      { delete[] base; }

      /** @brief Pushes one element into the queue at the front end.
       *  Must not be called concurrently with pop_front(). */
      void
      push_front(const T& t)
      {
	lcas_t former_borders = borders;
	int former_front, former_back;
	decode2(former_borders, former_front, former_back);
	*(base + former_front % max_size) = t;
#if _GLIBCXX_ASSERTIONS
	// Otherwise: front - back > max_size eventually.
	_GLIBCXX_PARALLEL_ASSERT(((former_front + 1) - former_back)
				 <= max_size);
#endif
	fetch_and_add(&borders, encode2(1, 0));
      }

      /** @brief Pops one element from the queue at the front end.
       *  Must not be called concurrently with pop_front(). */
      bool
      pop_front(T& t)
      {
	int former_front, former_back;
#pragma omp flush
	decode2(borders, former_front, former_back);
	while (former_front > former_back)
	  {
	    // Chance.
	    lcas_t former_borders = encode2(former_front, former_back);
	    lcas_t new_borders = encode2(former_front - 1, former_back);
	    if (compare_and_swap(&borders, former_borders, new_borders))
	      {
		t = *(base + (former_front - 1) % max_size);
		return true;
	      }
#pragma omp flush
	    decode2(borders, former_front, former_back);
	  }
	return false;
      }

      /** @brief Pops one element from the queue at the front end.
       *  Must not be called concurrently with pop_front(). */
      bool
      pop_back(T& t)	//queue behavior
      {
	int former_front, former_back;
#pragma omp flush
	decode2(borders, former_front, former_back);
	while (former_front > former_back)
	  {
	    // Chance.
	    lcas_t former_borders = encode2(former_front, former_back);
	    lcas_t new_borders = encode2(former_front, former_back + 1);
	    if (compare_and_swap(&borders, former_borders, new_borders))
	      {
		t = *(base + former_back % max_size);
		return true;
	      }
#pragma omp flush
	    decode2(borders, former_front, former_back);
	  }
	return false;
      }
  };
}	//namespace __gnu_parallel

#undef _GLIBCXX_VOLATILE

#endif
